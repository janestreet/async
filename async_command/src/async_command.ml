open Core
open Import
include Core.Command

type 'a with_options = ?behave_nicely_in_pipeline:bool -> ?extract_exn:bool -> 'a

let shutdown_with_error e =
  Stdlib.at_exit (fun () ->
    (* We use [Core] printing rather than [Async] printing, because the program may
       already be shutting down, which could cause the error to be omitted because
       shutdown only waits for flush of the output written before [shutdown] was called.
       We want to make sure the error is seen.  We delay it until [at_exit] to avoid
       interleaving this with flushing of Async writers. *)
    Core.prerr_endline (Error.to_string_hum e));
  shutdown 1
;;

let maybe_print_error_and_shutdown = function
  | Ok () -> shutdown 0
  | Error e -> shutdown_with_error e
;;

module Kind = struct
  type t =
    | Unit of ([ `Scheduler_started ] -> unit Deferred.t)
    | Or_error of ([ `Scheduler_started ] -> unit Or_error.t Deferred.t)
  [@@deriving variants]
end

let recursive_invocation = ref None

(* For command line applications, we want the following behavior: [async-command-exe |
   less] should never truncate the output of the exe.

   Two behaviors in async prevent that from happening:
   1. the at_shutdown handlers in writer.ml that waits for pipes to be flushed at most 5s
   even if the data hasn't made it to the OS
   2. the default 10s timeout before forced shutdown in shutdown.ml

   Here is what we do about it:
   1. Changing writer.ml might be ok, but only for stdout/stderr, not in general: if the
   process has pipes connecting it to other processes it spawned, we most likely don't
   want to block shutdown until they stop (could deadlock if there is a pipe in the
   other direction and the other one process waits for it to die).
   So instead, we wait for stdout and stderr to be flushed with no timeout, or that
   their consumer has left.
   2. We don't force shutdown until stdout/stderr is flushed.
   That wait can be bypassed by redefining [Shutdown.default_force], or passing
   [~force] to [shutdown], which looks like the desired behavior.

   These two behavior changes seem fine for servers as well (where stdout/stderr should
   contain almost nothing, or even be /dev/null), so we make them all the time.
*)
let in_async ?(behave_nicely_in_pipeline = true) ?extract_exn param on_result kind =
  Param.map param ~f:(fun staged_main () ->
    if behave_nicely_in_pipeline then Writer.behave_nicely_in_pipeline ();
    let main = Or_error.try_with (fun () -> unstage (staged_main ())) in
    match !recursive_invocation with
    | Some r -> Set_once.set_exn r [%here] (Or_error.map ~f:kind main)
    | None ->
      (match main with
       | Error e ->
         shutdown_with_error e;
         (never_returns (Scheduler.go ()) : unit)
       | Ok main ->
         let before_shutdown () =
           Deferred.List.iter
             ~how:`Parallel
             Writer.[ force stdout; force stderr ]
             ~f:(fun writer ->
               Deferred.any_unit
                 [ Writer.close_finished writer
                 ; Writer.consumer_left writer
                 ; Writer.flushed writer
                 ])
         in
         Shutdown.at_shutdown before_shutdown;
         Shutdown.set_default_force
           (let prev = Shutdown.default_force () in
            fun () ->
              Deferred.all_unit
                (* The 1s gives a bit of time to the process to stop silently rather than with
                   a "shutdown forced" message and exit 1 if [prev ()] finished first. *)
                [ prev (); (before_shutdown () >>= fun () -> after (sec 1.)) ]);
         upon
           (Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log ?extract_exn (fun () ->
              main `Scheduler_started))
           on_result;
         (never_returns (Scheduler.go ()) : unit)))
;;

type 'r staged = ([ `Scheduler_started ] -> 'r) Staged.t

module Staged = struct
  let async ?behave_nicely_in_pipeline ?extract_exn ~summary ?readme param =
    let on_result = maybe_print_error_and_shutdown in
    basic
      ~summary
      ?readme
      (in_async ?behave_nicely_in_pipeline ?extract_exn param on_result Kind.unit)
  ;;

  let async_spec ?behave_nicely_in_pipeline ?extract_exn ~summary ?readme spec main =
    async
      ?behave_nicely_in_pipeline
      ?extract_exn
      ~summary
      ?readme
      (Spec.to_param spec main)
  ;;

  let async_or_error ?behave_nicely_in_pipeline ?extract_exn ~summary ?readme param =
    let on_result res = maybe_print_error_and_shutdown (Or_error.join res) in
    basic
      ~summary
      ?readme
      (in_async ?behave_nicely_in_pipeline ?extract_exn param on_result Kind.or_error)
  ;;

  let async_spec_or_error
    ?behave_nicely_in_pipeline
    ?extract_exn
    ~summary
    ?readme
    spec
    main
    =
    async_or_error
      ?behave_nicely_in_pipeline
      ?extract_exn
      ~summary
      ?readme
      (Spec.to_param spec main)
  ;;
end

let stage_param = Param.map ~f:(fun main () -> stage (fun `Scheduler_started -> main ()))

let async ?behave_nicely_in_pipeline ?extract_exn ~summary ?readme param =
  Staged.async
    ?behave_nicely_in_pipeline
    ?extract_exn
    ~summary
    ?readme
    (stage_param param)
;;

let async_or_error ?behave_nicely_in_pipeline ?extract_exn ~summary ?readme param =
  Staged.async_or_error
    ?behave_nicely_in_pipeline
    ?extract_exn
    ~summary
    ?readme
    (stage_param param)
;;

let async_spec ?behave_nicely_in_pipeline ?extract_exn ~summary ?readme spec main =
  async ?behave_nicely_in_pipeline ?extract_exn ~summary ?readme (Spec.to_param spec main)
;;

let async_spec_or_error ?behave_nicely_in_pipeline ?extract_exn ~summary ?readme spec main
  =
  async_or_error
    ?behave_nicely_in_pipeline
    ?extract_exn
    ~summary
    ?readme
    (Spec.to_param spec main)
;;

module For_testing = struct
  let run_from_within_async ~argv cmd =
    let r = Set_once.create () in
    match !recursive_invocation with
    | Some _ ->
      raise_s [%message "cannot nest recursive command invocations during parsing"]
    | None ->
      recursive_invocation := Some r;
      (match
         Or_error.try_with (fun () ->
           Exn.protect
             ~finally:(fun () -> recursive_invocation := None)
             ~f:(fun () -> Command_unix.run ~argv cmd))
       with
       | Error _ as e -> return e
       | Ok () ->
         (match Set_once.get r with
          | None -> return (Ok ())
          | Some (Error _ as e) -> return e
          | Some (Ok kind) ->
            Monitor.try_with_join_or_error (fun () ->
              match kind with
              | Unit thunk ->
                let%map.Deferred () = thunk `Scheduler_started in
                Ok ()
              | Or_error thunk -> thunk `Scheduler_started)))
  ;;
end
