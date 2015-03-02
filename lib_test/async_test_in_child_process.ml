open Core.Std
open Async.Std

(* Turn off [ASYNC_CONFIG], so it isn't inherited by child processes. *)
let () = Unix.unsetenv "ASYNC_CONFIG"

module Expect = struct
  type t = (string * (Process.Output.t -> unit Or_error.t Deferred.t)) Blang.t
  with sexp_of

  type t1 = (string * unit Or_error.t Deferred.t) Blang.t
  with sexp_of

  let custom string f : t = Blang.base (string, fun _ -> f ())

  let eval (t : t) output =
    let t1 = Blang.map t ~f:(fun (expect, check) -> (expect, check output)) in
    Deferred.map (Deferred.all (List.map (Blang.values t1) ~f:snd)) ~f:(fun _ ->
      if Blang.eval t1 (fun (_, d) ->
        match Deferred.peek d with
        | None -> assert false
        | Some r -> Result.is_ok r)
      then Ok ()
      else Or_error.error "unexpected output" t1 <:sexp_of< t1 >>)
  ;;

  let bool expected f : t =
    Blang.base (expected, fun output ->
      return (if f output then Ok () else Or_error.error_string expected))

  let status expected f : t =
    bool ("exit status " ^ expected)
      (fun { Process.Output.exit_status; _ } -> f exit_status)
  ;;

  let ok = status "zero" Result.is_ok

  let error = status "nonzero" Result.is_error

  let no_output =
    bool "no output"
      (fun { Process.Output. stdout; stderr; _ } -> stdout = "" && stderr = "")
  ;;

  let ( || ) t1 t2 = Blang.or_ [ t1; t2 ]
  let ( && ) t1 t2 = Blang.and_ [ t1; t2 ]
  let not t = Blang.not_ t
end

let test_parent_subcommand = "test-parent"
let test_child_subcommand = "test-child"

module Test = struct
  type t =
    { id : int;
      source_code_position : Source_code_position.t;
      expect : Expect.t;
      run : unit -> unit Deferred.t;
    }
  with sexp_of

  let id_counter = ref 0

  let all = ref []

  let add source_code_position expect run =
    incr id_counter;
    all := { id = !id_counter;
             source_code_position;
             expect;
             run;
           } :: !all;
  ;;

  let get_id id =
    match List.find !all ~f:(fun t -> id = t.id) with
    | Some t -> t
    | None -> failwiths "no test with id" id <:sexp_of< int >>
  ;;

  let run_child id =
    let t = get_id id in
    t.run ()
  ;;

  let run_parent id =
    let t = get_id id in
    Process.create
      ~prog:Sys.executable_name
      ~args:[ test_child_subcommand;
              Int.to_string t.id;
            ]
      ()
    >>= function
    | Error e -> Error.raise e
    | Ok process ->
      Process.collect_output_and_wait process
      >>= fun output ->
      Monitor.try_with (fun () -> Expect.eval t.expect output)
      >>| function
      | Ok (Ok ()) -> ()
      | Ok (Error error) ->
        failwiths "check_in_parent returned error" error <:sexp_of< Error.t >>
      | Error exn ->
        failwiths "check_in_parent raised" exn <:sexp_of< exn >>
  ;;

  let run_all () =
    Deferred.List.filter_map !all ~f:(fun t ->
      Process.run
        ~prog:Sys.executable_name
        ~args:[ test_parent_subcommand;
                Int.to_string t.id;
              ]
        ()
      >>| function
      | Error error -> Some (t, error)
      | Ok "" -> None
      | Ok s ->
        Some (t,
              Error.create "\
parent unexpectedly wrote to stdout while exiting with status zero"
                s <:sexp_of< string >>))
    >>| fun errors ->
    if not (List.is_empty errors) then
      failwiths "errors"
        (List.map errors ~f:(fun (t, error) -> (t.source_code_position, error)))
        (<:sexp_of< (Source_code_position.t * Error.t) list >>);
  ;;
end

let add_test = Test.add

let shutdown_after d = upon d (fun () -> shutdown 0)

let main () =
  Command.(
    Spec.(
      run
        (group ~summary:"testing stuff"
           [ "list",
             basic ~summary:"list all the tests"
               empty
               (fun () ->
                 eprintf "%s\n"
                   (Sexp.to_string_hum (<:sexp_of< Test.t list >> !Test.all));
                 shutdown 0);

             "test-all",
             basic ~summary:"run all the tests"
               empty
               (fun () -> shutdown_after (Test.run_all ()));

             test_parent_subcommand,
             basic ~summary:"run the parent process of a test"
               (empty +> anon ("ID" %: int))
               (fun id () -> shutdown_after (Test.run_parent id));

             test_child_subcommand,
             basic ~summary:"run the child process of a test"
               (empty +> anon ("ID" %: int))
               (fun id () -> shutdown_after (Test.run_child id));

           ])));
  Scheduler.go ();
;;
