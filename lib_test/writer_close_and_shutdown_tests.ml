open Core.Std
open Async.Std

module Debug = Async_core.Debug

(* Turn off [ASYNC_CONFIG], so it isn't inherited by child processes. *)
let () = Core.Std.Unix.unsetenv "ASYNC_CONFIG"

let test_id_subcommand = "test-id"

let shutdown_after d = upon d (fun () -> shutdown 0)

module Test = struct
  type t =
    { id : int;
      info : Info.t;
      run : unit -> unit Deferred.t;
      check : unit -> unit Or_error.t Deferred.t;
    }

  let id_counter = ref 0

  let all = ref []

  let add ~info ~run ~check =
    incr id_counter;
    all := { id = !id_counter; info; run; check } :: !all;
  ;;

  let run_id id =
    match List.find !all ~f:(fun t -> id = t.id) with
    | None -> failwiths "no test with id" id <:sexp_of< int >>
    | Some t -> t.run ()
  ;;

  let run t =
    try_with (fun () ->
      Unix.fork_exec ~prog:Sys.executable_name
        ~args:[ Sys.executable_name;
                test_id_subcommand;
                Int.to_string t.id;
              ]
        ()
      >>= fun pid ->
      Unix.waitpid pid
      >>= function
      | Ok () -> t.check ()
      | Error error ->
        return
          (Error
             (Error.create "child process exited nonzero" error
                (<:sexp_of< Unix.Exit_or_signal.error >>))))
    >>| function
    | Ok (Ok ()) -> Ok ()
    | Ok (Error error) -> Error error
    | Error exn -> Error (Error.of_exn exn)
  ;;

  let run_all () =
    every (sec 0.1) (fun () -> ());
    Deferred.List.filter_map !all ~f:(fun t ->
      run t
      >>| function
      | Ok () -> None
      | Error error -> Some (t.info, error))
    >>| fun errors ->
    if not (List.is_empty errors) then
      failwiths "errors" errors <:sexp_of< (Info.t * Error.t) list >>
  ;;
end

let tmp_file = "/tmp/z.file"

let () =
  for power_of_two = 0 to 20 do
    let contents () =
      String.init (Float.to_int (Float.ldexp 1. power_of_two)) ~f:(fun i ->
        Char.of_int_exn (Int.rem i 256))
    in
    Test.add
      ~info:(Info.create ~here:_here_ "flush on shutdown" power_of_two <:sexp_of< int >>)
      ~run:(fun () ->
        Writer.open_file tmp_file
        >>= fun writer ->
        Writer.write writer (contents ());
        Deferred.unit)
      ~check:(fun () ->
        Reader.file_contents tmp_file
        >>= fun contents' ->
        Unix.unlink tmp_file
        >>| fun () ->
        if contents' = contents () then
          Ok ()
        else
          Or_error.error_string "contents mismatch")
  done
;;

let () =
  let string1 = "hello\n" in
  let string2 = "goodbye\n" in
  Test.add
    ~info:(Info.of_string "write to and flush an open writer after shutdown")
    ~run:(fun () ->
      Writer.open_file tmp_file
      >>= fun writer ->
      Shutdown.at_shutdown (fun () ->
        Writer.write writer string2;
        Writer.flushed writer);
      Writer.write writer string1;
      Deferred.unit)
    ~check:(fun () ->
      Reader.file_contents tmp_file
      >>= fun contents' ->
      Unix.unlink tmp_file
      >>| fun () ->
      if contents' = string1 ^ string2 then
        Ok ()
      else
        Or_error.error_string "contents mismatch")
;;

let () =
  let string1 = "hello\n" in
  let string2 = "goodbye\n" in
  Test.add
    ~info:(Info.of_string "write to and close an open writer after shutdown")
    ~run:(fun () ->
      Writer.open_file tmp_file
      >>= fun writer ->
      Shutdown.at_shutdown (fun () ->
        Writer.write writer string2;
        Writer.close writer);
      Writer.write writer string1;
      Deferred.unit)
    ~check:(fun () ->
      Reader.file_contents tmp_file
      >>= fun contents' ->
      Unix.unlink tmp_file
      >>| fun () ->
      if contents' = string1 ^ string2 then
        Ok ()
      else
        Or_error.error_string "contents mismatch")
;;

let command =
  Command.(
    Spec.(
      run
        (group ~summary:"testing stuff"
           [ "test-all",
             basic ~summary:"run all the tests"
               empty
               (fun () -> shutdown_after (Test.run_all ()));

             test_id_subcommand,
             basic ~summary:"run a single test"
               (empty +> anon ("ID" %: int))
               (fun id () -> shutdown_after (Test.run_id id));
           ])))
;;

let () = never_returns (Scheduler.go ())
