open Core.Std
open Async.Std
open Async_extended.Std

let test1 () =
  Process.backtick_new_exn ~prog:"ls" ~args:[] ()
  >>| fun _ls_output_string ->
  ()
;;

let test2 () =
  Process.backtick_status ~prog:"ls" ~args:[] ()
  >>| fun ({ Process.Output. stdout = _; stderr }, exit_or_signal) ->
  assert (Result.is_ok exit_or_signal);
  assert (stderr = "");
;;

let tests =
  [ "Process_test1", test1;
    "Process_test2", test2;
  ]
;;
