open Core.Std  let _ = _squelch_unused_module_warning_
open Async.Std
open Async_test_in_child_process

let () = add_test _here_ Expect.ok (fun () ->
  match Core.Std.Unix.fork () with
  | `In_the_parent pid -> Unix.waitpid_exn pid
  | `In_the_child ->
    Scheduler.reset_in_forked_process ();
    Deferred.unit)
;;

let () = add_test _here_ Expect.ok (fun () ->
  match Core.Std.Unix.fork () with
  | `In_the_parent pid -> Unix.waitpid_exn pid
  | `In_the_child ->
    Scheduler.reset_in_forked_process ();
    after (sec 0.01))
;;
