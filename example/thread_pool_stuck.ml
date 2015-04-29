open Core.Std   let _ = _squelch_unused_module_warning_
open Async.Std

let () =
  let num_sleeping = ref 0 in
  let message () =
    if false then Core.Std.eprintf "%d sleeping\n%!" !num_sleeping
  in
  let consume ~num_jobs ~sleep_for =
    Deferred.ignore
      (Deferred.List.init ~how:`Parallel num_jobs ~f:(fun _ ->
         In_thread.run (fun () ->
           incr num_sleeping;
           message ();
           Core.Std.Unix.sleep sleep_for;
           decr num_sleeping;
           message ())))
  in
  (* This causes a single message after about one second, while the initial 50 jobs are
     blocked.  Then, no more messages for then next ~20s as the 500 jobs complete and new
     ones take their place. *)
  consume ~num_jobs:500 ~sleep_for:2
  >>> fun () ->
  (* This causes one message per second for 60s, until failure. *)
  consume ~num_jobs:100 ~sleep_for:100
  >>> fun () ->
  shutdown 0
;;

let () = never_returns (Scheduler.go ())
