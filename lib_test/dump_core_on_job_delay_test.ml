open Core.Std
open Async.Std

let main () =
  Async_unix.Dump_core_on_job_delay.start_watching
    ~dump_if_delayed_by:(sec 2.)
    ~how_to_dump:`Call_abort;
  don't_wait_for begin
    after (sec 5.)
    >>= fun () ->
    Writer.save "starting-to-block" ~contents:""
    >>= fun () ->
    Time.pause (sec 5.); (* block -- should cause core dump *)
    shutdown 0;
    Deferred.never ();
  end;
;;

let () =
  main ();
  never_returns (Scheduler.go ())
;;
