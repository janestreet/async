open Core
open Async

let () =
  let test num_live_jobs =
    Gc.compact ();
    Deferred.create (fun finished ->
      let num_jobs = ref 0 in
      let start = Time_float.now () in
      upon
        (after (sec 5.))
        (fun () ->
          let elapsed = Time_float.diff (Time_float.now ()) start in
          Core.eprintf
            "num_live_jobs: %7d  nanos per job: %d\n%!"
            num_live_jobs
            (Float.iround_nearest_exn
               (Time_float.Span.to_ns elapsed /. Float.of_int !num_jobs));
          Ivar.fill_exn finished ());
      for _ = 1 to num_live_jobs do
        let rec loop () =
          upon Deferred.unit (fun () ->
            incr num_jobs;
            if Ivar.is_empty finished then loop ())
        in
        loop ()
      done)
  in
  upon
    (Deferred.repeat_until_finished 1 (fun num_live_jobs ->
       if num_live_jobs > 2_000_000
       then return (`Finished ())
       else test num_live_jobs >>| fun () -> `Repeat (num_live_jobs * 2)))
    (fun () -> shutdown 0);
  never_returns (Scheduler.go ())
;;
