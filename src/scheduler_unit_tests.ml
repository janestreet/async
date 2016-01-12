open Core.Std
open Std

let%test_module _ = (module struct
  (* [Scheduler.run_cycles_until_no_jobs_remain] includes things scheduled in the past *)
  let%test_unit _ =
    Thread_safe.block_on_async_exn (fun () ->
      let has_determined = ref false in
      upon (after (sec 0.)) (fun () -> has_determined := true);
      Async_kernel.Scheduler.run_cycles_until_no_jobs_remain ();
      assert !has_determined;
      Deferred.unit)
  ;;

  (* [Scheduler.run_cycles_until_no_jobs_remain] keeps running cycles as long as there are
     jobs in the past. *)
  let%test_unit _ =
    Thread_safe.block_on_async_exn (fun () ->
      let has_determined = ref false in
      let rec loop i =
        if i = 0
        then has_determined := true
        else upon (after (sec 0.)) (fun () -> loop (i - 1))
      in
      loop 10;
      Async_kernel.Scheduler.run_cycles_until_no_jobs_remain ();
      assert !has_determined;
      Deferred.unit)
  ;;

  (* [Scheduler.run_cycles_until_no_jobs_remain] does not include things outside of the
     event precision. *)
  let%test_unit _ =
    Thread_safe.block_on_async_exn (fun () ->
      let has_determined = ref false in
      upon (after (Time.Span.of_day 1.)) (fun () -> has_determined := true);
      Async_kernel.Scheduler.run_cycles_until_no_jobs_remain ();
      assert (not !has_determined);
      Deferred.unit)
  ;;
end)
