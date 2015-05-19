open Core.Std
open Std

TEST_MODULE = struct
  TEST_UNIT = (* abort *)
    Thread_safe.block_on_async_exn (fun () ->
      let event = Clock.Event.after (sec 1_000.) in
      assert (Clock.Event.abort event = `Ok);
      assert (Clock.Event.abort event = `Previously_aborted);
      Clock.Event.fired event
      >>= function
      | `Happened -> assert false
      | `Aborted  -> return ())
  ;;

  TEST_UNIT = (* happen *)
    Thread_safe.block_on_async_exn (fun () ->
      Deferred.List.iter [ -1.; 0.; 0.01 ] ~f:(fun span ->
        let span = sec span in
        let ran = ref false in
        let event =
          Clock.Event.run_at (Time.add (Time.now ()) span) (fun () -> ran := true) ()
        in
        Clock.Event.fired event
        >>| function
        | `Aborted  -> assert false
        | `Happened ->
          assert !ran;
          assert (Clock.Event.abort event = `Previously_happened)))
  ;;

  TEST_UNIT = (* reschedule *)
    Thread_safe.block_on_async_exn (fun () ->
      let count = ref 0 in
      let time0 = Timing_wheel_ns.now (Async_kernel.Scheduler0.t ()).events in
      let after span = Time_ns.add time0 span in
      let module Event = Clock_ns.Event in
      let sec = Time_ns.Span.of_sec in
      let event = Event.run_at (after (sec 1.)) incr count in
      let ensure_scheduled_after span =
        <:test_result< int >> !count ~expect:0;
        match Event.status event with
        | `Aborted | `Happened -> assert false
        | `Scheduled_at time -> <:test_result< Time_ns.t >> time ~expect:(after span)
      in
      ensure_scheduled_after (sec 1.);
      let ensure_reschedule_after_is_ok span =
        match Event.reschedule_at event (after span) with
        | `Ok -> ensure_scheduled_after span
        | `Previously_aborted
        | `Previously_happened
        | `Too_late_to_reschedule
          -> assert false
      in
      ensure_reschedule_after_is_ok (sec 2. );
      ensure_reschedule_after_is_ok (sec 1. );
      ensure_reschedule_after_is_ok (sec 0.1);
      (* Now, we reschedule for a time not in the future, which causes Async to enqueue he
         job to fire the event.  Subsequently, rescheduling in the past is allowed, but
         rescheduling in the future is not. *)
      ensure_reschedule_after_is_ok (sec (-1.));
      ensure_reschedule_after_is_ok (sec 0.   );
      begin match Event.reschedule_after event (sec 1.) with
        | `Too_late_to_reschedule -> ensure_scheduled_after (sec 0.)
        | `Ok
        | `Previously_aborted
        | `Previously_happened
          -> assert false
      end;
      Event.fired event
      >>| function
      | `Aborted -> assert false
      | `Happened ->
        <:test_result< int >> !count ~expect:1;
        assert (Event.abort event = `Previously_happened);
        assert (Event.reschedule_after event (sec 1.) = `Previously_happened))
  ;;
end
