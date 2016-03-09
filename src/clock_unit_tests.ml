open Core.Std
open Std

let%test_module "Clock.every" = (module struct
  let%test_unit _ = (* [~stop] *)
    Thread_safe.block_on_async_exn (fun () ->
      let r = ref 1_000 in
      let stop = Ivar.create () in
      Clock_ns.every Time_ns.Span.nanosecond ~stop:(Ivar.read stop)
        (fun () ->
           assert (!r >= 0);
           decr r;
           if !r = 0 then Ivar.fill stop ());
      Ivar.read stop)
  ;;

  let%test_unit "[every f ~stop] doesn't hold onto [f] after [stop] becomes determined" =
    Thread_safe.block_on_async_exn (fun () ->
      let event =
        Clock.Event.run_after (sec 10.) (fun () -> failwith "test timed out") ()
      in
      let stop = Ivar.create () in
      let ran_once = Ivar.create () in
      Clock.every Time.Span.day ~stop:(Ivar.read stop) (fun () -> Ivar.fill ran_once ());
      let ran_once_finalized = Ivar.create () in
      Gc.add_finalizer_exn ran_once (fun _ -> Ivar.fill ran_once_finalized ());
      Ivar.read ran_once
      >>= fun () ->
      Ivar.fill stop ();
      (* Yield so that [every] gets a chance to react to [stop] being filled, and clean
         up its (indirect) reference [ran_once]. *)
      Scheduler.yield ()
      >>= fun () ->
      (* [ran_once] is no longer live; a GC should enable its finalizer to run. *)
      Gc.full_major ();
      Ivar.read ran_once_finalized
      >>= fun () ->
      ignore (Clock.Event.abort event ());
      return ())
  ;;

  exception E

  let%test_unit _ = (* [~continue_on_error:true] *)
    Thread_safe.block_on_async_exn (fun () ->
      let count = 100 in
      let r = ref count in
      let stop = Ivar.create () in
      let exns =
        Monitor.catch_stream (fun () ->
          Clock_ns.every Time_ns.Span.nanosecond
            ~continue_on_error:true
            ~stop:(Ivar.read stop)
            (fun () ->
               assert (!r >= 0);
               decr r;
               if !r = 0 then Ivar.fill stop ();
               raise E))
      in
      let num_exns = ref 0 in
      let finished = Ivar.create () in
      Stream.iter exns ~f:(fun exn ->
        match Monitor.extract_exn exn with
        | E ->
          incr num_exns;
          if !num_exns = count then Ivar.fill finished ()
        | _ -> assert false);
      Ivar.read finished)
  ;;

  let%test_unit _ = (* [~continue_on_error:false] *)
    Thread_safe.block_on_async_exn (fun () ->
      let exns =
        Monitor.catch_stream (fun () ->
          Clock_ns.every Time_ns.Span.nanosecond ~continue_on_error:false
            (let called = ref false in
             fun () ->
               assert (not !called);
               called := true;
               raise E))
      in
      let finished = Ivar.create () in
      Stream.iter exns ~f:(fun exn ->
        match Monitor.extract_exn exn with
        | E -> Ivar.fill finished ();
        | _ -> assert false);
      Ivar.read finished)
  ;;

  let%test_unit _ = (* if [f] asynchronously raises and also returns, the exception goes to the
                 enclosing monitor, and iteration continues. *)
    List.iter
      [ false; true ]
      ~f:(fun continue_on_error ->
        Thread_safe.block_on_async_exn (fun () ->
          let stop = Ivar.create () in
          let exns =
            Monitor.catch_stream (fun () ->
              Clock_ns.every Time_ns.Span.nanosecond
                ~stop:(Ivar.read stop)
                ~continue_on_error
                (let r = ref 0 in
                 let do_raise = Ivar.create () in
                 fun () ->
                   incr r;
                   match !r with
                   | 1 -> upon (Ivar.read do_raise) (fun () -> raise E)
                   | 2 -> Ivar.fill do_raise ()
                   | 3 -> Ivar.fill stop ()
                   | _ -> assert false))
          in
          let got_exn = Ivar.create () in
          Stream.iter exns ~f:(fun exn ->
            match Monitor.extract_exn exn with
            | E -> Ivar.fill got_exn ();
            | _ -> assert false);
          Ivar.read stop
          >>= fun () ->
          Ivar.read got_exn))
  ;;
end)

let%test_module _ = (module struct
  module Event = Clock.Event

  let%test_unit _ = (* abort *)
    Thread_safe.block_on_async_exn (fun () ->
      let event = Event.after (sec 1_000.) in
      assert (Event.abort event () = `Ok);
      assert (Event.abort event () = `Previously_aborted ());
      Event.fired event
      >>= function
      | `Happened () -> assert false
      | `Aborted  () -> return ())
  ;;

  let%test_unit _ = (* happen *)
    Thread_safe.block_on_async_exn (fun () ->
      Deferred.List.iter [ -1.; 0.; 0.01 ] ~f:(fun span ->
        let span = sec span in
        let ran = ref false in
        let event =
          Event.run_at (Time.add (Time.now ()) span) (fun () -> ran := true) ()
        in
        Event.fired event
        >>| function
        | `Aborted  () -> assert false
        | `Happened () ->
          assert !ran;
          assert (Event.abort event () = `Previously_happened ())))
  ;;

  let%test_unit _ = (* reschedule *)
    Thread_safe.block_on_async_exn (fun () ->
      let count = ref 0 in
      let time0 = Timing_wheel_ns.now (Async_kernel.Scheduler1.t ()).time_source.events in
      let after span = Time_ns.add time0 span in
      let module Event = Clock_ns.Event in
      let sec = Time_ns.Span.of_sec in
      let event = Event.run_at (after (sec 1.)) incr count in
      let ensure_scheduled_after span =
        [%test_result: int] !count ~expect:0;
        match Event.status event with
        | `Aborted () | `Happened () -> assert false
        | `Scheduled_at time -> [%test_result: Time_ns.t] time ~expect:(after span)
      in
      ensure_scheduled_after (sec 1.);
      let ensure_reschedule_after_is_ok span =
        match Event.reschedule_at event (after span) with
        | `Ok -> ensure_scheduled_after span
        | `Previously_aborted ()
        | `Previously_happened ()
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
        | `Previously_aborted ()
        | `Previously_happened ()
          -> assert false
      end;
      Event.fired event
      >>| function
      | `Aborted  () -> assert false
      | `Happened () ->
        [%test_result: int] !count ~expect:1;
        assert (Event.abort event () = `Previously_happened ());
        assert (Event.reschedule_after event (sec 1.) = `Previously_happened ()))
  ;;

  let%test_unit _ = (* [Event.run_after] where [f] raises *)
    Thread_safe.block_on_async_exn (fun () ->
      let event = ref None in
      try_with (fun () ->
        event := Some (Event.run_after (sec 0.) (fun () -> failwith "foo") ());
        Deferred.never ())
      >>| function
      | Ok _ -> assert false
      | Error _ ->
        match Event.status (Option.value_exn !event) with
        | `Scheduled_at _ -> ()
        | `Aborted _ | `Happened _ -> assert false)
  ;;

  let%test_unit _ = (* [Event.run_after] where [f] calls [abort] *)
    Thread_safe.block_on_async_exn (fun () ->
      let event_ref = ref None in
      let event =
        Event.run_after (sec 0.) (fun () ->
          Event.abort_exn (Option.value_exn !event_ref) ()) ()
      in
      event_ref := Some event;
      Event.fired event
      >>| function
      | `Aborted () -> ()
      | `Happened () -> assert false)
  ;;
end)
