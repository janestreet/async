open Core.Std
open Async.Std

let log = Async_kernel.Debug.log

let closeness ~expected ~actual = Float.abs (expected -. actual) /. expected

let sub_milli () =
  Deferred.create (fun success ->
    let interval = sec 0.0002 in
    let trial_length = sec 0.1 in
    let expected_num_intervals = Time.Span.(//) trial_length interval in
    let rec test trials_remaining =
      if trials_remaining = 0 then failwith "unable to succeed";
      let stop = ref false in
      let num_not_late = ref 0 in
      let num_late = ref 0 in
      upon (after trial_length) (fun () -> stop := true);
      Deferred.create (fun finished ->
        let rec loop at =
          if !stop then Ivar.fill finished ()
          else begin
            let next = Time.add at interval in
            if Time.(<=) (Time.now ()) next
            then incr num_not_late
            else incr num_late;
            Clock.run_at next loop next
          end
        in
        loop (Time.now ()))
      >>> fun () ->
      let c =
        closeness ~expected:expected_num_intervals
          ~actual:(Float.of_int !num_not_late)
      in
      if 0.1 < c
      then test (trials_remaining - 1)
      else Ivar.fill success ()
    in
    test 100)
;;

let tests = [
  "Clock_test.sub_milli", sub_milli;
]
