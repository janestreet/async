open Core.Std
open Async.Std

let counter () =
  let i = ref 0 in
  Scheduler.add_busy_poller (fun () ->
    incr i;
    if !i < 1_000_000 then
      `Continue_polling
    else begin
      `Stop_polling ()
    end)
;;

let stop_after () =
  let continue = ref true in
  upon (after (sec 0.001)) (fun () -> continue := false);
  Scheduler.add_busy_poller (fun () ->
    if !continue then
      `Continue_polling
    else
      `Stop_polling ())
;;

let at_intervals () =
  let rec try_more num_remaining_tries =
    let stop = after (sec 1.) in
    let num_fires = ref 0 in
    let busy_poller_finished =
      Scheduler.add_busy_poller (fun () ->
        if is_some (Deferred.peek stop) then
          `Stop_polling ()
        else
          `Continue_polling)
    in
    Stream.iter (Clock.at_intervals (sec 0.001) ~stop) ~f:(fun () -> incr num_fires);
    stop
    >>= fun () ->
    busy_poller_finished
    >>= fun () ->
    let num_fires = !num_fires in
    if 500 <= num_fires && num_fires <= 15_000 then
      Deferred.unit
    else if num_remaining_tries = 0 then
      failwiths "bad num_fires" num_fires <:sexp_of< int >>
    else
      try_more (num_remaining_tries - 1)
  in
  try_more 5
;;

let error () =
  let monitor = Monitor.create () in
  let d1 = Stream.next (Monitor.detach_and_get_error_stream monitor) in
  let d2 =
   within' ~monitor (fun () ->
      Scheduler.add_busy_poller (fun () -> failwith "foo"))
  in
  d1
  >>| fun _ ->
  assert (not (Deferred.is_determined d2));
;;

let stop_after_error () =
  let monitor = Monitor.create () in
  let d = Stream.next (Monitor.detach_and_get_error_stream monitor) in
  don't_wait_for
    (within' ~monitor (fun () ->
       let first_time = ref true in
       Scheduler.add_busy_poller (fun () ->
         if !first_time
         then (first_time := false; failwith "foo")
         else failwith "error")));
  d
  >>| fun _ ->
  ()
;;

let tests =
  [ "Busy_poll_test.counter", counter;
    "Busy_poll_test.stop_after", stop_after;
    "Busy_poll_test.at_intervals", at_intervals;
    "Busy_poll_test.error", error;
    "Busy_poll_test.srop_after_error", stop_after_error;
  ]
;;
