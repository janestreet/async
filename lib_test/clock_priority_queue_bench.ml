open Core.Std
open Async.Std

let stdout = force Writer.stdout

let log message a sexp_of_a =
  eprintf "%s\n" (Sexp.to_string_hum (Info.sexp_of_t (Info.create message a sexp_of_a)));
;;

module Report = struct
  type t =
    { alarms_per_second : int;
      load : string;
      real_time : Time.Span.t;
      num_alarms_error : float;
      load_error : float;
    }
  with sexp_of
end

let user_plus_sys () =
  let { Unix.tms_utime; tms_stime; _ } = Unix.times () in
  sec (tms_utime +. tms_stime)
;;

let main () =
  let test ~alarms_per_second ?(force_test_to_run_for = 0) () =
    Deferred.create (fun result ->
      let second = sec 1. in
      let start = Time.add (Time.now ()) second in
      let stop = ref false in
      let num_alarms = ref 0 in
      let num_stopped = ref 0 in
      let rec alarm_every_second_until_stop at =
        if !stop then begin
          incr num_stopped;
          if !num_stopped = alarms_per_second then Ivar.fill result ();
        end else begin
          incr num_alarms;
          Clock.run_at at
            (fun () -> alarm_every_second_until_stop (Time.add at second))
            ();
        end
      in
      for i = 0 to alarms_per_second - 1 do
        alarm_every_second_until_stop
          (Time.add start (Time.Span.of_sec (float i /. float alarms_per_second)));
      done;
      at start
      >>> fun () ->
      let user_plus_sys_at_start = user_plus_sys () in
      num_alarms := 0;
      let rec loop at i last_load =
        let at = Time.add at second in
        Clock.at at
        >>> fun () ->
        let now = Time.now () in
        let real_time = Time.diff now start in
        let user_plus_sys = Time.Span.(-) (user_plus_sys ()) user_plus_sys_at_start in
        let load = Time.Span.(//) user_plus_sys real_time in
        let expected_num_alarms = i * alarms_per_second in
        let num_alarms = !num_alarms in
        let error f1 f2 = Float.abs (f1 -. f2) /. (f1 +. f2) in
        let num_alarms_error =
          error (Float.of_int num_alarms) (Float.of_int expected_num_alarms)
        in
        let load_error = error load last_load in
        let print_status () =
          printf "%s\n"
            (Sexp.to_string_hum
               (Report.sexp_of_t
                  { Report.
                    alarms_per_second;
                    load = sprintf "%.0f%%" (load *. 100.);
                    real_time;
                    num_alarms_error;
                    load_error;
                  }));
        in
        let fail message =
          print_status ();
          Writer.flushed stdout
          >>> fun () ->
          failwith message
        in
        let expected_real_time = Time.Span.scale second (Float.of_int i) in
        if 0.1 < Float.abs (Time.Span.to_sec (Time.Span.(-) real_time expected_real_time))
        then
          fail "real time differs to much from expected_real_time"
        else if load_error > 0.01
            || num_alarms_error > 0.001
            || i < force_test_to_run_for
        then begin
          if i < 30 then
            loop at (i + 1) load
          else
            fail "unable to get stable readings"
        end else begin
          stop := true;
          print_status ();
        end
      in
      loop start 1 0.00)
  in
  let rec loop alarms_per_second =
    test ~alarms_per_second ()
    >>> fun () ->
    loop (alarms_per_second * 2)
  in
  if true then
    loop 1024
  else begin
    test ~alarms_per_second:65_536 ~force_test_to_run_for:20 ()
    >>> fun () ->
    let module Gc = Core.Std.Gc in
    log "gc" (Gc.stat ()) <:sexp_of< Gc.Stat.t >>;
    shutdown 0
  end;
  never ()
;;

let () =
  Command.run
    (Command.basic
       ~summary:"Benchmark for the clock priority queue of async."
       Command.Spec.(empty)
       (fun () ->
         upon (main ()) (fun () -> shutdown 0);
         never_returns (Scheduler.go ())))
;;
