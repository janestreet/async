open! Core
open! Async
open! Async_dynamic_log

(* type tests = | Global | Regular [@@warning "-37"] *)

(* let test_global () = Global_dynamic_log.set_listener
   "/home/ubuntu/async-logging/async/async_dynamic_log/bin/log_control_1.txt"
   |> don't_wait_for; upon (Clock.after (Time_float_unix.Span.of_int_ms 100))
   (fun () -> Clock_ns.every (Time_ns_unix.Span.create ?ms:(Some 1000) ())
   (fun () -> Global_dynamic_log.debug_s [%message "Sample debug message"];
   Global_dynamic_log.info_s [%message "Sample info message"];
   Global_dynamic_log.error_s [%message "Sample error message"])) ;; *)

let test_regular () =
  let send_errors_to_top_level_monitor e =
    let e = try Error.raise e with e -> e in
    Monitor.send_exn Monitor.main ~backtrace:`Get e
  in
  let log =
    Log_dynamic.create
      ~level:`Info
      ~output:[ Log.Output.stderr () ]
      ~on_error:(`Call send_errors_to_top_level_monitor)
      ~listener_file_path:
        "/home/ubuntu/async-logging/async/async_dynamic_log/bin/log_control_1.txt"
      ()
  in
  Clock_ns.every (Time_ns_unix.Span.create ?ms:(Some 1000) ()) (fun () ->
      Log_dynamic.debug_s log [%message "Sample debug message"];
      Log_dynamic.info_s log [%message "Sample info message"];
      Log_dynamic.error_s log [%message "Sample error message"])
;;

(* let test which () = match which with Global -> test_global () | Regular ->
   test_regular () ;; *)

let command =
  Command.basic
    ~summary:"log verbosity change proof of concept"
    [%map_open.Command
      let () = return () in
      fun () -> test_regular ()]
;;
