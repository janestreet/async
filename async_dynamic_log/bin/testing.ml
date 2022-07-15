open! Core
open! Async
open! Async_dynamic_log.Log_dynamic

let test () =
  don't_wait_for
    (Global_dynamic.set_listener
       "/home/ubuntu/async-logging/async/async_dynamic_log/bin/lit.txt");
  Clock_ns.every (Time_ns_unix.Span.create ?ms:(Some 1000) ()) (fun () ->
      Global_dynamic.debug_s [%message "Sample debug log"];
      Global_dynamic.info_s [%message "Sample info log"];
      Global_dynamic.error_s [%message "Sample error log"])
;;

let command =
  Command.basic
    ~summary:"log verbosity change proof of concept"
    [%map_open.Command
      let () = return () in
      fun () -> test ()]
;;
