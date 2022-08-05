open! Core
open! Async
open! Async_dynamic_log

let test () =
  let send_errors_to_top_level_monitor e =
    let e = try Error.raise e with e -> e in
    Monitor.send_exn Monitor.main ~backtrace:`Get e
  in
  let%bind log =
    Log_dynamic.create
      ~level:`Info
      ~output:[ Log.Output.stderr () ]
      ~on_error:(`Call send_errors_to_top_level_monitor)
      ~listener_file_path:
        "/home/ubuntu/async-logging/async/async_dynamic_log/src/gen.txt"
      ()
  in
  Deferred.never
    (Clock_ns.every (Time_ns.Span.create ~sec:1 ()) (fun () ->
         Log_dynamic.error_s [%here] log [%message "hi"];
         Log_dynamic.error_s [%here] log [%message "bye"]))
;;

let command =
  Command.async
    ~summary:"log verbosity change proof of concept"
    [%map_open.Command
      let () = return () in
      fun () -> test ()]
;;