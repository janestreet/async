open! Core
open! Async
open! Async_dynamic_log

let test () =
  Core.print_endline (Int.to_string [%here].Lexing.pos_lnum);
  let send_errors_to_top_level_monitor e =
    let e = try Error.raise e with e -> e in
    Monitor.send_exn Monitor.main ~backtrace:`Get e
  in
  let log =
    Log_dynamic.create
      ~level:`Info
      ~output:[ Log.Output.stderr () ]
      ~on_error:(`Call send_errors_to_top_level_monitor)
      ()
  in
  upon
    (Log_dynamic.set_control_file
       "/home/ubuntu/async-logging/async/async_dynamic_log/src/gen.txt")
    (fun () ->
      Clock_ns.every (Time_ns.Span.create ~sec:1 ()) (fun () ->
          Log_dynamic.error_s [%here] log [%message "hi"];
          Log_dynamic.error_s [%here] log [%message "bye"]))
;;

let command =
  Command.basic
    ~summary:"log verbosity change proof of concept"
    [%map_open.Command
      let () = return () in
      fun () -> test ()]
;;