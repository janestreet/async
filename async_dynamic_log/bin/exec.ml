open! Core
open! Async

let () =
  Command_unix.run Testing.command;
  Core.never_returns (Async_unix.Scheduler.go ())
;;
