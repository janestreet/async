open! Core
open! Async

let () =
  Command_unix.run Testing_individual_statements.command;
  Core.never_returns (Async_unix.Scheduler.go ())
;;
