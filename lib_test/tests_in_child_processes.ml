open Core.Std
open Async.Std  let _ = _squelch_unused_module_warning_
open Async_test_in_child_process

(* The following [include]s exist to make those modules depend on this one. *)
include (Reset_in_forked_process_tests : sig end)
include (Socket_test : sig end)
include (Shutdown_tests : sig end)
include (Writer_close_and_shutdown_tests : sig end)

let () = never_returns (main ())
