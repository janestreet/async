include Async_core.Std
include Async_unix.Std
include Async_extra.Std

(* Check that no Async library code accidentally created the scheduler. *)
let () = assert (Scheduler.is_ready_to_initialize ())
