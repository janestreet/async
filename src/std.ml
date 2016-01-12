include Async_kernel.Std
include Async_unix.Std
include Async_extra.Std

let%test "Async library initialization does not initialize the scheduler" =
  Scheduler.is_ready_to_initialize ()
;;

