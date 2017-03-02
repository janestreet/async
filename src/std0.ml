include Async_kernel
include Async_unix.Std
include Async_extra.Std

let%test "Async library initialization does not initialize the scheduler" =
  Scheduler.is_ready_to_initialize ()
;;

module Expect_test_config
  : Expect_test_config.S with type 'a IO.t = 'a Deferred.t =
struct
  module IO = Deferred

  let flush () =
    Deferred.all_unit
      [ Writer.flushed (Lazy.force Writer.stdout)
      ; Writer.flushed (Lazy.force Writer.stderr) ]

  let run f = Thread_safe.block_on_async_exn f

  let flushed () = Writer.bytes_to_write (Lazy.force Writer.stdout) = 0
end
