open Jane.Std
open Async.Std

module Sound = Async_extended.Std.Sound (* so omake isn't confused about a circular dependency *)

let () = upon (Sound.play Sound.default) (fun () -> shutdown 0)

let () = never_returns (Scheduler.go ())
