open Core.Std
open Async.Std

module File_tail = Async.Std.File_tail

let () =
  let files = List.tl_exn (Array.to_list Sys.argv) in
  Deferred.List.iter ~how:`Parallel files ~f:(fun file ->
    Pipe.iter_without_pushback (File_tail.create file) ~f:(fun update ->
      printf "%s %s\n%!" file (File_tail.Update.to_string_hum update)))
  >>> fun () ->
  shutdown 0
;;

let () = never_returns (Scheduler.go ())
