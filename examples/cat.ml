open Jane.Std
open Async.Std

module Fd = Unix.Fd

let cat ~input ~output =
  let reader = Reader.create input in
  let writer = Writer.create ~raise_when_consumer_leaves:false output in
  let buf = String.create 4096 in
  let rec loop () =
    upon
      (choose
        [
          choice (Reader.read reader buf) (fun r -> `Reader r);
          choice (Writer.consumer_left writer) (fun () -> `Epipe);
        ])
    (function
      | `Reader r ->
        begin
          match r with
          | `Eof -> upon (Writer.flushed writer) (fun _ ->
            never_returns (Shutdown.shutdown_and_raise 0))
          | `Ok len ->
              Writer.write_substring writer (Substring.create buf ~pos:0 ~len);
              loop ()
        end
      | `Epipe -> never_returns (Shutdown.shutdown_and_raise 0))
  in
  loop ()
;;

let () = cat ~input:(Fd.stdin ()) ~output:(Fd.stdout ())

let () = never_returns (Scheduler.go ())
