open Core.Std
open Async.Std

let eprintf = Core.Std.Printf.eprintf

let resurrect = ref []

let rec finished x =
  eprintf "finished\n%!";
  resurrect := x :: !resurrect;
  Gc.add_finalizer_exn x finished;
;;

let () =
  let s = String.create 10 in
  Gc.add_finalizer_exn s finished;
;;

let compact () =
  resurrect := [];
  eprintf "compacting\n%!";
  Gc.compact ();
  eprintf "done compacting\n%!";
;;

let rec loop i =
  if i = 0 then
    shutdown 0
  else
    upon (Clock.after (sec 1.)) (fun () ->
      compact ();
      loop (i - 1))
;;

let () = loop 5

let () = never_returns (Scheduler.go ())

