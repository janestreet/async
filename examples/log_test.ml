open Core.Std
open Async.Std

let main () =
  let open Log in
  Global.set_level `Debug;
  let rotating_file =
    Output.rotating_file
      `Sexp
      ~basename:"/tmp/log_test/messages"
      { Rotation.messages = None
      ; size = None
      ; time = Some (Time.Ofday.create ~hr:13 ~min:47 (), Zone.machine_zone ())
      ; keep = `At_least 3
      }
  in
  Global.set_output [Output.screen; rotating_file];
  let i = ref 0 in
  Clock.every (sec 1.) (fun () ->
    Global.info "%d" !i;
    incr i
  )
;;

let () =
  main ();
  never_returns (Scheduler.go ())
