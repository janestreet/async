open Core.Std
open Async.Std

let () =
  (* This raises a toplevel-unhandled exception in Async.  But we haven't started the
     scheduler yet. *)
  Scheduler.within (fun () ->
    failwith "this exception should be caught by the try-with below");
  (* We start the scheduler, which should raise the exception. *)
  begin
    try
      never_returns (Scheduler.go ~raise_unhandled_exn:true ())
    with
    | _ -> Pervasives.exit 0
  end;
  assert false;
;;
