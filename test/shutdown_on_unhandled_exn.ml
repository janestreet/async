open Core.Std
open Async.Std

let () =
  Shutdown.shutdown_on_unhandled_exn ();
  upon (return ()) (fun () -> failwith "this should cause [shutdown] to be called");
  Shutdown.at_shutdown (fun () ->
    Debug.amf _here_ "shutdown handler ran";
    return ());
  begin
    try
      never_returns (Scheduler.go ~raise_unhandled_exn:true ())
    with
    | _ -> assert false;
  end;
  assert false;
;;
