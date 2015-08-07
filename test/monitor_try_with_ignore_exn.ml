open Core.Std
open Async.Std

let () =
  don't_wait_for begin
    try_with (fun () ->
      upon (Scheduler.yield ()) (fun () ->
        upon (Scheduler.yield ()) (fun () -> shutdown 0);
        failwith "delayed exception");
      return ())
    >>= function
    | Ok ()   -> return ()
    | Error _ -> assert false
  end
;;

let () = never_returns (Scheduler.go ())

