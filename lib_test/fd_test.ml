open Core.Std  let _ = _squelch_unused_module_warning_
open Async.Std

let syscall_in_thread_doesn't_raise () =
  Fd.syscall_in_thread ~name:"z" (Fd.stdout ())
    (fun _ -> raise (Unix.Unix_error (Unix.EINTR, "", "")))
  >>| function
  | `Error _ -> ()
  | `Already_closed | `Ok _ -> assert false
;;

let tests =
  [ "Fd.syscall_in_thread", syscall_in_thread_doesn't_raise;
  ]
;;
