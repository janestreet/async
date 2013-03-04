open Core.Std  let _ = _squelch_unused_module_warning_
open Async.Std

let in_thread_syscall () =
  let module M = struct
    exception Foo
  end in
  In_thread.syscall ~name:"test" (fun () -> raise M.Foo)
  >>| function
  | Error M.Foo -> ()
  | _ -> assert false
;;

let tests =
  [ "In_thread.syscall", in_thread_syscall;
  ]
;;
