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

type when_finished =
  [ `Notify_the_scheduler
  | `Take_the_async_lock
  | `Best
  ]
with sexp_of

let runs =
  List.map [ `Notify_the_scheduler
           ; `Take_the_async_lock
           ; `Best
           ]
    ~f:(fun when_finished ->
      (String.concat [ "In_thread.run__"
                     ; when_finished |> <:sexp_of< when_finished >> |> Sexp.to_string
                     ],
       (fun () -> In_thread.run ~when_finished ignore)))
;;

let tests =
  [ "In_thread.syscall", in_thread_syscall
  ]
  @ runs
;;
