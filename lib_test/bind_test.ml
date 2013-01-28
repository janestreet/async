open Core.Std
open Async.Std

(* Note: The ordering of trace_connect outputs don't appear
 to correctly correspond to tests.  Sorry! *)

(* [protect]  generates a single spurious empty_star trace.
   [all_unit] generates a single spurious full_star trace. *)

(* Reminder:

  let bind t f =
    create (fun i -> t >>> fun a -> connect i (f a))
  ;;
*)

let trace = ref false
(* let trace = Deferred.debug_trace_connect *)

(** Guaranteed to be filled after everything else that
 will be scheduled this thread has been executed. *)
let tick () = Deferred.create (fun ivar ->
  Deferred.unit >>> fun () ->
  Deferred.unit >>> fun () ->
  Ivar.fill ivar ())

let chatty f () =
  let g () = Deferred.create (fun ivar ->
    trace := true;
    f () >>> fun () -> (trace := false; Ivar.fill ivar ())
  ) in
  Monitor.protect g ~finally:(fun () -> trace := false; return ())
;;

let t_full_star () =
  Deferred.unit >>= fun () -> Deferred.unit
;;

let t_empty_star () =
  Deferred.unit >>= fun () -> tick ()
;;

let t_star_empty () =
  Deferred.create (fun ivar ->
    ignore (Deferred.unit >>= fun () ->
            let x = tick () in
            x >>> (fun () -> Ivar.fill ivar ());
            x))
;;

let t_inline_inline () =
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  let m = Deferred.unit >>= fun () -> begin
    let x = tick () in
    x >>> (fun () -> Ivar.fill ivar1 ());
    x
  end in
  m >>> (fun () -> Ivar.fill ivar2 ());
  Deferred.all_unit [Ivar.read ivar1; Ivar.read ivar2]
;;

let t_inline_many () =
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  let ivar3 = Ivar.create () in
  let m = Deferred.unit >>= fun () -> begin
    let x = tick () in
    x >>> (fun () -> Ivar.fill ivar1 ());
    x
  end in
  m >>> (fun () -> Ivar.fill ivar2 ());
  m >>> (fun () -> Ivar.fill ivar3 ());
  Deferred.all_unit [Ivar.read ivar1; Ivar.read ivar2; Ivar.read ivar3]
;;

let t_many_inline () =
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  let ivar3 = Ivar.create () in
  let m = Deferred.unit >>= fun () -> begin
    let x = tick () in
    x >>> (fun () -> Ivar.fill ivar1 ());
    x >>> (fun () -> Ivar.fill ivar2 ());
    x
  end in
  m >>> (fun () -> Ivar.fill ivar3 ());
  Deferred.all_unit [Ivar.read ivar1; Ivar.read ivar2; Ivar.read ivar3]
;;

let t_many_many () =
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  let ivar3 = Ivar.create () in
  let ivar4 = Ivar.create () in
  let m = Deferred.unit >>= fun () -> begin
    let x = tick () in
    x >>> (fun () -> Ivar.fill ivar1 ());
    x >>> (fun () -> Ivar.fill ivar2 ());
    x
  end in
  m >>> (fun () -> Ivar.fill ivar3 ());
  m >>> (fun () -> Ivar.fill ivar4 ());
  Deferred.all_unit [Ivar.read ivar1; Ivar.read ivar2; Ivar.read ivar3; Ivar.read ivar4]
;;

(* What are the semantics of these tests? We would like to verify
   that if we attach a removable callback to a deferred, and
   then that deferred gets modified by a [connect] (this means
   it had to be a bind, and its callback needs to fire after
   we attached the removable callback), we would like to verify that the
   callback is still removed.  The steps are relatively
   simple, but arranging all of the callbacks to fire in the
   right order is not.

   Deferreds are numbered in the order they should be filled.
   Ivars named retX represent Ivars that are filled in order to
   ensure all necessary callbacks are called.
*)

let never_called () = assert false
let nop () = ()

(* KEY:
     i  t <-- deferreds
  m >>= f <-- syntax

   i <-- empty deferred with name i
  [ ]--(C) <-- removable callback
    \--( ) <-- normal callback
    \--{ } <-- inline callback
    \-- .  <-- indicates that though there is only one
               callback, this is a bag with one element

  [X] <-- full deferred
  [ ]<--[I] <-- indirection
*)

(*

1 is anything (in this case, Empty_many_handlers
with a removable callback and a normal callback)
and 2 is empty (the result of a bind with nothing
attached to it).

       1     2
 (C)--[ ]   [ ]
 ( )--/

Callback for bind fires:

      [I]-->[ ]--(C)
             \---( )

Removable callback is removed:

      [I]-->[ ]--( )

1 is filled in, normal callback fires:

      [I]-->[X]***X

*)
let t_choice_star_empty () =
  let ret = Ivar.create () in
  let i1 = Ivar.create () in
  let t1 = Ivar.read i1 in
  let remove = Ivar.create () in
  let tremove = Ivar.read remove in
  t1 >>> Ivar.fill ret;
  ignore (choose [choice t1 never_called; choice tremove nop]);
  let t2 = Deferred.unit >>= fun () -> Ivar.fill remove (); t1 in
  ignore t2;
  tremove >>> Ivar.fill i1;
  Ivar.read ret
;;

(*
1 has one handler (the removable callback), 2 has one
inline handler (a normal callback). (Note that 1 cannot be an inline
handler, due to implementation reasons.)

       1     2
 (C)--[ ]   [ ]--{ }

Callback for bind fires:

      [I]-->[ ]--( )
              \--(C)
*)

let t_choice_one_inline () =
  let ret = Ivar.create () in
  let i1 = Ivar.create () in
  let t1 = Ivar.read i1 in
  let remove = Ivar.create () in
  let tremove = Ivar.read remove in
  ignore (choose [choice t1 never_called; choice tremove nop]);
  let t2 = Deferred.unit >>= fun () -> Ivar.fill remove (); t1 in
  t2 >>> Ivar.fill ret;
  tremove >>> Ivar.fill i1;
  Ivar.read ret

(*
1 has one inline handler (normal) and 2 has one handler (removable).

       1     2
 { }--[ ]   [ ]--(C)

Callback for bind fires:

      [I]-->[ ]--(C)
              \--( )
*)
let t_choice_inline_one () =
  let ret = Ivar.create () in
  let i1 = Ivar.create () in
  let t1 = Ivar.read i1 in
  let remove = Ivar.create () in
  let tremove = Ivar.read remove in
  let t2 = Deferred.unit >>= fun () -> Ivar.fill remove (); t1 in
  ignore (choose [choice t2 never_called; choice tremove nop]);
  t1 >>> Ivar.fill ret;
  tremove >>> Ivar.fill i1;
  Ivar.read ret

(* 1 has one handler (removable) and 2 has one handler (removable). *)
let t_choice_one_one () =
  let i1 = Ivar.create () in
  let t1 = Ivar.read i1 in
  let remove = Ivar.create () in
  let tremove = Ivar.read remove in
  let t2 = Deferred.unit >>= fun () -> Ivar.fill remove (); t1 in
  tremove >>> Ivar.fill i1;
  ignore (choose [choice t1 never_called; choice tremove nop]);
  ignore (choose [choice t2 never_called; choice tremove nop]);
  tremove

(* 1 has one handler (removable) and 2 has many handlers (one removable). *)
let t_choice_one_many () =
  let ret = Ivar.create () in
  let remove = Ivar.create () in
  let tremove = Ivar.read remove in
  let i1 = Ivar.create () in
  let t1 = Ivar.read i1 in
  let t2 = Deferred.unit >>= fun () -> Ivar.fill remove (); t1 in
  t2 >>> Ivar.fill ret;
  tremove >>> Ivar.fill i1;
  ignore (choose [choice t1 never_called; choice tremove nop]);
  ignore (choose [choice t2 never_called; choice tremove nop]);
  Ivar.read ret

(* 2 has one handler (removable) and 1 has many handlers (one removable). *)
let t_choice_many_one () =
  let ret = Ivar.create () in
  let remove = Ivar.create () in
  let tremove = Ivar.read remove in
  let i1 = Ivar.create () in
  let t1 = Ivar.read i1 in
  let t2 = Deferred.unit >>= fun () -> Ivar.fill remove (); t1 in
  t1 >>> Ivar.fill ret;
  tremove >>> Ivar.fill i1;
  ignore (choose [choice t1 never_called; choice tremove nop]);
  ignore (choose [choice t2 never_called; choice tremove nop]);
  Ivar.read ret

(* 1 has one inline handler and 2 has many handlers (one removable). *)
let t_choice_inline_many () =
  let ret1 = Ivar.create () in
  let ret2 = Ivar.create () in
  let remove = Ivar.create () in
  let tremove = Ivar.read remove in
  let i1 = Ivar.create () in
  let t1 = Ivar.read i1 in
  let t2 = Deferred.unit >>= fun () -> Ivar.fill remove (); t1 in
  t1 >>> Ivar.fill ret1;
  t2 >>> Ivar.fill ret2;
  tremove >>> Ivar.fill i1;
  ignore (choose [choice t2 never_called; choice tremove nop]);
  Deferred.all_unit [Ivar.read ret1; Ivar.read ret2]

(* 2 has one inline handler and 1 has many handlers (one removable). *)
let t_choice_many_inline () =
  let ret1 = Ivar.create () in
  let ret2 = Ivar.create () in
  let remove = Ivar.create () in
  let tremove = Ivar.read remove in
  let i1 = Ivar.create () in
  let t1 = Ivar.read i1 in
  let t2 = Deferred.unit >>= fun () -> Ivar.fill remove (); t1 in
  t1 >>> Ivar.fill ret1;
  t2 >>> Ivar.fill ret2;
  tremove >>> Ivar.fill i1;
  ignore (choose [choice t1 never_called; choice tremove nop]);
  Deferred.all_unit [Ivar.read ret1; Ivar.read ret2]

(* 1 has many handlers (one removable) and 2 has many handlers (one removable). *)
let t_choice_many_many () =
  let ret1 = Ivar.create () in
  let ret2 = Ivar.create () in
  let remove = Ivar.create () in
  let tremove = Ivar.read remove in
  let i1 = Ivar.create () in
  let t1 = Ivar.read i1 in
  let t2 = Deferred.unit >>= fun () -> Ivar.fill remove (); t1 in
  t1 >>> Ivar.fill ret1;
  t2 >>> Ivar.fill ret2;
  tremove >>> Ivar.fill i1;
  ignore (choose [choice t1 never_called; choice tremove nop]);
  ignore (choose [choice t2 never_called; choice tremove nop]);
  Deferred.all_unit [Ivar.read ret1; Ivar.read ret2]

let tests = [
  ("Bind_test.t_full_star",   chatty t_full_star);
  ("Bind_test.t_empty_star",  chatty t_empty_star);
  ("Bind_test.t_star_empty",  chatty t_star_empty);
  ("Bind_test.t_inline_inline", chatty t_inline_inline);
  ("Bind_test.t_inline_many", chatty t_inline_many);
  ("Bind_test.t_many_inline", chatty t_many_inline);
  ("Bind_test.t_many_many",   chatty t_many_many);
  ("Bind_test.t_choice_star_empty",  chatty t_choice_star_empty);
  ("Bind_test.t_choice_one_inline",  chatty t_choice_one_inline);
  ("Bind_test.t_choice_inline_one",  chatty t_choice_inline_one);
  ("Bind_test.t_choice_one_one",     chatty t_choice_one_one);
  ("Bind_test.t_choice_one_many",    chatty t_choice_one_many);
  ("Bind_test.t_choice_many_one",    chatty t_choice_many_one);
  ("Bind_test.t_choice_inline_many", chatty t_choice_inline_many);
  ("Bind_test.t_choice_many_inline", chatty t_choice_many_inline);
  ("Bind_test.t_choice_many_many",   chatty t_choice_many_many);
]
