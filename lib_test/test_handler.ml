open Core.Std
open Async.Std
let return = Deferred.return

exception Exn

let t0 () =
  Deferred.create (fun ivar ->
    Monitor.try_with (fun () -> raise Exn) >>> function
      | Ok () -> assert false
      | Error error ->
        assert (Monitor.extract_exn error = Exn);
          Ivar.fill ivar ())
;;

let t1 () =
  let monitor = Monitor.create ~name:"1st-monitor" () in
  let errors = Monitor.errors monitor in
  Deferred.create (fun ivar ->
    begin
      Scheduler.within' ~monitor (fun () -> raise Exn) >>> fun _ ->
        assert false
    end;
    Stream.next errors >>> function
      | Stream.Nil -> assert false
      | Stream.Cons (error, _) ->
          assert (Monitor.extract_exn error = Exn);
          Ivar.fill ivar ())
;;

(* handlers run in the execution context that was in effect when they
   were created rather than the execution context that is in effect when
   they are installed. *)
let t2 () =
  let m1 = Monitor.create ~name:"1st-monitor" () in
  let m2 = Monitor.create ~name:"2nd-monitor" () in
  let errors1 = Monitor.errors m1 in
  let errors2 = Monitor.errors m2 in
  Deferred.create (fun ivar ->
    Scheduler.within' ~monitor:m1 (fun () ->
      return (Handler.create (fun _ -> raise Exn (* to m1 *)))
    ) >>> fun handler ->
      Scheduler.within' ~monitor:m2 (fun () ->
        let _uninstall = Handler.install handler Deferred.unit in
        Deferred.unit
      ) >>> fun () ->
          (Stream.iter errors2 ~f:(fun _ -> assert false));
          (Stream.iter errors1 ~f:(fun error ->
            assert (Monitor.extract_exn error = Exn);
            Ivar.fill ivar ())))
;;

(* prepending to the handler under [m2] doesn't affect this property *)
let t3 () =
  let m1 = Monitor.create ~name:"1st-monitor" () in
  let m2 = Monitor.create ~name:"2nd-monitor" () in
  let errors1 = Monitor.errors m1 in
  let errors2 = Monitor.errors m2 in
  Deferred.create (fun ivar ->
    Scheduler.within' ~monitor:m1 (fun () ->
      return (Handler.create (fun _ -> raise Exn (* to m1 *)))
    ) >>> fun handler ->
      Scheduler.within' ~monitor:m2 (fun () ->
        let handler = Handler.prepend handler ~f:Fn.id in
        let _uninstall = Handler.install handler Deferred.unit in
        Deferred.unit
      ) >>> fun () ->
          (Stream.iter errors2 ~f:(fun _ -> assert false));
          (Stream.iter errors1 ~f:(fun error ->
            assert (Monitor.extract_exn error = Exn);
            Ivar.fill ivar ())))
;;

(* A variant of t3 where the failure happens inside the prepended function *)
let t6 () =
  let m1 = Monitor.create ~name:"1st-monitor" () in
  let m2 = Monitor.create ~name:"2nd-monitor" () in
  let errors1 = Monitor.errors m1 in
  let errors2 = Monitor.errors m2 in
  Deferred.create (fun ivar ->
    Scheduler.within' ~monitor:m1 (fun () ->
      return (Handler.create (fun x -> x))
    ) >>> fun handler ->
      Scheduler.within' ~monitor:m2 (fun () ->
        let handler =
          Handler.prepend handler ~f:(fun _ -> raise Exn (* to m1 *))
        in
        let _uninstall = Handler.install handler Deferred.unit in
        Deferred.unit
      ) >>> fun () ->
          (Stream.iter errors2 ~f:(fun _ -> assert false));
          (Stream.iter errors1 ~f:(fun error ->
            assert (Monitor.extract_exn error = Exn);
            Ivar.fill ivar ())))
;;

(* filtering the handler under [m2] doesn't affect this property *)
let t4 () =
  let m1 = Monitor.create ~name:"1st-monitor" () in
  let m2 = Monitor.create ~name:"2nd-monitor" () in
  let errors1 = Monitor.errors m1 in
  let errors2 = Monitor.errors m2 in
  Deferred.create (fun ivar ->
    Scheduler.within' ~monitor:m1 (fun () ->
      return (Handler.create (fun _ -> raise Exn (* to m1 *)))
    ) >>> fun handler ->
      Scheduler.within' ~monitor:m2 (fun () ->
        let handler = Handler.filter handler ~f:(fun _ -> true) in
        let _uninstall = Handler.install handler Deferred.unit in
        Deferred.unit
      ) >>> fun () ->
          (Stream.iter errors2 ~f:(fun _ -> assert false));
          (Stream.iter errors1 ~f:(fun error ->
            assert (Monitor.extract_exn error = Exn);
            Ivar.fill ivar ())))
;;

(* A variant of t4 where the failure happens inside the filter predicate *)
let t7 () =
  let m1 = Monitor.create ~name:"1st-monitor" () in
  let m2 = Monitor.create ~name:"2nd-monitor" () in
  let errors1 = Monitor.errors m1 in
  let errors2 = Monitor.errors m2 in
  Deferred.create (fun ivar ->
    Scheduler.within' ~monitor:m1 (fun () ->
      return (Handler.create (fun x -> x))
    ) >>> fun handler ->
      Scheduler.within' ~monitor:m2 (fun () ->
        let handler =
          Handler.filter handler ~f:(fun _ -> raise Exn (* to m1 *))
        in
        let _uninstall = Handler.install handler Deferred.unit in
        Deferred.unit
      ) >>> fun () ->
          (Stream.iter errors2 ~f:(fun _ -> assert false));
          (Stream.iter errors1 ~f:(fun error ->
            assert (Monitor.extract_exn error = Exn);
            Ivar.fill ivar ())))
;;

(* handlers can be uninstalled *)
let t5 () =
  let m1 = Monitor.create ~name:"1st-monitor" () in
  let m2 = Monitor.create ~name:"2nd-monitor" () in
  let errors1 = Monitor.errors m1 in
  let errors2 = Monitor.errors m2 in
  Deferred.create (fun ivar ->
    Scheduler.within' ~monitor:m1 (fun () ->
      return (Handler.create (fun _ -> raise Exn (* to m1 *)))
    ) >>> fun handler ->
      Scheduler.within' ~monitor:m2 (fun () ->
        let uninstall = Handler.install handler Deferred.unit in
        uninstall ();
        Deferred.unit
      ) >>> fun () ->
          (Stream.iter errors2 ~f:(fun _ -> assert false));
          (Stream.iter errors1 ~f:(fun _ -> assert false));
          Clock.after Time.Span.millisecond >>> fun () ->
            Ivar.fill ivar ())
;;

let tests = [
  ("Test_handler.t0", t0);
  ("Test_handler.t1", t1);
  ("Test_handler.t2", t2);
  ("Test_handler.t3", t3);
  ("Test_handler.t4", t4);
  ("Test_handler.t5", t5);
  ("Test_handler.t6", t6);
  ("Test_handler.t7", t7);
]
