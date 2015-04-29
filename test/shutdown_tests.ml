open Core.Std
open Async.Std
open Async_test_in_child_process

let () = add_test _here_ Expect.(ok && no_output) (fun () -> return ())

let () =
  add_test _here_ Expect.(ok && no_output) (fun () ->
    let ivar = Ivar.create () in
    Shutdown.don't_finish_before (Ivar.read ivar);
    shutdown 0;
    Ivar.fill ivar ();
    Deferred.unit)
;;

let () =
  add_test _here_ Expect.(ok && no_output) (fun () ->
    let ivar = Ivar.create () in
    Shutdown.don't_finish_before (Ivar.read ivar);
    Shutdown.at_shutdown (fun () -> Ivar.fill ivar (); Deferred.unit);
    Deferred.unit);
;;

let () =
  add_test _here_ Expect.(ok && no_output) (fun () ->
    Shutdown.don't_finish_before (after (sec 0.01));
    Deferred.unit);
;;

let () =
  add_test _here_ Expect.error (fun () ->
    Shutdown.don't_finish_before (never ());
    shutdown 0 ~force:(after (sec 0.01));
    never ());
;;

let () =
  add_test _here_ Expect.(ok && no_output) (fun () ->
    for i = 1 to 100 do
      Shutdown.don't_finish_before (after (Time.Span.of_ms (Float.of_int i)));
    done;
    shutdown 0;
    never ());
;;

let () =
  add_test _here_ Expect.error (fun () ->
    for i = 1 to 100 do
      Shutdown.don't_finish_before (after (Time.Span.of_ms (Float.of_int i)));
    done;
    shutdown 0 ~force:(after (sec 0.01));
    never ());
;;

let () =
  add_test _here_ Expect.ok (fun () ->
    for _i = 1 to 1_000 do
      Shutdown.don't_finish_before Deferred.unit;
    done;
    shutdown 0;
    never ());
;;
