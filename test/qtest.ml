(** Regression test runner. *)

open Core.Std;;
open Async.Std
open Qtest_lib.Std;;

let tests =
  []
  @ Bind_test.tests
  @ Busy_poll_test.tests
  @ Clock_test.tests
  @ Fd_test.tests
  @ Finalizer_test.tests
  @ In_thread_test.tests
  @ Quickcheck_clock_test.tests
  @ Log_test.tests
  @ Process_test.tests
  @ Reader_test.tests
  @ Ready_to_test.tests
  @ Rpc_test.tests
  @ Socket_test.tests
  @ Tcp_file_test.tests
  @ Tcp_serve.tests
  @ Test_handler.tests
  @ Thread_safe_test.tests
  @ Wait_test.tests
  @ Writer_test.tests
  @ Dynamic_port_writer_test.tests
;;

let () =
  (* this test takes roughly 40s alone, so 25min on something that compiles
     with -j 12 should be ok *)
  after (Time.Span.of_min 25.)
  >>> fun () ->
  eprintf "Shutting down test after a 25min timeout\n%!";
  Shutdown.shutdown 3
;;

let () = Runner.main tests
