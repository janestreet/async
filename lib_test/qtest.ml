(** Regression test runner. *)

open Core.Std;;
open Qtest_lib.Std;;

let tests =
  []
  @ Bind_test.tests
  @ Finalizer_test.tests
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
;;

let () = Runner.main tests

