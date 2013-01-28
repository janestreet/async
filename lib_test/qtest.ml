(** Regression test runner. *)

open Core.Std;;
open Qtest_lib.Std;;

let tests =
  []
  @ Bind_test.tests
  @ Finalizer_test.tests
  @ Reader_test.tests
  @ Rpc_canary_test.tests
  @ Rpc_test.tests
  @ Tcp_serve.tests
  @ Test_handler.tests
  @ Thread_safe_test.tests
  @ Unpack_sequence_test.tests
  @ Wait_test.tests
  @ Writer_test.tests
;;

let () =
  let module Runner = Qtest_lib.Std.Runner.Make(Version_util) in
  Runner.main tests
