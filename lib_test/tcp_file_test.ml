open Core.Std
open Qtest_lib.Std
open Async.Std

let serve_existing_static_file () =
  Utils.probably_unused_port ()
  >>= fun port ->
  Tcp_file.Server.serve ~auth:(fun _ -> true) ~port
  >>= fun (server : Tcp.Server.inet) ->
  let file = (Filename.dirname Sys.executable_name) ^/ "tcp_file_test.ml" in
  Tcp_file.Server.serve_existing_static_file file
  >>= fun () ->
  Tcp.Server.close server

let tests = [
  "Tcp_file_test.serve_existing_static_file", serve_existing_static_file;
]
