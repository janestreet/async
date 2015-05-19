open Core.Std
open Async.Std

let test () =
  Process.run ~prog:"../example/dynamic_port_writer.exe" ~args:["parent"] ()
  >>= fun output ->
  let _ : string = Or_error.ok_exn output in
  Deferred.unit

let tests = [
  ("Dynamic_port_writer_test", test);
]
