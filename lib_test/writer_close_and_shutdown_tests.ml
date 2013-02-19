open Core.Std
open Async.Std
open Async_test_in_child_process

let tmp_file = "/tmp/z.file"

let () =
  for power_of_two = 0 to 20 do
    let contents () =
      String.init (Float.to_int (Float.ldexp 1. power_of_two)) ~f:(fun i ->
        Char.of_int_exn (Int.rem i 256))
    in
    let expect =
      Expect.(ok && no_output && custom "correct contents" (fun () ->
        Reader.file_contents tmp_file
        >>= fun contents' ->
        Unix.unlink tmp_file
        >>| fun () ->
        if contents' = contents () then
          Ok ()
        else
          Or_error.error_string "contents mismatch"))
    in
    add_test _here_ expect (fun () ->
      Writer.open_file tmp_file
      >>= fun writer ->
      Writer.write writer (contents ());
      Deferred.unit)
  done
;;

let () =
  let string1 = "hello\n" in
  let string2 = "goodbye\n" in
  let expect =
    Expect.(ok && no_output && custom "correct contents" (fun () ->
      Reader.file_contents tmp_file
      >>= fun contents' ->
      Unix.unlink tmp_file
      >>| fun () ->
      if contents' = string1 ^ string2 then
        Ok ()
      else
        Or_error.error_string "contents mismatch"))
  in
  add_test _here_ expect (fun () ->
    Writer.open_file tmp_file
    >>= fun writer ->
    Shutdown.at_shutdown (fun () ->
      Writer.write writer string2;
      Writer.flushed writer);
    Writer.write writer string1;
    Deferred.unit);
;;

let () =
  let string1 = "hello\n" in
  let string2 = "goodbye\n" in
  let expect =
    Expect.(ok && no_output && custom "correct contents" (fun () ->
      Reader.file_contents tmp_file
      >>= fun contents' ->
      Unix.unlink tmp_file
      >>| fun () ->
      if contents' = string1 ^ string2 then
        Ok ()
      else
        Or_error.error_string "contents mismatch"))
  in
  add_test _here_ expect (fun () ->
    Writer.open_file tmp_file
    >>= fun writer ->
    Shutdown.at_shutdown (fun () ->
      Writer.write writer string2;
      Writer.close writer);
    Writer.write writer string1;
    Deferred.unit);
;;

let () = never_returns (main ())
