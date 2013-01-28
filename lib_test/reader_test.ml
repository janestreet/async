open Core.Std
open Qtest_lib.Std
open Async.Std

exception Unexpected_sexps of Sexp.t list with sexp

let test_sexps reader =
  let sexps = Reader.read_sexps reader in
  Pipe.to_list sexps
  >>| fun sexps ->
  let last = List.hd_exn (List.rev sexps) in
  let last = Sexp.to_string last in
  assert_string_equal "(last sexp)" last

let read_sexps_file () =
  Reader.with_file "reader_test.sexp" ~f:(fun reader ->
    test_sexps reader)

let read_sexps_pipe () =
  Reader.with_file "reader_test.sexp" ~f:(fun reader ->
    let sexps = Reader.read_sexps reader in
    Unix.pipe ()
    >>= function (`Reader reader_fd, `Writer writer_fd) ->
    let writer = Writer.create writer_fd in
    Pipe.iter sexps ~f:(fun sexp -> Writer.write_sexp writer sexp; Writer.flushed writer)
    >>= fun () ->
    Writer.close writer
    >>= fun () ->
    let reader = Reader.create reader_fd in
    test_sexps reader)

let tests = [
  "Reader_test.read_sexps_file", read_sexps_file;
  "Reader_test.read_sexps_pipe", read_sexps_pipe;
]
