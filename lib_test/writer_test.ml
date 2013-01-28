open Core.Std
open Qtest_lib.Std
open Async.Std

let write () =
  let file = "tmp_writer_test.txt" in
  Writer.open_file file
  >>= fun writer ->
    Writer.write writer "abc\n";
    Writer.write writer "def\n";
    Writer.write writer "ghi\n";
    Writer.close writer
  >>= fun () ->
    Reader.with_file file ~f:Reader.contents
  >>= fun contents ->
    assert_string_equal contents "abc\ndef\nghi\n";
    Unix.unlink file

let multiple_writers n () =
  let files = Array.init n ~f:(fun i ->
    sprintf "writer_test%d.txt" i)
  in
  let files = Array.to_list files in
  let writers = List.map ~f:Writer.open_file files in
  Deferred.all writers
  >>= fun writers ->
    for i = 1 to 10
    do
      List.iter writers ~f:(fun writer ->
        Writer.writef writer "line %d\n" i)
    done;
    Deferred.all_unit (List.map ~f:Writer.close writers)
  >>= fun () ->
    Deferred.all_unit (List.map ~f:Unix.unlink files)

let append max () =
  let file = "tmp_writer_test_append.txt" in
  Writer.save file ~contents:""
  >>= fun () ->
  let append text =
    Writer.with_file file ~append:true ~f:(fun writer ->
      Writer.write writer text;
      Deferred.unit)
  in
  Deferred.create (fun ivar ->
    let rec write i =
      append (Int.to_string i ^ "\n")
      >>> fun () ->
        if i < max
        then write (i + 1)
        else Ivar.fill ivar ()
    in
    write 1)
  >>= fun () ->
    Reader.open_file file
  >>= fun reader ->
    let lines = Reader.lines reader in
    let expected = ref 1 in
    Pipe.iter' lines ~f:(fun lines ->
      Queue.iter lines ~f:(fun line ->
        assert_string_equal (Int.to_string !expected) line;
        incr expected);
      Deferred.unit)
  >>= fun () ->
    assert_string_equal (Int.to_string !expected) (Int.to_string (max + 1));
    Reader.close reader
  >>= fun () ->
    Unix.unlink file

let tests = [
  "Writer_test.write", write;
  "Writer_test.10 writers", multiple_writers 10;
  "Writer_test.100 writers", multiple_writers 100;
  "Writer_test.1000 writers", multiple_writers 1000;
  "Writer_test.append", append 1000;
]
