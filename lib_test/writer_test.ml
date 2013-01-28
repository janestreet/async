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
;;

let write_lots writer =
  let ss = String.make 4096 's' in
  for _i = 1 to 64 do
    Writer.write writer ss
  done

let buffer_age_limit () =
  Unix.pipe (Info.of_string "buffer_age_limit")
  >>= function (`Reader reader_fd, `Writer writer_fd) ->
    let writer_to_close = ref None in
    try_with (fun () ->
      let writer =
        Writer.create writer_fd ~buffer_age_limit:(`At_most (sec 2.))
      in
      writer_to_close := Some writer;
      write_lots writer;
      after (sec 5.)
      >>| fun () ->
        `Did_not_raise_error)
  >>= function
    | Ok `Did_not_raise_error -> failwith "buffer-age check did not fire"
    | Error _ ->
      (* buffer-age check fired correctly *)
      Writer.close ~force_close:(return ()) (Option.value_exn !writer_to_close)
  >>= fun () ->
    Unix.Fd.close reader_fd
;;

let reduce_buffer_age_limit () =
  Unix.pipe (Info.of_string "buffer_age_limit")
  >>= function (`Reader reader_fd, `Writer writer_fd) ->
    let writer_to_close = ref None in
    try_with (fun () ->
      let writer =
        Writer.create writer_fd ~buffer_age_limit:(`At_most (sec 180.))
      in
      writer_to_close := Some writer;
      Writer.set_buffer_age_limit writer (`At_most (sec 1.));
      write_lots writer;
      after (sec 2.)
      >>| fun () ->
      `Did_not_raise_error)
  >>= function
  | Ok `Did_not_raise_error -> failwith "buffer-age check did not fire"
  | Error _ ->
    (* buffer-age check fired correctly *)
    Writer.close ~force_close:(return ()) (Option.value_exn !writer_to_close)
    >>= fun () ->
    Unix.Fd.close reader_fd;
;;

let increase_buffer_age_limit () =
  Unix.pipe (Info.of_string "buffer_age_limit")
  >>= function (`Reader reader_fd, `Writer writer_fd) ->
    let writer =
      Writer.create writer_fd ~buffer_age_limit:(`At_most (sec 2.))
    in
    Writer.set_buffer_age_limit writer (`At_most (sec 180.));
    write_lots writer;
    after (sec 5.)
  >>= fun () ->
    Writer.close ~force_close:(return ()) writer
  >>= fun () ->
    Unix.Fd.close reader_fd

let flush_on_close () =
  let module Debug = Async_core.Debug in
  let file = "flush_on_close.txt" in
  Writer.open_file file
  >>= fun writer ->
  let pipe_r, pipe_w = Pipe.create () in
  let buffer = Buffer.create 1 in
  let write s =
    Buffer.add_string buffer s;
    Pipe.write_without_pushback pipe_w s;
  in
  don't_wait_for (Writer.transfer writer (Pipe.map pipe_r ~f:Fn.id)
                    (fun s -> Writer.write writer s));
  write "hello\n";
  Pipe.downstream_flushed pipe_w
  >>= function
  | `Reader_closed -> assert false
  | `Ok ->
    write "goodbye\n";
    Writer.close writer
    >>= fun () ->
    Reader.file_contents file
    >>= fun s ->
    assert (s = Buffer.contents buffer);
    Unix.unlink file;
;;

let schedule_non_zero_pos () =
  let file = "schedule-non-zero-pos.txt" in
  Writer.with_file file ~f:(fun writer ->
    let buf = Bigstring.create 2 in
    Bigstring.set buf 1 '$';
    Writer.schedule_bigstring writer buf ~pos:1 ~len:1;
    return ())
  >>= fun () ->
  Reader.with_file file ~f:Reader.contents
  >>= fun contents ->
  assert_string_equal contents "$";
  Unix.unlink file
;;

let stdout () =
  Core.Std.printf "not from writer 1\n";
  return ()

let tests = [
  "Writer_test.10 writers", multiple_writers 10;
  "Writer_test.100 writers", multiple_writers 100;
  "Writer_test.500 writers", multiple_writers 500;
  "Writer_test.append", append 1000;
  "Writer_test.buffer_age_limit", buffer_age_limit;
  "Writer_test.flush on close", flush_on_close;
  "Writer_test.increase_buffer_age_limit", increase_buffer_age_limit;
  "Writer_test.reduce_buffer_age_limit", reduce_buffer_age_limit;
  "Writer_test.schedule_with_nonzero_pos", schedule_non_zero_pos;
  "Writer_test.stdout", stdout;
  "Writer_test.write", write;
]
