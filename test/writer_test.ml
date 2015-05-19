open Core.Std
open Qtest_lib.Std
open Async.Std

let concat = String.concat

let tests = ref []

let add_test name { Lexing. pos_fname; pos_lnum; _ } f =
  let name =
    concat [ Filename.basename pos_fname
           ; ":"; Int.to_string pos_lnum
           ; " "; name
           ]
  in
  tests := (name, f) :: !tests
;;

let write = add_test "write" _here_ (fun () ->
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
  Unix.unlink file)
;;

let multiple_writers here n =
  add_test (concat [ Int.to_string n; " writers" ]) here (fun () ->
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
    Deferred.all_unit (List.map ~f:Unix.unlink files))
;;

let () = multiple_writers _here_ 10
let () = multiple_writers _here_ 100
let () = multiple_writers _here_ 500

let append = add_test "append" _here_ (fun () ->
  let max = 1000 in
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
  Unix.unlink file)
;;

let write_lots writer =
  let ss = String.make 4096 's' in
  for _i = 1 to 64 do
    Writer.write writer ss
  done
;;

let () = add_test "buffer_age_limit" _here_ (fun () ->
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
      Unix.Fd.close reader_fd)
;;

let () = add_test "reduce_buffer_age_limit" _here_ (fun () ->
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
      Unix.Fd.close reader_fd)
;;

let () = add_test "increase_buffer_age_limit" _here_ (fun () ->
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
    Unix.Fd.close reader_fd)
;;

let () = add_test "flush_on_close" _here_ (fun () ->
  let module Debug = Async_kernel.Debug in
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
    Unix.unlink file)
;;

let () = add_test "schedule_non_zero_pos" _here_ (fun () ->
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
  Unix.unlink file)
;;

let () = add_test "stdout" _here_ (fun () ->
  Core.Std.printf "not from writer 1\n";
  return ())
;;

let transfer_test f =
  let pipe_reader, pipe_writer = Pipe.create () in
  Unix.pipe (Info.create "transfer" () <:sexp_of< unit >>)
  >>= fun (`Reader rfd, `Writer wfd) ->
  let reader = Reader.create rfd in
  let writer = Writer.create wfd ~raise_when_consumer_leaves:false in
  let transfer_finished =
    Writer.transfer writer pipe_reader (fun s -> Writer.write writer s)
  in
  f pipe_reader pipe_writer reader writer;
  transfer_finished
  >>= fun () ->
  Reader.close reader
  >>= fun () ->
  Writer.close writer
;;

let () = add_test "transfer close consumer" _here_ (fun () ->
  transfer_test (fun _pipe_reader pipe_writer reader _writer ->
    don't_wait_for
      (Reader.close reader
       >>| fun () ->
       Pipe.write_without_pushback pipe_writer "hello")))
;;

let () = add_test "transfer close producer" _here_ (fun () ->
  transfer_test (fun _pipe_reader pipe_writer _reader _writer ->
    Pipe.close pipe_writer))
;;

let dfor from to_ ~f =
  let rec loop i =
    if i > to_
    then return ()
    else f i >>= fun () -> loop (i + 1)
  in
  loop from
;;

let () = add_test "write_iobuf" _here_ (fun () ->
  let file = "write_iobuf_test" in
  let iobuf = Iobuf.of_string "hello" in
  dfor (-1) (Iobuf.length iobuf + 1) ~f:(fun pos ->
    dfor (-1) (Iobuf.length iobuf + 1 - pos) ~f:(fun len ->
      let slice_is_ok = pos >= 0 && len >= 0 && pos + len <= Iobuf.length iobuf in
      Writer.with_file file ~f:(fun writer ->
        begin
          try
            Writer.write_iobuf writer iobuf ~pos ~len
          with _ -> assert (not slice_is_ok);
        end;
        return ())
      >>= fun () ->
      Reader.file_contents file
      >>= fun s ->
      <:test_result< string >> s
        ~expect:(if slice_is_ok
                 then Iobuf.to_string (Iobuf.sub_shared iobuf ~pos ~len)
                 else "");
      return ()))
  >>= fun () ->
  Unix.unlink file)
;;

let () = add_test "save_sexps" _here_ (fun () ->
  let values = ["abc",1 ;"def",2 ;"ghi jkl mno",123124] in
  let sexps = List.map values ~f:<:sexp_of<string * int>> in
  let file = "save_sexps" in
  Writer.save_sexps file sexps
  >>= fun () ->
  Reader.load_sexps_exn file <:of_sexp<string * int>>
  >>= fun read_values ->
  Unix.unlink file
  >>| fun () ->
  <:test_eq<(string * int) list>> values read_values)
;;

let () = add_test "write_sexp" _here_ (fun () ->
  (* this tests a special case when writing a single sexp that's not surrounded by
     quotes or parens, in those cases write_sexp must add whitespace after the sexp *)
  let values = [1;2;-3;4;-1;1;123;-1;-5] in
  let sexps = List.map values ~f:Int.sexp_of_t in
  let file = "write_sexp" in
  Writer.with_file file ~f:(fun w ->
    List.iter sexps ~f:(fun s -> Writer.write_sexp w s);
    Deferred.unit)
  >>= fun () ->
  Reader.load_sexps_exn file Int.t_of_sexp
  >>= fun read_values ->
  Unix.unlink file
  >>| fun () ->
  <:test_eq<int list>> values read_values)
;;

let tests = List.rev !tests
