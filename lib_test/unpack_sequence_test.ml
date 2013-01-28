open Jane.Std
open Qtest_lib.Std
open Async.Std

let sexps =
  let a x = Sexp.Atom x in
  let l x = Sexp.List x in
  [ a "";
    a "hello";
    l [];
    l [ a "" ];
    l [ a ""; a "hello" ];
  ]
;;

let test () =
  Unix.pipe ()
  >>= fun (`Reader reader_fd, `Writer writer_fd) ->
  let string_writer = Writer.create writer_fd in
  let (sexp_reader, unpack_result) =
    Unpack_sequence.unpack_from_reader
      (Unpack_buffer.create Wire.unpack_sexp)
      (Reader.create reader_fd)
  in
  (* write all the sexps at a single go. *)
  List.iter sexps ~f:(fun sexp ->
    Writer.write string_writer (Wire.to_string sexp));
  (* write all the sexps, one ... character ... at ... a ... time *)
  let rec loop_sexps sexps =
    match sexps with
    | [] -> whenever (Writer.close string_writer)
    | sexp :: sexps ->
      let packed = Wire.to_string sexp in
      let rec loop_bytes i =
        if i = String.length packed then
          loop_sexps sexps
        else begin
          Writer.write string_writer (String.sub packed ~pos:i ~len:1);
          after (sec 0.001)
          >>> fun () ->
          loop_bytes (i + 1)
        end
      in
      loop_bytes 0
  in
  loop_sexps sexps;
  let all = Pipe.read_all sexp_reader in
  unpack_result
  >>= fun result ->
  let module R = Unpack_sequence.Result in
  begin match result with
  | R.Input_closed -> ()
  | r -> Error.raise (R.to_error r)
  end;
  all
  >>| fun all ->
  assert (sexps @ sexps = Queue.to_list all);
;;

let tests =
  [
    "Unpack_Sequence_test", test;
  ]
;;
