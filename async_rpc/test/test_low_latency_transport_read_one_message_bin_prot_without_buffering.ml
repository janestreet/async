open! Core
open! Async
open! Import

let max_message_size = 1024 * 1024

type t =
  { reader : Rpc.Low_latency_transport.Reader.With_internal_reader.t
  ; writer : Rpc.Low_latency_transport.Writer.t
  ; async_reader : Reader.t
  }

let setup () =
  let%bind `Reader read_fd, `Writer write_fd =
    Unix.pipe (Info.of_string "test-low-latency-transport-disallow-large-reads")
  in
  let reader =
    Rpc.Low_latency_transport.Reader.With_internal_reader.create ~max_message_size read_fd
  in
  let writer = Rpc.Low_latency_transport.Writer.create ~max_message_size write_fd in
  let async_reader = Reader.create read_fd in
  return { reader; writer; async_reader }
;;

let write_bin_prot_int t i =
  Rpc.Low_latency_transport.Writer.send_bin_prot t.writer Int.bin_writer_t i
  |> [%globalize: unit Rpc.Low_latency_transport.Send_result.t]
  |> [%sexp_of: unit Rpc.Low_latency_transport.Send_result.t]
  |> print_s
;;

let read_one_message_bin_prot t ~allow_buffering =
  (if allow_buffering
   then
     Rpc.Low_latency_transport.Reader.read_one_message_bin_prot
       (Rpc.Low_latency_transport.Reader.With_internal_reader.transport_reader t.reader)
   else
     Rpc.Low_latency_transport.Reader.With_internal_reader
     .read_one_message_bin_prot_without_buffering
       t.reader)
    Int.bin_reader_t
  >>| [%sexp_of: (int, [ `Closed | `Eof ]) Result.t]
  >>| print_s
;;

let read_one_message_bin_prot_from_fd t =
  Reader.read_bin_prot t.async_reader Int.bin_reader_t
  >>| [%sexp_of: int Reader.Read_result.t]
  >>| print_s
;;

let%expect_test "default (buffering)" =
  let%bind t = setup () in
  write_bin_prot_int t 1;
  [%expect {| (Sent (result ()) (bytes 1)) |}];
  write_bin_prot_int t 2;
  [%expect {| (Sent (result ()) (bytes 1)) |}];
  (* The default behavior (buffering) is for the reader to fill it's internal buffer. In
     this case that means reading the messages for both 1 and 2. *)
  let%bind () = read_one_message_bin_prot t ~allow_buffering:true in
  [%expect {| (Ok 1) |}];
  write_bin_prot_int t 3;
  [%expect {| (Sent (result ()) (bytes 1)) |}];
  (* The 2 message is skipped because it is sitting in the reader's internal buffer *)
  let%bind () = read_one_message_bin_prot_from_fd t in
  [%expect {| (Ok 3) |}];
  return ()
;;

let%expect_test "disallow buffering" =
  let%bind t = setup () in
  write_bin_prot_int t 1;
  [%expect {| (Sent (result ()) (bytes 1)) |}];
  write_bin_prot_int t 2;
  [%expect {| (Sent (result ()) (bytes 1)) |}];
  (* With buffering disabled the reader will only read the 1 message. *)
  let%bind () = read_one_message_bin_prot t ~allow_buffering:false in
  [%expect {| (Ok 1) |}];
  write_bin_prot_int t 3;
  [%expect {| (Sent (result ()) (bytes 1)) |}];
  (* Both the 2 and 3 messages can be read from the file descriptor *)
  let%bind () = read_one_message_bin_prot_from_fd t in
  [%expect {| (Ok 2) |}];
  let%bind () = read_one_message_bin_prot_from_fd t in
  [%expect {| (Ok 3) |}];
  return ()
;;
