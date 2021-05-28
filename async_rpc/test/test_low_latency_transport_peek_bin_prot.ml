open! Core
open! Async
open! Import

let%expect_test "peeking a message shouldn't result in read offset to change" =
  let max_message_size = 1024 * 1024 in
  let pipe_r, pipe_w = Core_unix.pipe () in
  let writer_fd = Fd.create Fifo pipe_w (Info.of_string "low-latency-transport-writer") in
  let reader_fd = Fd.create Fifo pipe_r (Info.of_string "low-latency-transport-reader") in
  let writer_transport = Rpc.Low_latency_transport.create ~max_message_size writer_fd in
  let writer = writer_transport.writer in
  let reader_transport =
    Rpc.Low_latency_transport.With_internal_reader.create ~max_message_size reader_fd
  in
  let reader = reader_transport.reader_with_internal_reader in
  let%bind () =
    match Rpc.Low_latency_transport.Writer.send_bin_prot writer bin_writer_char 'a' with
    | Sent () ->
      (match%bind
         Rpc.Low_latency_transport.Reader.With_internal_reader.peek_bin_prot
           reader
           bin_reader_char
       with
       | Error (`Eof | `Closed) -> raise_s [%message "Received EOF while peeking"]
       | Ok msg ->
         printf "peeked message is: %c\n" msg;
         (* read the message now *)
         let%bind () =
           match%bind
             Rpc.Low_latency_transport.Reader.read_one_message_bin_prot
               (Rpc.Low_latency_transport.Reader.With_internal_reader.transport_reader
                  reader)
               bin_reader_char
           with
           | Error (`Eof | `Closed) -> raise_s [%message "Received EOF while reading"]
           | Ok msg ->
             printf "read message is: %c\n" msg;
             return ()
         in
         return ())
    | (Closed | Message_too_big _) as res ->
      raise_s
        [%message
          "write_bin_prot error for low latency transport"
            (res : unit Rpc.Low_latency_transport.Send_result.t)]
  in
  [%expect {|
    peeked message is: a
    read message is: a |}];
  return ()
;;
