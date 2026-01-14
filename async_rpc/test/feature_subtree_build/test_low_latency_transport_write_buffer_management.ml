open! Core
open! Async
open! Async_rpc_test.Import

let max_message_size = 1024 * 1024

(* We want this size (16 KiB) since we want messages (which are 1/4 of the pipe buffer
   size) to be at least a page in size (4 KiB) so buffers are flushed as expected. *)
let target_pipe_buffer_size = 16 * 1024

let default_pipe_buffer_size =
  let%map `Reader (_ : Fd.t), `Writer write_fd =
    Unix.pipe (Info.of_string "default-pipe-buffer-size")
  in
  Linux_extended.Fcntl.fcntl_setpipe_sz
    ~fd:(Fd.file_descr_exn write_fd)
    ~size:target_pipe_buffer_size
;;

type t =
  { reader : Rpc.Low_latency_transport.Reader.With_internal_reader.t
  ; writer : Rpc.Low_latency_transport.Writer.With_internal_writer.t
  ; transport_writer : Rpc.Low_latency_transport.Writer.t
  ; read_fd : Fd.t
  ; pipe_buffer_size : int
  }

let setup config =
  let%bind `Reader read_fd, `Writer write_fd =
    Unix.pipe (Info.of_string "test-low-latency-transport-write-buffer-management")
  in
  let pipe_buffer_size =
    Linux_extended.Fcntl.fcntl_setpipe_sz
      ~fd:(Fd.file_descr_exn write_fd)
      ~size:target_pipe_buffer_size
  in
  let reader =
    Rpc.Low_latency_transport.Reader.With_internal_reader.create
      ~config
      ~max_message_size
      read_fd
  in
  let writer =
    Rpc.Low_latency_transport.Writer.With_internal_writer.create
      ~config
      ~max_message_size
      write_fd
  in
  return
    { reader
    ; writer
    ; transport_writer =
        Rpc.Low_latency_transport.Writer.With_internal_writer.transport_writer writer
    ; read_fd
    ; pipe_buffer_size
    }
;;

let write_bin_prot_string t s =
  Rpc.Low_latency_transport.Writer.send_bin_prot t.transport_writer String.bin_writer_t s
  |> [%globalize: unit Rpc.Low_latency_transport.Send_result.t]
  |> [%sexp_of: unit Rpc.Low_latency_transport.Send_result.t]
  |> print_s
;;

let write_bin_prot_nat0_and_bigstring_like_bin_prot_string t s =
  Rpc.Low_latency_transport.Writer.send_bin_prot_and_bigstring
    t.transport_writer
    Bin_prot.Type_class.bin_writer_nat0
    (Bin_prot.Nat0.of_int (String.length s))
    ~buf:(Bigstring.of_string s)
    ~pos:0
    ~len:(String.length s)
  |> [%globalize: unit Rpc.Low_latency_transport.Send_result.t]
  |> [%sexp_of: unit Rpc.Low_latency_transport.Send_result.t]
  |> print_s
;;

let read_one_message_bin_prot t ~allow_buffering =
  let%map res =
    (if allow_buffering
     then
       Rpc.Low_latency_transport.Reader.read_one_message_bin_prot
         (Rpc.Low_latency_transport.Reader.With_internal_reader.transport_reader t.reader)
     else
       Rpc.Low_latency_transport.Reader.With_internal_reader
       .read_one_message_bin_prot_without_buffering
         t.reader)
      String.bin_reader_t
  in
  match res with
  | Ok s -> s
  | Error (`Eof | `Closed) -> failwith "Reader failed to read"
;;

let random_string n = String.init n ~f:(fun _ -> Random.char ())

let payload_len_from_target_msg_len target_msg_len =
  let target_bin_prot_len = target_msg_len - Rpc.Low_latency_transport.Header.length in
  let bin_prot_header_len =
    (* This is techincally not accurate since the [nat0] also takes up a variable number
       of bytes, but it's close enough and we check anyways *)
    Bin_prot.Size.bin_size_nat0 (Bin_prot.Nat0.of_int target_bin_prot_len)
  in
  let payload_len = target_bin_prot_len - bin_prot_header_len in
  [%test_result: int]
    (String.bin_size_t (random_string payload_len))
    ~expect:target_bin_prot_len;
  payload_len
;;

let%expect_test "fast-write case where buffer only big enough for header" =
  let%bind t =
    setup
      (Rpc.Low_latency_transport.Config.create
         ~initial_buffer_size:Rpc.Low_latency_transport.Header.length
         ~buffering_threshold_in_bytes:0
         ~start_batching_after_num_messages:10
         ())
  in
  let a = random_string 1024 in
  write_bin_prot_string t a;
  [%expect {| (Sent (result ()) (bytes 1027)) |}];
  let%bind a_read = read_one_message_bin_prot t ~allow_buffering:false in
  [%test_result: string] a_read ~expect:a;
  return ()
;;

module%test [@name "write buffer tests dependent on amortized compaction behavior"] _ =
struct
  let test_expected_bytes_written_and_buffered t ~here ~written ~buffered =
    [%test_result: Int63.t]
      ~here:[ here ]
      (Rpc.Low_latency_transport.Writer.bytes_written t.transport_writer)
      ~expect:(Int63.of_int written);
    [%test_result: int]
      ~here:[ here ]
      (Rpc.Low_latency_transport.Writer.bytes_to_write t.transport_writer)
      ~expect:buffered
  ;;

  let%expect_test "writev2 works when writing from middle of buffer" =
    (* This test works by carefully managing both the unix pipe's buffer and the transport
       writer's buffer. The pipe buffer is 64 KiB, so all writes until that limit will
       succeed, even if the reader is not reading yet.

       The idea is to fill that pipe buffer so messages are buffered on the transport
       writer's buffer. We then read some data off the pipe and trigger a partial flush of
       the transport writer's buffer so [flushed_pos] is non-zero. We then read all the
       data off the pipe and trigger another flush, which will now write from a nonzero
       [flushed_pos]

       We need to perform the reads off the pipe synchronously, since otherwise there is
       an an async job that flushes the write buffer automatically, without using
       [writev2] *)
    let%bind default_pipe_buffer_size in
    (* We want the write buffer to be large enough to not deal with resizing and to never
       buffer if we can still write to the pipe *)
    let%bind t =
      setup
        (Rpc.Low_latency_transport.Config.create
           ~initial_buffer_size:(default_pipe_buffer_size * 4)
           ~buffering_threshold_in_bytes:0
           ())
    in
    let msg_len = t.pipe_buffer_size / 4 in
    (* Payload message length that will make sure the overall messages + header are
       exactly 1/4 the size of the pipe buffer *)
    let payload_len = payload_len_from_target_msg_len msg_len in
    (* We first fill the pipe buffer with 4 messages, as well as having another message
       buffered in the transport writer *)
    let num_messages_to_read_from_fd = 5 in
    let strings_to_read_from_fd =
      List.init num_messages_to_read_from_fd ~f:(fun _ -> random_string payload_len)
    in
    List.iter strings_to_read_from_fd ~f:(fun s -> write_bin_prot_string t s);
    [%expect
      {|
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:t.pipe_buffer_size
      ~buffered:msg_len;
    let num_messages_to_read_from_transport = 4 in
    let strings_to_read_from_transport =
      List.init num_messages_to_read_from_transport ~f:(fun _ ->
        random_string payload_len)
    in
    (* The next message we write will also get buffered since nothing has been read yet. *)
    write_bin_prot_string t (List.nth_exn strings_to_read_from_transport 0);
    [%expect {| (Sent (result ()) (bytes 4088)) |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:t.pipe_buffer_size
      ~buffered:(msg_len * 2);
    (* We read 1 message from the pipe directly. This allows an additional message to be
       flushed from the write buffer. *)
    let buf = Bigstring.create (msg_len * num_messages_to_read_from_fd) in
    let read_len_1_message =
      Bigstring_unix.read (Fd.file_descr_exn t.read_fd) ~len:msg_len buf
    in
    [%test_result: int] read_len_1_message ~expect:msg_len;
    (* To flush a message, we need to perform another write. The pipe buffer should be
       full now, and there should be two messages buffered in the transport writer. *)
    write_bin_prot_string t (List.nth_exn strings_to_read_from_transport 1);
    [%expect {| (Sent (result ()) (bytes 4088)) |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:(t.pipe_buffer_size + msg_len)
      ~buffered:(msg_len * 2);
    (* The part of the buffer containing the message that was just flushed should now be
       wasted. Compaction shouldn't have happened since we are not wasting at least half
       the buffer yet. *)
    [%test_result: int]
      (Rpc.Low_latency_transport.Writer.With_internal_writer.For_testing
       .buffer_flushed_pos
         t.writer)
      ~expect:msg_len;
    (* Read the remaining messages from the pipe *)
    let read_len_4_messages =
      Bigstring_unix.read
        (Fd.file_descr_exn t.read_fd)
        ~pos:read_len_1_message
        ~len:(msg_len * (num_messages_to_read_from_fd - 1))
        buf
    in
    [%test_result: int] read_len_4_messages ~expect:(msg_len * 4);
    (* There are now 5 messages in the buffer corresponding to [strings_to_read_from_fd].
       We check that the messages are received correctly. *)
    List.iteri strings_to_read_from_fd ~f:(fun i s ->
      let str =
        String.bin_read_t
          buf
          ~pos_ref:
            (ref
               ((* We need to skip over the low-latency transport header *)
                Rpc.Low_latency_transport.Header.length
                + (i * msg_len)))
      in
      [%test_result: string] str ~expect:s);
    (* This next write will flush the buffer when [flushed_pos] is non-zero. We also want
       to write another message to ensure we didn't send any garbage after the flush from
       the middle *)
    write_bin_prot_string t (List.nth_exn strings_to_read_from_transport 2);
    write_bin_prot_string t (List.nth_exn strings_to_read_from_transport 3);
    [%expect
      {|
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:
        ((num_messages_to_read_from_fd + num_messages_to_read_from_transport) * msg_len)
      ~buffered:0;
    let%bind () = Rpc.Low_latency_transport.Writer.flushed t.transport_writer in
    (* Read the remaining messages from the transport and check they match the expected *)
    let%bind () =
      Deferred.List.iter strings_to_read_from_transport ~how:`Sequential ~f:(fun s ->
        read_one_message_bin_prot t ~allow_buffering:true
        >>| [%test_result: string] ~expect:s)
    in
    return ()
  ;;

  let%expect_test "Test partial write from bigstring in writev2" =
    let%bind default_pipe_buffer_size in
    let%bind t =
      setup
        (Rpc.Low_latency_transport.Config.create
           ~initial_buffer_size:(default_pipe_buffer_size * 4)
           ~buffering_threshold_in_bytes:0
           ())
    in
    let msg_len = t.pipe_buffer_size / 4 in
    (* Payload message length that will make sure the overall messages + header are
       exactly 1/4 the size of the pipe buffer *)
    let payload_len = payload_len_from_target_msg_len msg_len in
    (* We want the write buffer to be large enough to not deal with resizing and to never
       buffer if we can still write to the pipe *)
    (* We first fill the pipe buffer with 4 messages, as well as having another message
       buffered in the transport writer *)
    let num_messages_to_read_from_fd = 5 in
    let strings_to_read_from_fd =
      List.init num_messages_to_read_from_fd ~f:(fun _ -> random_string payload_len)
    in
    List.iter strings_to_read_from_fd ~f:(fun s ->
      write_bin_prot_nat0_and_bigstring_like_bin_prot_string t s);
    [%expect
      {|
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:t.pipe_buffer_size
      ~buffered:msg_len;
    let num_messages_to_buffer = 2 in
    let strings_to_buffer =
      List.init num_messages_to_buffer ~f:(fun _ -> random_string payload_len)
    in
    (* The next message we write will also get buffered since nothing has been read yet. *)
    write_bin_prot_nat0_and_bigstring_like_bin_prot_string
      t
      (List.nth_exn strings_to_buffer 0);
    [%expect {| (Sent (result ()) (bytes 4088)) |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:t.pipe_buffer_size
      ~buffered:(msg_len * 2);
    (* We read 1 message from the pipe directly. This allows an additional message to be
       flushed from the write buffer. *)
    let buf = Bigstring.create (msg_len * num_messages_to_read_from_fd) in
    let read_len_1_message =
      Bigstring_unix.read (Fd.file_descr_exn t.read_fd) ~len:msg_len buf
    in
    [%test_result: int] read_len_1_message ~expect:msg_len;
    (* To flush a message, we need to perform another write. The pipe buffer should be
       full now, and there should be two messages buffered in the transport writer. *)
    write_bin_prot_nat0_and_bigstring_like_bin_prot_string
      t
      (List.nth_exn strings_to_buffer 1);
    [%expect {| (Sent (result ()) (bytes 4088)) |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:(t.pipe_buffer_size + msg_len)
      ~buffered:(msg_len * 2);
    (* The part of the buffer containing the message that was just flushed should now be
       wasted. Compaction shouldn't have happened since we are not wasting at least half
       the buffer yet. *)
    [%test_result: int]
      (Rpc.Low_latency_transport.Writer.With_internal_writer.For_testing
       .buffer_flushed_pos
         t.writer)
      ~expect:msg_len;
    (* Read the remaining messages from the pipe *)
    let read_len_4_messages =
      Bigstring_unix.read
        (Fd.file_descr_exn t.read_fd)
        ~pos:read_len_1_message
        ~len:(msg_len * (num_messages_to_read_from_fd - 1))
        buf
    in
    [%test_result: int] read_len_4_messages ~expect:(msg_len * 4);
    (* There are now 5 messages in the buffer corresponding to [strings_to_read_from_fd].
       We check that the messages are received correctly. *)
    List.iteri strings_to_read_from_fd ~f:(fun i s ->
      let str =
        String.bin_read_t
          buf
          ~pos_ref:
            (ref
               ((* We need to skip over the low-latency transport header *)
                Rpc.Low_latency_transport.Header.length
                + (i * msg_len)))
      in
      [%test_result: string] str ~expect:s);
    (* This next write will flush the buffer when [flushed_pos] is non-zero. We also want
       to flush with a message that will be partially written out, so we write one that's
       as large as the pipe buffer. *)
    let string_to_partially_flush =
      random_string (payload_len_from_target_msg_len t.pipe_buffer_size)
    in
    write_bin_prot_nat0_and_bigstring_like_bin_prot_string t string_to_partially_flush;
    [%expect {| (Sent (result ()) (bytes 16376)) |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:((num_messages_to_read_from_fd * msg_len) + t.pipe_buffer_size)
      ~buffered:(t.pipe_buffer_size - (num_messages_to_buffer * msg_len));
    (* Read the strings from from the transport and check they match the expected *)
    let%bind () =
      Deferred.List.iter strings_to_buffer ~how:`Sequential ~f:(fun s ->
        read_one_message_bin_prot t ~allow_buffering:true
        >>| [%test_result: string] ~expect:s)
    in
    let%bind () = Rpc.Low_latency_transport.Writer.flushed t.transport_writer in
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:
        (((num_messages_to_read_from_fd + num_messages_to_buffer) * msg_len)
         + t.pipe_buffer_size)
      ~buffered:0;
    let%bind () =
      read_one_message_bin_prot t ~allow_buffering:true
      >>| [%test_result: string] ~expect:string_to_partially_flush
    in
    return ()
  ;;

  let%expect_test "Test buffer growing when enough space exists after compaction" =
    let%bind default_pipe_buffer_size in
    (* Both the pipe buffer and initial write buffer can hold 4 messages *)
    let%bind t =
      setup
        (Rpc.Low_latency_transport.Config.create
           ~initial_buffer_size:default_pipe_buffer_size
           ~buffering_threshold_in_bytes:0
           ())
    in
    let msg_len = t.pipe_buffer_size / 4 in
    (* Payload message length that will make sure the overall messages + header are
       exactly 1/4 the size of the pipe buffer *)
    let payload_len = payload_len_from_target_msg_len msg_len in
    let strings_to_fill_buffers = List.init 7 ~f:(fun _ -> random_string payload_len) in
    List.iter strings_to_fill_buffers ~f:(fun s -> write_bin_prot_string t s);
    [%expect
      {|
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      (Sent (result ()) (bytes 4088))
      |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:(msg_len * 4)
      ~buffered:(msg_len * 3);
    (* The pipe buffer has 4 messages buffered and the write buffer has 3. We now read one
       message off the pipe and partially flush the buffer by writing another message *)
    let%bind () =
      read_one_message_bin_prot t ~allow_buffering:false
      >>| [%test_result: string] ~expect:(List.hd_exn strings_to_fill_buffers)
    in
    let payload_to_flush = random_string payload_len in
    write_bin_prot_string t payload_to_flush;
    [%expect {| (Sent (result ()) (bytes 4088)) |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:(msg_len * 5)
      ~buffered:(msg_len * 3);
    [%test_result: int]
      (Rpc.Low_latency_transport.Writer.With_internal_writer.For_testing
       .buffer_flushed_pos
         t.writer)
      ~expect:msg_len;
    (* The write buffer now has 3 messages buffered, and the empty space for the fourth
       message is at the front of the buffer. We now write another message, which should
       grow the buffer to the next power of 2 *)
    let payload_to_resize = random_string payload_len in
    write_bin_prot_string t payload_to_resize;
    [%expect {| (Sent (result ()) (bytes 4088)) |}];
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:(msg_len * 5)
      ~buffered:(msg_len * 4);
    [%test_result: int]
      (Rpc.Low_latency_transport.Writer.With_internal_writer.For_testing.buffer_capacity
         t.writer)
      ~expect:(t.pipe_buffer_size * 2);
    (* Read off all the strings and check they were delivered properly *)
    let to_read =
      List.tl_exn strings_to_fill_buffers @ [ payload_to_flush; payload_to_resize ]
    in
    let%bind () =
      Deferred.List.iter to_read ~how:`Sequential ~f:(fun s ->
        read_one_message_bin_prot t ~allow_buffering:true
        >>| [%test_result: string] ~expect:s)
    in
    test_expected_bytes_written_and_buffered
      t
      ~here:[%here]
      ~written:(msg_len * 9)
      ~buffered:0;
    let%bind () = Rpc.Low_latency_transport.Writer.flushed t.transport_writer in
    return ()
  ;;
end
