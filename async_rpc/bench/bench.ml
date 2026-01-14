open Core
open Async

module%bench [@name "low latency transport"] _ = struct
  let config =
    Rpc.Low_latency_transport.Config.create
      ~buffering_threshold_in_bytes:0
        (* Never automatically buffer, we want to make sure we are only buffering messages
           because the reader is behind. *)
      ~write_timeout:(Time_ns.Span.of_min 20.)
        (* Once a benchmark finishes running, the write buffer is still nonempty so the
           writer still attempts to wait until the file descriptor can be written to
           without blocking. This times out since the reader is no longer reading, so we
           just make the timeout long enough that the entire run finishes running before
           the error message prints *)
      ()
  ;;

  let max_message_size = 32 * 1024

  let setup () =
    let%map `Reader read_fd, `Writer write_fd =
      Unix.pipe (Info.of_string "low-latency-transport")
    in
    let reader =
      Rpc.Low_latency_transport.Reader.With_internal_reader.create
        ~config
        ~max_message_size
        read_fd
    in
    let writer =
      Rpc.Low_latency_transport.Writer.create ~config ~max_message_size write_fd
    in
    reader, writer
  ;;

  let data =
    let enough_room_for_headers = 40 in
    Base.Sys.opaque_identity
      (String.init (max_message_size - enough_room_for_headers) ~f:(fun (_ : int) -> 'a'))
  ;;

  let write_bin_prot_string writer s =
    let send_result =
      Rpc.Low_latency_transport.Writer.send_bin_prot writer String.bin_writer_t s
    in
    match send_result with
    | Sent _ -> ()
    | Closed | Message_too_big _ -> failwith "Writer failed to write"
  ;;

  let read_one_message_bin_prot reader =
    let%map result =
      Rpc.Low_latency_transport.Reader.With_internal_reader
      .read_one_message_bin_prot_without_buffering
        reader
        String.bin_reader_t
    in
    match result with
    | Ok (_ : string) ->
      (* Don't bother timing string equality. *)
      ()
    | Error (_ : [ `Eof | `Closed ]) -> failwith "Reader failed to read"
  ;;

  let different_powers_of_2_for_testing =
    List.map [ 1; 5; 7; 8; 9; 10; 12 ] ~f:(fun power -> 1 lsl power)
  ;;

  let%bench_fun ("variable pre-writing"
    [@indexed num_messages = 0 :: different_powers_of_2_for_testing])
    =
    let reader, writer = Thread_safe.block_on_async_exn setup in
    let (_ : unit list) =
      List.init num_messages ~f:(fun (_ : int) -> write_bin_prot_string writer data)
    in
    fun () ->
      write_bin_prot_string writer data;
      Thread_safe.block_on_async_exn (fun () -> read_one_message_bin_prot reader)
  ;;

  let%bench_fun "pre-write lots of data for perf" =
    let reader, writer = Thread_safe.block_on_async_exn setup in
    let (_ : unit list) =
      List.init (1 lsl 12) ~f:(fun (_ : int) -> write_bin_prot_string writer data)
    in
    fun () ->
      write_bin_prot_string writer data;
      Thread_safe.block_on_async_exn (fun () -> read_one_message_bin_prot reader)
  ;;
end
