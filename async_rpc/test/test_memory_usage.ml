open! Core
open! Async
open! Import

let rpc =
  Rpc.Rpc.create
    ~name:"memory-test-rpc"
    ~version:1
    ~bin_query:String.bin_t
    ~bin_response:String.bin_t
    ~include_in_error_count:Only_on_exn
;;

let implementations = [ Rpc.Rpc.implement rpc (fun () query -> return query) ]

(* We avoid using [Test_helpers.with_rpc_server_connection] because the "taps" it installs
   for snooping on communication between the client and the server can't handle tens of
   thousands of RPCs, and we actually don't want to buffer any additional state anyway. *)
let with_connection ~f =
  let server_ivar = Ivar.create () in
  let%bind server =
    Rpc.Connection.serve
      ~implementations:
        (Rpc.Implementations.create_exn
           ~implementations
           ~on_unknown_rpc:`Raise
           ~on_exception:Log_on_background_exn)
      ~initial_connection_state:(fun _ conn ->
        Ivar.fill_exn server_ivar conn;
        ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ()
  in
  let port = Tcp.Server.listening_on server in
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port { Host_and_port.host = "localhost"; port }
  in
  let%bind client = Rpc.Connection.client where_to_connect >>| Result.ok_exn in
  let%bind server_conn = Ivar.read server_ivar in
  let%bind result = f ~client ~server:server_conn in
  let%bind () = Rpc.Connection.close client in
  let%bind () = Tcp.Server.close server in
  return result
;;

let get_heap_words () = Gc.heap_words () |> Byte_units.of_words_int

let%expect_test "regression test for memory leak per RPC dispatch" =
  let num_rpcs = 1000 in
  with_connection ~f:(fun ~client ~server:_ ->
    Gc.full_major ();
    let before_heap = get_heap_words () in
    let send_rpc i =
      let query = sprintf "query_%d" i in
      (* We want to ensure that we don't leak memory per RPC dispatch. *)
      let%bind response = Rpc.Rpc.dispatch rpc client query >>| ok_exn in
      [%test_result: string] response ~expect:query;
      (* We additionally want to ensure that peeking at the close reason does not leak
         memory. *)
      let close_started =
        Rpc.Connection.close_reason ~on_close:`started client
        |> Deferred.peek
        |> Option.is_some
      in
      assert (not close_started);
      return ()
    in
    let%bind () = Deferred.for_ 0 ~to_:num_rpcs ~do_:send_rpc in
    Gc.full_major ();
    let after_heap = get_heap_words () in
    let heap_growth = Byte_units.(after_heap - before_heap) in
    [%test_eq: Byte_units.t] heap_growth Byte_units.zero;
    return ())
;;

module%test [@name "pipe-rpc pushback"] _ = struct
  let pipe_rpc ~client_pushes_back =
    Rpc.Pipe_rpc.create
      ?client_pushes_back
      ()
      ~name:"pipe-rpc"
      ~version:1
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: unit]
      ~bin_error:[%bin_type_class: Error.t]
  ;;

  let rec write_n_messages writer = function
    | 0 -> ()
    | n ->
      Pipe.write_without_pushback writer ();
      write_n_messages writer (n - 1)
  ;;

  let max_message_size = 1024 * 1024

  let create_connection_and_dispatch_batches rpc ~make_transport ~is_tcp_transport =
    (* We write [batches] of [messages_per_batch] to the pipe-rpc, with batches separated
       by a [Scheduler.yield ()]. This is to simulate bursts of messages coming through
       from a pipe-rpc implementation. [500] was chosen to try and interact negatively
       with the pipe-rpc default of setting the buffer size to [100] and with the async
       scheduler number of jobs run per cycle.

       We wait until [all_written] to start reading from the client connection to maximize
       pushback on the server connection. *)
    let batches = 500 in
    let messages_per_batch = 500 in
    let all_written = Ivar.create () in
    let server_implementations =
      [ Rpc.Pipe_rpc.implement rpc (fun () () ->
          Pipe.create_reader ~close_on_exception:false (fun writer ->
            Deferred.repeat_until_finished batches (function
              | 0 -> Deferred.return (`Finished ())
              | n ->
                write_n_messages writer messages_per_batch;
                let%bind () = Scheduler.yield () in
                Deferred.return (`Repeat (n - 1)))
            >>| Ivar.fill_exn all_written)
          |> Deferred.Or_error.return)
      ]
    in
    test1
      ~make_transport
      ~make_client_transport:make_transport
      ~on_handshake_error:`Raise
      ~server_state:()
      ~server_implementations
      ~f:(fun ~server_side_connection ~client_side_connection ->
        Async_rpc_kernel.Async_rpc_kernel_private.Util.For_testing
        .number_of_unique_flushed_waits_due_to_streaming_pushback := 0;
        Rpc.Transport.Reader.For_testing
        .number_of_unique_deferreds_to_wait_for_before_reading_next_chunk := 0;
        let%bind pipe, (_ : Rpc.Pipe_rpc.Metadata.t) =
          Rpc.Pipe_rpc.dispatch_exn rpc client_side_connection ()
        in
        let%bind () = Ivar.read all_written in
        let%bind () = Scheduler.yield_until_no_jobs_remain () in
        print_s
          [%message
            ""
              ~number_of_unique_flushed_waits_due_to_streaming_pushback:
                (!Async_rpc_kernel.Async_rpc_kernel_private.Util.For_testing
                  .number_of_unique_flushed_waits_due_to_streaming_pushback
                 : int)];
        if is_tcp_transport
        then
          print_s
            [%message
              "TCP transport reader pushback behavior stats"
                ~number_of_unique_deferreds_to_wait_for_before_reading_next_chunk:
                  (!Rpc.Transport.Reader.For_testing
                    .number_of_unique_deferreds_to_wait_for_before_reading_next_chunk
                   : int)];
        let%bind messages = Pipe.to_list pipe in
        assert (List.length messages = batches * messages_per_batch);
        let%bind () = Async_rpc_kernel.Rpc.Connection.close client_side_connection in
        let%bind () = Async_rpc_kernel.Rpc.Connection.close server_side_connection in
        let%bind () =
          Async_rpc_kernel.Rpc.Connection.close_finished client_side_connection
        in
        let%bind () =
          Async_rpc_kernel.Rpc.Connection.close_finished server_side_connection
        in
        Deferred.unit)
      ()
  ;;

  let%expect_test "pipe transport" =
    let create_connection_and_dispatch_batches_pipe rpc =
      (* We need to create a fresh pair for each test *)
      let client_transport, server_transport =
        Async_rpc_kernel.Pipe_transport.create_pair
          Async_rpc_kernel.Pipe_transport.Kind.string
      in
      (* The test1 function calls make_transport twice: once with (r1, w1) for server,
         once with (r2, w2) for client. We ignore these file descriptors and return our
         pre-created transports *)
      let first_call = ref true in
      let make_transport _ =
        if !first_call
        then (
          first_call := false;
          server_transport)
        else client_transport
      in
      create_connection_and_dispatch_batches rpc ~make_transport
    in
    let%bind () =
      create_connection_and_dispatch_batches_pipe
        (pipe_rpc ~client_pushes_back:None)
        ~is_tcp_transport:false
    in
    [%expect {| (number_of_unique_flushed_waits_due_to_streaming_pushback 0) |}];
    let%bind () =
      create_connection_and_dispatch_batches_pipe
        (pipe_rpc ~client_pushes_back:(Some ()))
        ~is_tcp_transport:false
    in
    [%expect {| (number_of_unique_flushed_waits_due_to_streaming_pushback 1) |}];
    Deferred.unit
  ;;

  let%expect_test "tcp transport" =
    let create_connection_and_dispatch_batches_with_transport rpc =
      let make_transport (fd_r, fd_w) =
        let reader = Reader.create fd_r in
        let writer = Writer.create fd_w in
        Rpc.Transport.of_reader_writer ~max_message_size reader writer
      in
      create_connection_and_dispatch_batches rpc ~make_transport
    in
    let%bind () =
      create_connection_and_dispatch_batches_with_transport
        (pipe_rpc ~client_pushes_back:None)
        ~is_tcp_transport:true
    in
    [%expect
      {|
      (number_of_unique_flushed_waits_due_to_streaming_pushback 0)
      ("TCP transport reader pushback behavior stats"
       (number_of_unique_deferreds_to_wait_for_before_reading_next_chunk 0))
      |}];
    let%bind () =
      create_connection_and_dispatch_batches_with_transport
        (pipe_rpc ~client_pushes_back:(Some ()))
        ~is_tcp_transport:true
    in
    [%expect
      {|
      (number_of_unique_flushed_waits_due_to_streaming_pushback 1)
      ("TCP transport reader pushback behavior stats"
       (number_of_unique_deferreds_to_wait_for_before_reading_next_chunk 1))
      |}];
    Deferred.unit
  ;;

  let%expect_test "low latency transport" =
    let create_connection_and_dispatch_batches_with_transport rpc =
      let make_transport (fd_r, fd_w) =
        { Async_rpc_kernel.Rpc.Transport.reader =
            Rpc.Low_latency_transport.Reader.create fd_r ~max_message_size
        ; writer = Rpc.Low_latency_transport.Writer.create fd_w ~max_message_size
        }
      in
      create_connection_and_dispatch_batches rpc ~make_transport
    in
    let%bind () =
      create_connection_and_dispatch_batches_with_transport
        (pipe_rpc ~client_pushes_back:None)
        ~is_tcp_transport:false
    in
    [%expect {| (number_of_unique_flushed_waits_due_to_streaming_pushback 0) |}];
    let%bind () =
      create_connection_and_dispatch_batches_with_transport
        (pipe_rpc ~client_pushes_back:(Some ()))
        ~is_tcp_transport:false
    in
    [%expect {| (number_of_unique_flushed_waits_due_to_streaming_pushback 1) |}];
    Deferred.unit
  ;;
end
