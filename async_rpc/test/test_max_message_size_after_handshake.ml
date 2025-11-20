open! Core
open Async
open Import

let env_var_max_message_size = 125

(* These tests must be in a file by themselves because of lazy evaluation of the
   environment variable *)
let triangle_query'
  ?filter_events
  ~kind
  ~base_max_message_size
  ~client_message_size
  ~n
  ~str
  ()
  =
  (* We set the env var to the same value in every test, but do so here as it causes
     problems for other tests when it is a toplevel operation *)
  (Unix.putenv [@ocaml.alert "-unsafe_multidomain"])
    ~key:"ASYNC_RPC_MAX_MESSAGE_SIZE"
    ~data:(Int.to_string env_var_max_message_size);
  let make_transport_default_size (fd_r, fd_w) : Rpc.Transport.t =
    { reader = Reader.create fd_r |> Rpc.Transport.Reader.of_reader
    ; writer = Writer.create fd_w |> Rpc.Transport.Writer.of_writer
    }
  in
  let make_transport ~max_message_size (fd_r, fd_w) : Rpc.Transport.t =
    { reader = Reader.create fd_r |> Rpc.Transport.Reader.of_reader ~max_message_size
    ; writer = Writer.create fd_w |> Rpc.Transport.Writer.of_writer ~max_message_size
    }
  in
  let make_client_transport =
    match client_message_size with
    | None -> make_transport_default_size
    | Some max_message_size -> make_transport ~max_message_size
  in
  let make_transport =
    if base_max_message_size = env_var_max_message_size
    then make_transport_default_size
    else make_transport ~max_message_size:base_max_message_size
  in
  let timeout_to_ensure_test_runner_finishes__aid_in_debugging =
    (* This timeout point is so that you can still use the test runner if you accidentally
       introduce a bug that would otherwise cause your deferred to never become
       determined. Without a timeout it is much more annoying to debug. In practice we
       never expect this to be hit. *)
    Time_ns.Span.of_int_sec 20
  in
  let server_conn = Ivar.create () in
  let client_conn = Ivar.create () in
  let%bind test_result =
    test1
      ?filter_events
      ~trace:true
      ~on_handshake_error:`Raise
      ~make_client_transport
      ~make_transport
      ~server_implementations:[ replicate_imp; pipe_triangle_imp ]
      ~server_state:()
      ~f:(fun ~server_side_connection ~client_side_connection ->
        Ivar.fill_exn server_conn server_side_connection;
        Ivar.fill_exn client_conn client_side_connection;
        match kind with
        | `Rpc ->
          let%map.Deferred.Or_error r =
            Rpc.Rpc.dispatch replicate_rpc client_side_connection (n, str)
          in
          `Plain r
        | `Pipe ->
          let%map.Deferred.Or_error r =
            Rpc.Pipe_rpc.dispatch pipe_triangle_rpc client_side_connection (n, str)
          in
          `Pipe r)
      ()
  and client_server_close_reason =
    let connected_and_closed_connections =
      let%bind server_conn = Ivar.read server_conn in
      let%bind client_conn = Ivar.read client_conn in
      let%bind client_reason =
        Rpc.Connection.close_reason client_conn ~on_close:`started
      in
      let%map server_reason =
        Rpc.Connection.close_reason server_conn ~on_close:`started
      in
      client_reason, server_reason
    in
    Clock_ns.with_timeout
      timeout_to_ensure_test_runner_finishes__aid_in_debugging
      connected_and_closed_connections
  in
  let print_s_without_error_locations sexp =
    Sexp.to_string_hum sexp
    |> Re2.replace_exn ~f:(const ".ml:LOC") (Re2.of_string "\\.ml:[0-9]+:[0-9]+")
    |> print_endline
  in
  let%bind () =
    match test_result with
    | Error error ->
      print_s_without_error_locations ([%sexp_of: Error.t] error);
      return ()
    | Ok (`Plain s) ->
      print_s [%message "Got response" ~length:(String.length s : int)];
      return ()
    | Ok (`Pipe (Error ())) -> failwith "RPC implementation failed"
    | Ok (`Pipe (Ok (pipe, (_ : Rpc.Pipe_rpc.Metadata.t)))) ->
      let count = ref 0 in
      (match%map
         Clock_ns.with_timeout
           timeout_to_ensure_test_runner_finishes__aid_in_debugging
           (Pipe.iter pipe ~f:(fun (_ : string) ->
              incr count;
              return ()))
       with
       | `Result _ -> print_s [%message "pipe_closed" ~num_results:(!count : int)]
       | `Timeout ->
         if Pipe.is_closed pipe
         then print_s [%message "Timed out unexpectedly!"]
         else print_s [%message "pipe never closed" ~results:(!count : int)])
  in
  print_endline "(client, server) connection close reasons:";
  print_s_without_error_locations
    ([%sexp_of: (Info.t * Info.t) Clock_ns.Or_timeout.t] client_server_close_reason);
  return ()
;;

let triangle_query ~n ~str_length =
  triangle_query'
    ~base_max_message_size:125
    ~client_message_size:None
    ~n
    ~str:(String.make str_length 'A')
    ()
;;

let%expect_test "Query too large" =
  let%bind () =
    triangle_query ~kind:`Rpc ~n:1 ~str_length:(env_var_max_message_size + 1)
  in
  [%expect
    {|
    B (replicate)    136B (Failed_to_send Query Too_large)
    ((rpc_error (Message_too_big ((size 136) (max_message_size 125))))
     (connection_description <created-directly>) (rpc_name replicate)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result
     ((("Connection closed by local side:" "Rpc.Connection.with_close finished")
       (connection_description <created-directly>))
      ("EOF or connection closed" (connection_description <created-directly>))))
    |}];
  let%map () =
    triangle_query ~kind:`Pipe ~n:1 ~str_length:(env_var_max_message_size + 1)
  in
  [%expect
    {|
    B (pipe_tri)     143B (Failed_to_send Query Too_large)
    ((rpc_error (Message_too_big ((size 143) (max_message_size 125))))
     (connection_description <created-directly>) (rpc_name pipe_tri)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result
     ((("Connection closed by local side:" "Rpc.Connection.with_close finished")
       (connection_description <created-directly>))
      ("EOF or connection closed" (connection_description <created-directly>))))
    |}]
;;

let%expect_test "response and error too large" =
  let%map () = triangle_query ~kind:`Rpc ~n:20 ~str_length:10 in
  [%expect
    {|
    B (replicate)     18B (Sent Query)
    A (replicate)     18B (Received Query)
    A (replicate)    211B (Failed_to_send (Response Single_succeeded) Too_large)
    B (replicate)     65B (Received
     (Response
      (Response_finished_rpc_error_or_exn
       (Write_error (Message_too_big ((size 211) (max_message_size 125)))))))
    ((rpc_error
      (Write_error (Message_too_big ((size 211) (max_message_size 125)))))
     (connection_description <created-directly>) (rpc_name replicate)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result
     ((("Connection closed by local side:" "Rpc.Connection.with_close finished")
       (connection_description <created-directly>))
      ("EOF or connection closed" (connection_description <created-directly>))))
    |}]
;;

let%expect_test "response too large" =
  let%map () =
    triangle_query'
      ~base_max_message_size:200
      ~client_message_size:(Some 200)
      ~kind:`Rpc
      ~n:100
      ~str:"some string"
      ()
  in
  [%expect
    {|
    B (replicate)     19B (Sent Query)
    A (replicate)     19B (Received Query)
    A (replicate)    1111B (Failed_to_send (Response Single_succeeded) Too_large)
    B (replicate)     66B (Received
     (Response
      (Response_finished_rpc_error_or_exn
       (Write_error (Message_too_big ((size 1111) (max_message_size 200)))))))
    ((rpc_error
      (Write_error (Message_too_big ((size 1111) (max_message_size 200)))))
     (connection_description <created-directly>) (rpc_name replicate)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result
     ((("Connection closed by local side:" "Rpc.Connection.with_close finished")
       (connection_description <created-directly>))
      ("EOF or connection closed" (connection_description <created-directly>))))
    |}]
;;

let%expect_test "responses small enough" =
  let n = 2 in
  let str_length = env_var_max_message_size / (n + 1) in
  let%map () = triangle_query ~kind:`Pipe ~n ~str_length in
  [%expect
    {|
    B (pipe_tri)      54B (Sent Query)
    A (pipe_tri)      54B (Received Query)
    A (pipe_tri)       9B (Sent (Response Streaming_initial))
    A (pipe_tri)      53B (Sent (Response Streaming_update))
    A (pipe_tri)      94B (Sent (Response Streaming_update))
    A (pipe_tri)      10B (Sent (Response Streaming_closed))
    B (pipe_tri)       9B (Received (Response Partial_response))
    B (pipe_tri)      53B (Received (Response Partial_response))
    B (pipe_tri)      94B (Received (Response Partial_response))
    B (pipe_tri)      10B (Received (Response Response_finished_ok))
    (pipe_closed (num_results 2))
    (client, server) connection close reasons:
    (Result
     ((("Connection closed by local side:" "Rpc.Connection.with_close finished")
       (connection_description <created-directly>))
      ("EOF or connection closed" (connection_description <created-directly>))))
    |}]
;;

let%expect_test "last entry too large" =
  let str_length_to_overflow_after_3 = env_var_max_message_size / 3 in
  let%map () =
    triangle_query ~kind:`Pipe ~n:3 ~str_length:str_length_to_overflow_after_3
  in
  [%expect
    {|
    B (pipe_tri)      54B (Sent Query)
    A (pipe_tri)      54B (Received Query)
    A (pipe_tri)       9B (Sent (Response Streaming_initial))
    A (pipe_tri)      53B (Sent (Response Streaming_update))
    A (pipe_tri)      94B (Sent (Response Streaming_update))
    A (pipe_tri)     137B (Failed_to_send (Response Streaming_update) Too_large)
    A (pipe_tri)      10B (Sent (Response Streaming_closed))
    B (pipe_tri)       9B (Received (Response Partial_response))
    B (pipe_tri)      53B (Received (Response Partial_response))
    B (pipe_tri)      94B (Received (Response Partial_response))
    B (pipe_tri)      10B (Sent Abort_streaming_rpc_query)
    B (pipe_tri)      10B (Received (Response One_way_so_no_response))
    B (pipe_tri)      65B (Received (Response Partial_response))
    B (pipe_tri)      10B (Received
     (Response
      (Response_finished_rpc_error_or_exn
       (Write_error (Message_too_big ((size 137) (max_message_size 125)))))))
    A (pipe_tri)      10B (Received Abort_streaming_rpc_query)
    (pipe_closed (num_results 2))
    (client, server) connection close reasons:
    (Result
     ((("Connection closed by local side:" "Rpc.Connection.with_close finished")
       (connection_description <created-directly>))
      ("EOF or connection closed" (connection_description <created-directly>))))
    |}]
;;

let%expect_test "multiple entries too large" =
  let%map () = triangle_query ~kind:`Pipe ~n:5 ~str_length:30 in
  [%expect
    {|
    B (pipe_tri)      43B (Sent Query)
    A (pipe_tri)      43B (Received Query)
    A (pipe_tri)       9B (Sent (Response Streaming_initial))
    A (pipe_tri)      42B (Sent (Response Streaming_update))
    A (pipe_tri)      72B (Sent (Response Streaming_update))
    A (pipe_tri)     102B (Sent (Response Streaming_update))
    A (pipe_tri)     132B (Failed_to_send (Response Streaming_update) Too_large)
    A (pipe_tri)     168B (Failed_to_send (Response Streaming_update) Too_large)
    A (pipe_tri)      10B (Sent (Response Streaming_closed))
    B (pipe_tri)       9B (Received (Response Partial_response))
    B (pipe_tri)      42B (Received (Response Partial_response))
    B (pipe_tri)      72B (Received (Response Partial_response))
    B (pipe_tri)     102B (Received (Response Partial_response))
    B (pipe_tri)      10B (Sent Abort_streaming_rpc_query)
    B (pipe_tri)      10B (Received (Response One_way_so_no_response))
    B (pipe_tri)      65B (Received (Response Partial_response))
    B (pipe_tri)      65B (Received (Response Partial_response))
    B (pipe_tri)      10B (Received
     (Response
      (Response_finished_rpc_error_or_exn
       (Write_error (Message_too_big ((size 132) (max_message_size 125)))))))
    A (pipe_tri)      10B (Received Abort_streaming_rpc_query)
    (pipe_closed (num_results 3))
    (client, server) connection close reasons:
    (Result
     ((("Connection closed by local side:" "Rpc.Connection.with_close finished")
       (connection_description <created-directly>))
      ("EOF or connection closed" (connection_description <created-directly>))))
    |}]
;;

let%expect_test "query too big for callee" =
  (* Here the client successfully sends more bytes of query than the server will take, and
     so the server rejects it. The large limit should allow enough room for the server to
     respond with an error message over the protocol (but it doesn’t) *)
  let%map () =
    let base_max_message_size = 8_000 in
    triangle_query'
      ~kind:`Pipe
      ~base_max_message_size
      ~client_message_size:(Some 10_000)
      ~n:1
      ~str:(String.init base_max_message_size ~f:(Fn.const 'x'))
      ()
  in
  [%expect
    {|
    B (pipe_tri)     8019B (Sent Query)
    ((rpc_error
      (Connection_closed
       ((("Connection closed by remote side:"
          ("exn raised in RPC connection loop"
           (monitor.ml.Error
            ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
             ((Message_size 8019) (Max_message_size 8000))
             lib/async_rpc/core/src/rpc_transport.ml:LOC)
            ("<backtrace elided in test>"
             "Caught by monitor RPC connection loop"))))
         (connection_description <created-directly>)))))
     (connection_description <created-directly>) (rpc_name pipe_tri)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result
     ((("Connection closed by remote side:"
        ("exn raised in RPC connection loop"
         (monitor.ml.Error
          ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
           ((Message_size 8019) (Max_message_size 8000))
           lib/async_rpc/core/src/rpc_transport.ml:LOC)
          ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))
       (connection_description <created-directly>))
      (("exn raised in RPC connection loop"
        (monitor.ml.Error
         ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
          ((Message_size 8019) (Max_message_size 8000))
          lib/async_rpc/core/src/rpc_transport.ml:LOC)
         ("<backtrace elided in test>" "Caught by monitor RPC connection loop")))
       (connection_description <created-directly>))))
    |}]
;;

let%expect_test "response too big for caller" =
  let%map () =
    let server_message_size = 10_000 in
    triangle_query'
      ~kind:`Rpc
      ~base_max_message_size:server_message_size
      ~client_message_size:(Some 8_000)
      ~n:2
      ~str:(String.init ((server_message_size / 2) - 100) ~f:(Fn.const 'x'))
      ()
  in
  [%expect
    {|
    B (replicate)    4912B (Sent Query)
    A (replicate)    4912B (Received Query)
    A (replicate)    9811B (Sent (Response Single_succeeded))
    ((rpc_error
      (Uncaught_exn
       (monitor.ml.Error
        ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
         ((Message_size 9811) (Max_message_size 8000))
         lib/async_rpc/core/src/rpc_transport.ml:LOC)
        ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))
     (connection_description <created-directly>) (rpc_name replicate)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result
     ((("exn raised in RPC connection loop"
        (monitor.ml.Error
         ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
          ((Message_size 9811) (Max_message_size 8000))
          lib/async_rpc/core/src/rpc_transport.ml:LOC)
         ("<backtrace elided in test>" "Caught by monitor RPC connection loop")))
       (connection_description <created-directly>))
      (("Connection closed by remote side:"
        ("exn raised in RPC connection loop"
         (monitor.ml.Error
          ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
           ((Message_size 9811) (Max_message_size 8000))
           lib/async_rpc/core/src/rpc_transport.ml:LOC)
          ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))
       (connection_description <created-directly>))))
    |}]
;;

let%expect_test "pipe response too big for caller" =
  let%map () =
    let server_message_size = 10_000 in
    triangle_query'
    (* Filter the [Streaming_closed] message to remove nondeterminism in message ordering *)
      ~filter_events:(fun event ->
        let open Async_rpc_kernel.Tracing_event in
        match event with
        | { event = Event.Sent (Kind.Response Sent_response_kind.Streaming_closed); _ } ->
          true
        | _ -> false)
      ~kind:`Pipe
      ~base_max_message_size:server_message_size
      ~client_message_size:(Some 8_000)
      ~n:2
      ~str:(String.init ((server_message_size / 2) - 100) ~f:(Fn.const 'x'))
      ()
  in
  [%expect
    {|
    B (pipe_tri)     4919B (Sent Query)
    A (pipe_tri)     4919B (Received Query)
    A (pipe_tri)       9B (Sent (Response Streaming_initial))
    A (pipe_tri)     4918B (Sent (Response Streaming_update))
    A (pipe_tri)     9818B (Sent (Response Streaming_update))
    B (pipe_tri)       9B (Received (Response Partial_response))
    B (pipe_tri)     4918B (Received (Response Partial_response))
    (pipe_closed (num_results 1))
    (client, server) connection close reasons:
    (Result
     ((("exn raised in RPC connection loop"
        (monitor.ml.Error
         ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
          ((Message_size 9818) (Max_message_size 8000))
          lib/async_rpc/core/src/rpc_transport.ml:LOC)
         ("<backtrace elided in test>" "Caught by monitor RPC connection loop")))
       (connection_description <created-directly>))
      (("Connection closed by remote side:"
        ("exn raised in RPC connection loop"
         (monitor.ml.Error
          ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
           ((Message_size 9818) (Max_message_size 8000))
           lib/async_rpc/core/src/rpc_transport.ml:LOC)
          ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))
       (connection_description <created-directly>))))
    |}]
;;
