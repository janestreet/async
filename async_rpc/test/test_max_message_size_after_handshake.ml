open! Core
open Async
open Import

let env_var_max_message_size = 40

(* These tests must be in a file by themselves because of lazy evaluation of the environment
   variable *)
let triangle_query' ~kind ~base_max_message_size ~client_message_size ~n ~str =
  (* We set the env var to the same value in every test, but do so here as it causes
     problems for other tests when it is a toplevel operation *)
  Unix.putenv
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
      ~trace:true
      ~make_client_transport
      ~make_transport
      ~imp:[ replicate_imp; pipe_triangle_imp ]
      ~state:()
      ~f:(fun sconn conn ->
        Ivar.fill_exn server_conn sconn;
        Ivar.fill_exn client_conn conn;
        match kind with
        | `Rpc ->
          let%map.Deferred.Or_error r = Rpc.Rpc.dispatch replicate_rpc conn (n, str) in
          `Plain r
        | `Pipe ->
          let%map.Deferred.Or_error r =
            Rpc.Pipe_rpc.dispatch pipe_triangle_rpc conn (n, str)
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

let triangle_query ~n ~str =
  triangle_query' ~base_max_message_size:40 ~client_message_size:None ~n ~str
;;

let%expect_test "Query too large" =
  let%bind () =
    triangle_query
      ~kind:`Rpc
      ~n:1
      ~str:"very long string that will be too big for the query"
  in
  [%expect
    {|
    B (replicate)     68B (Failed_to_send Query Too_large)
    ((rpc_error
      (Uncaught_exn
       ("Message cannot be sent"
        ((reason (Message_too_big ((size 68) (max_message_size 40))))
         (connection
          ((description <created-directly>)
           (writer
            ((t ((file_descr _) (info (writer "rpc_test 1")) (kind Fifo)))
             (max_message_size 40) (total_bytes 8)))))))))
     (connection_description <created-directly>) (rpc_name replicate)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result ("Rpc.Connection.with_close finished" "EOF or connection closed")) |}];
  let%map () =
    triangle_query
      ~kind:`Pipe
      ~n:1
      ~str:"very long string that will be too big for the query"
  in
  [%expect
    {|
    B (pipe_tri)      72B (Failed_to_send Query Too_large)
    ((rpc_error
      (Uncaught_exn
       ("Message cannot be sent"
        ((reason (Message_too_big ((size 72) (max_message_size 40))))
         (connection
          ((description <created-directly>)
           (writer
            ((t ((file_descr _) (info (writer "rpc_test 1")) (kind Fifo)))
             (max_message_size 40) (total_bytes 8)))))))))
     (connection_description <created-directly>) (rpc_name pipe_tri)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result ("Rpc.Connection.with_close finished" "EOF or connection closed")) |}]
;;

let%expect_test "response and error too large" =
  let%map () = triangle_query ~kind:`Rpc ~n:20 ~str:"some string" in
  [%expect
    {|
    B (replicate)     28B (Sent Query)
    A (replicate)     28B (Received Query)
    A (replicate)    229B (Failed_to_send (Response Single_succeeded) Too_large)
    A (replicate)    282B (Failed_to_send (Response Single_or_streaming_error) Too_large)
    ((rpc_error (Connection_closed ("EOF or connection closed")))
     (connection_description <created-directly>) (rpc_name replicate)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result
     ("EOF or connection closed"
      ("exn raised in RPC connection loop"
       (monitor.ml.Error
        ("Failed to send write error to client"
         ((error (Message_too_big ((size 282) (max_message_size 40))))
          (reason (Message_too_big ((size 62) (max_message_size 40))))))
        ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))) |}]
;;

let%expect_test "response too large" =
  let%map () =
    triangle_query'
      ~base_max_message_size:200
      ~client_message_size:(Some 200)
      ~kind:`Rpc
      ~n:100
      ~str:"some string"
  in
  [%expect
    {|
    B (replicate)     28B (Sent Query)
    A (replicate)     28B (Received Query)
    A (replicate)    1109B (Failed_to_send (Response Single_succeeded) Too_large)
    B (<unknown>)     64B (Received (Response Response_finished))
    ((rpc_error
      (Write_error (Message_too_big ((size 1109) (max_message_size 200)))))
     (connection_description <created-directly>) (rpc_name replicate)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result ("Rpc.Connection.with_close finished" "EOF or connection closed")) |}]
;;

let%expect_test "responses small enough" =
  let%map () = triangle_query ~kind:`Pipe ~n:2 ~str:"shorter string" in
  [%expect
    {|
    B (pipe_tri)      35B (Sent Query)
    A (pipe_tri)      35B (Received Query)
    A (pipe_tri)       7B (Sent (Response Streaming_initial))
    A (pipe_tri)      24B (Sent (Response Streaming_update))
    A (pipe_tri)      38B (Sent (Response Streaming_update))
    A (pipe_tri)       8B (Sent (Response Streaming_closed))
    B (<unknown>)      7B (Received (Response Partial_response))
    B (<unknown>)     24B (Received (Response Partial_response))
    B (<unknown>)     38B (Received (Response Partial_response))
    B (<unknown>)      8B (Received (Response Response_finished))
    (pipe_closed (num_results 2))
    (client, server) connection close reasons:
    (Result ("Rpc.Connection.with_close finished" "EOF or connection closed")) |}]
;;

let%expect_test "last entry too large" =
  let%map () = triangle_query ~kind:`Pipe ~n:3 ~str:"shorter string" in
  [%expect
    {|
    B (pipe_tri)      35B (Sent Query)
    A (pipe_tri)      35B (Received Query)
    A (pipe_tri)       7B (Sent (Response Streaming_initial))
    A (pipe_tri)      24B (Sent (Response Streaming_update))
    A (pipe_tri)      38B (Sent (Response Streaming_update))
    A (pipe_tri)      52B (Failed_to_send (Response Streaming_update) Too_large)
    B (<unknown>)      7B (Received (Response Partial_response))
    B (<unknown>)     24B (Received (Response Partial_response))
    B (<unknown>)     38B (Received (Response Partial_response))
    (pipe_closed (num_results 2))
    (client, server) connection close reasons:
    (Result
     ("Rpc.Connection.with_close finished"
      ("exn raised in RPC connection loop"
       (monitor.ml.Error
        ("Failed to send write error to client"
         ((error (Message_too_big ((size 52) (max_message_size 40))))
          (reason (Message_too_big ((size 61) (max_message_size 40))))))
        ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))) |}]
;;

(* I think something a little interesting is going on here. Considering the two tests
   above and the two tests below, we have:

   - 2 elts: works
   - 3rd elt is too big. We put it into the pipe, the rpc lib takes it out of the pipe,
     fails to send, and closes the connection and pipe, causing the pipe on the dispatcher
     side to close too
   - 4th elt is also too big. When we want to write it, we wait for the pipe pushback
     (i.e. for the rpc lib to take it out of the pipe) and manage to stuff the next elt in
     before the pipe is closed.
   - 5th elt is also too big. This time, when we try to do the pushback before writing, we
     fail because the pipe (which has the 4th elt in its buffer) is now closed.

   It mostly seems like quite sensitive dependence on the order that async tasks run.
*)

let%expect_test "multiple entries too large but no exn" =
  let%map () = triangle_query ~kind:`Pipe ~n:4 ~str:"shorter string" in
  [%expect
    {|
    B (pipe_tri)      35B (Sent Query)
    A (pipe_tri)      35B (Received Query)
    A (pipe_tri)       7B (Sent (Response Streaming_initial))
    A (pipe_tri)      24B (Sent (Response Streaming_update))
    A (pipe_tri)      38B (Sent (Response Streaming_update))
    A (pipe_tri)      52B (Failed_to_send (Response Streaming_update) Too_large)
    B (<unknown>)      7B (Received (Response Partial_response))
    B (<unknown>)     24B (Received (Response Partial_response))
    B (<unknown>)     38B (Received (Response Partial_response))
    (pipe_closed (num_results 2))
    (client, server) connection close reasons:
    (Result
     ("Rpc.Connection.with_close finished"
      ("exn raised in RPC connection loop"
       (monitor.ml.Error
        ("Failed to send write error to client"
         ((error (Message_too_big ((size 52) (max_message_size 40))))
          (reason (Message_too_big ((size 61) (max_message_size 40))))))
        ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))) |}]
;;

let%expect_test "multiple entries too large" =
  let%map () = triangle_query ~kind:`Pipe ~n:5 ~str:"shorter string" in
  [%expect
    {|
    B (pipe_tri)      35B (Sent Query)
    A (pipe_tri)      35B (Received Query)
    A (pipe_tri)       7B (Sent (Response Streaming_initial))
    A (pipe_tri)      24B (Sent (Response Streaming_update))
    A (pipe_tri)      38B (Sent (Response Streaming_update))
    A (pipe_tri)      52B (Failed_to_send (Response Streaming_update) Too_large)
    B (<unknown>)      7B (Received (Response Partial_response))
    B (<unknown>)     24B (Received (Response Partial_response))
    B (<unknown>)     38B (Received (Response Partial_response))
    1969-12-31 19:00:00.000000-05:00 Error ("Exception raised to [Monitor.try_with] that already returned.""This error was captured by a default handler in [Async.Log]."(exn(monitor.ml.Error("write to closed pipe"(pipe((id <hidden_in_test>)(buffer())(size_budget 0)(reserved_space 0)(pushback(Full()))(num_values_read 3)(blocked_flushes())(blocked_reads())(closed(Full()))(read_closed(Full()))(consumers(((pipe_id 7)(values_read(Have_not_been_sent_downstream Empty))(downstream_flushed <fun>))))(upstream_flusheds()))))("<backtrace elided in test>""Caught by monitor Monitor.protect"))))
    (pipe_closed (num_results 2))
    (client, server) connection close reasons:
    (Result
     ("Rpc.Connection.with_close finished"
      ("exn raised in RPC connection loop"
       (monitor.ml.Error
        ("Failed to send write error to client"
         ((error (Message_too_big ((size 52) (max_message_size 40))))
          (reason (Message_too_big ((size 61) (max_message_size 40))))))
        ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))) |}]
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
  in
  [%expect
    {|
    B (pipe_tri)     8027B (Sent Query)
    ((rpc_error (Connection_closed ("EOF or connection closed")))
     (connection_description <created-directly>) (rpc_name pipe_tri)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result
     ("EOF or connection closed"
      ("exn raised in RPC connection loop"
       (monitor.ml.Error
        ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
         ((Message_size 8027) (Max_message_size 8000))
         lib/async_rpc/core/src/rpc_transport.ml:LOC)
        ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))) |}]
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
  in
  [%expect
    {|
    B (replicate)    4921B (Sent Query)
    A (replicate)    4921B (Received Query)
    A (replicate)    9809B (Sent (Response Single_succeeded))
    ((rpc_error
      (Uncaught_exn
       (monitor.ml.Error
        ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
         ((Message_size 9809) (Max_message_size 8000))
         lib/async_rpc/core/src/rpc_transport.ml:LOC)
        ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))
     (connection_description <created-directly>) (rpc_name replicate)
     (rpc_version 0))
    (client, server) connection close reasons:
    (Result
     (("exn raised in RPC connection loop"
       (monitor.ml.Error
        ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
         ((Message_size 9809) (Max_message_size 8000))
         lib/async_rpc/core/src/rpc_transport.ml:LOC)
        ("<backtrace elided in test>" "Caught by monitor RPC connection loop")))
      "EOF or connection closed")) |}]
;;

let%expect_test "pipe response too big for caller" =
  let%map () =
    let server_message_size = 10_000 in
    triangle_query'
      ~kind:`Pipe
      ~base_max_message_size:server_message_size
      ~client_message_size:(Some 8_000)
      ~n:2
      ~str:(String.init ((server_message_size / 2) - 100) ~f:(Fn.const 'x'))
  in
  [%expect
    {|
    B (pipe_tri)     4927B (Sent Query)
    A (pipe_tri)     4927B (Received Query)
    A (pipe_tri)       7B (Sent (Response Streaming_initial))
    A (pipe_tri)     4916B (Sent (Response Streaming_update))
    A (pipe_tri)     9816B (Sent (Response Streaming_update))
    A (pipe_tri)       8B (Sent (Response Streaming_closed))
    B (<unknown>)      7B (Received (Response Partial_response))
    B (<unknown>)    4916B (Received (Response Partial_response))
    (pipe_closed (num_results 1))
    (client, server) connection close reasons:
    (Result
     (("exn raised in RPC connection loop"
       (monitor.ml.Error
        ("Rpc_transport: message is too large or has negative size. Try increasing the size limit by setting the ASYNC_RPC_MAX_MESSAGE_SIZE env var"
         ((Message_size 9816) (Max_message_size 8000))
         lib/async_rpc/core/src/rpc_transport.ml:LOC)
        ("<backtrace elided in test>" "Caught by monitor RPC connection loop")))
      "EOF or connection closed")) |}]
;;
