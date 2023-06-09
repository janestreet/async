open! Core
open Async
open Import

(* These tests must be in a file by themselves because of lazy evaluation of the environment
   variable *)
let triangle_query ~n ~str =
  (* We only need to set this once but this makes it clear that we always use the same
     value. *)
  Unix.putenv ~key:"ASYNC_RPC_MAX_MESSAGE_SIZE" ~data:"40";
  let make_transport_default_size (fd_r, fd_w) : Rpc.Transport.t =
    { reader = Reader.create fd_r |> Rpc.Transport.Reader.of_reader
    ; writer = Writer.create fd_w |> Rpc.Transport.Writer.of_writer
    }
  in
  match%bind
    let server_conn = Ivar.create () in
    Monitor.try_with (fun () ->
      test1
        ~trace:true
        ~make_transport:make_transport_default_size
        ~imp:[ pipe_triangle_imp ]
        ~state:()
        ~f:(fun sconn conn ->
          Ivar.fill_exn server_conn sconn;
          Rpc.Pipe_rpc.dispatch_exn pipe_triangle_rpc conn (n, str))
        ())
  with
  | Error exn ->
    print_s ([%sexp_of: Exn.t] exn);
    return ()
  | Ok (pipe, (_ : Rpc.Pipe_rpc.Metadata.t)) ->
    let count = ref 0 in
    (match%map
       Clock_ns.with_timeout
         (Time_ns.Span.of_int_sec 5)
         (Pipe.iter pipe ~f:(fun (_ : string) ->
            incr count;
            return ()))
     with
     | `Result _ -> Core.printf "got %d results, pipe closed\n" !count
     | `Timeout ->
       if Pipe.is_closed pipe
       then Core.printf "Timed out unexpectedly!\n"
       else Core.printf "pipe never closed. Got %d results\n" !count)
;;

let%expect_test "Query too large" =
  let%map () =
    triangle_query ~n:1 ~str:"very long string that will be too big for the query"
  in
  [%expect
    {|
    B 1 (pipe_tri)    72B (Failed_to_send Query Too_large)
    (monitor.ml.Error
     ("Message cannot be sent"
      ((reason (Message_too_big ((size 72) (max_message_size 40))))
       (connection
        ((description <created-directly>)
         (writer
          ((t ((file_descr _) (info (writer "rpc_test 1")) (kind Fifo)))
           (max_message_size 40) (total_bytes 8)))))))
     ("<backtrace elided in test>" "Caught by monitor Monitor.protect")) |}]
;;

let%expect_test "responses small enough" =
  let%map () = triangle_query ~n:2 ~str:"shorter string" in
  [%expect
    {|
    B 2 (pipe_tri)    35B (Sent Query)
    A 2 (pipe_tri)    35B (Received Query)
    A 2 (pipe_tri)     7B (Sent (Response Streaming_initial))
    A 2 (pipe_tri)    24B (Sent (Response Streaming_update))
    A 2 (pipe_tri)    38B (Sent (Response Streaming_update))
    A 2 (pipe_tri)     8B (Sent (Response Streaming_closed))
    B 2 (<unknown>)    7B (Received (Response Partial_response))
    B 2 (<unknown>)   24B (Received (Response Partial_response))
    B 2 (<unknown>)   38B (Received (Response Partial_response))
    B 2 (<unknown>)    8B (Received (Response Response_finished))
    got 2 results, pipe closed |}]
;;

let%expect_test "last entry too large" =
  let%map () = triangle_query ~n:3 ~str:"shorter string" in
  [%expect
    {|
    B 3 (pipe_tri)    35B (Sent Query)
    A 3 (pipe_tri)    35B (Received Query)
    A 3 (pipe_tri)     7B (Sent (Response Streaming_initial))
    A 3 (pipe_tri)    24B (Sent (Response Streaming_update))
    A 3 (pipe_tri)    38B (Sent (Response Streaming_update))
    A 3 (pipe_tri)    52B (Failed_to_send (Response Streaming_update) Too_large)
    B 3 (<unknown>)    7B (Received (Response Partial_response))
    B 3 (<unknown>)   24B (Received (Response Partial_response))
    B 3 (<unknown>)   38B (Received (Response Partial_response))
    got 2 results, pipe closed |}]
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
  let%map () = triangle_query ~n:4 ~str:"shorter string" in
  [%expect
    {|
    B 4 (pipe_tri)    35B (Sent Query)
    A 4 (pipe_tri)    35B (Received Query)
    A 4 (pipe_tri)     7B (Sent (Response Streaming_initial))
    A 4 (pipe_tri)    24B (Sent (Response Streaming_update))
    A 4 (pipe_tri)    38B (Sent (Response Streaming_update))
    A 4 (pipe_tri)    52B (Failed_to_send (Response Streaming_update) Too_large)
    B 4 (<unknown>)    7B (Received (Response Partial_response))
    B 4 (<unknown>)   24B (Received (Response Partial_response))
    B 4 (<unknown>)   38B (Received (Response Partial_response))
    got 2 results, pipe closed |}]
;;

let%expect_test "multiple entries too large" =
  let%map () = triangle_query ~n:5 ~str:"shorter string" in
  [%expect
    {|
    B 5 (pipe_tri)    35B (Sent Query)
    A 5 (pipe_tri)    35B (Received Query)
    A 5 (pipe_tri)     7B (Sent (Response Streaming_initial))
    A 5 (pipe_tri)    24B (Sent (Response Streaming_update))
    A 5 (pipe_tri)    38B (Sent (Response Streaming_update))
    A 5 (pipe_tri)    52B (Failed_to_send (Response Streaming_update) Too_large)
    B 5 (<unknown>)    7B (Received (Response Partial_response))
    B 5 (<unknown>)   24B (Received (Response Partial_response))
    B 5 (<unknown>)   38B (Received (Response Partial_response))
    1969-12-31 19:00:00.000000-05:00 Error ("Exception raised to [Monitor.try_with] that already returned.""This error was captured by a default handler in [Async.Log]."(exn(monitor.ml.Error("write to closed pipe"(pipe((id <hidden_in_test>)(buffer())(size_budget 0)(reserved_space 0)(pushback(Full()))(num_values_read 3)(blocked_flushes())(blocked_reads())(closed(Full()))(read_closed(Full()))(consumers(((pipe_id 7)(values_read(Have_not_been_sent_downstream Empty))(downstream_flushed <fun>))))(upstream_flusheds()))))("<backtrace elided in test>""Caught by monitor Monitor.protect"))))
    got 2 results, pipe closed |}]
;;
