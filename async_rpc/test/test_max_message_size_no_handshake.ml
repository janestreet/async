open! Core
open Async
open Import

(* This test must be in a file by itself because of lazy evaluation of the environment
   variable
*)
let%expect_test "handshake is too large" =
  Unix.putenv ~key:"ASYNC_RPC_MAX_MESSAGE_SIZE" ~data:"1";
  let make_transport_default_size (fd_r, fd_w) : Rpc.Transport.t =
    { reader = Reader.create fd_r |> Rpc.Transport.Reader.of_reader
    ; writer = Writer.create fd_w |> Rpc.Transport.Writer.of_writer
    }
  in
  let handshake_exn = Ivar.create () in
  let on_handshake_error exn =
    Ivar.fill_if_empty handshake_exn exn;
    return ()
  in
  let%bind _ =
    test1
      ~trace:true
      ~on_handshake_error:(`Call on_handshake_error)
      ~make_transport:make_transport_default_size
      ~make_client_transport:make_transport_default_size
      ~imp:[ pipe_count_imp ]
      ~state:()
      ~f:(fun _ conn ->
        Rpc.Pipe_rpc.dispatch_exn pipe_count_rpc conn 1 |> Deferred.ignore_m)
      ()
  in
  let%bind exn = Ivar.read handshake_exn in
  print_s [%sexp (exn : exn)];
  [%expect
    {|
    (handshake_error.ml.Handshake_error
     ((Message_too_big ((size 12) (max_message_size 1))) <created-directly>))
    |}];
  return ()
;;
