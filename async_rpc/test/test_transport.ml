open! Core
open! Async

let rpc =
  Rpc.Rpc.create
    ~name:"test-rpc"
    ~version:1
    ~bin_query:Bigstring.Stable.V1.bin_t
    ~bin_response:Bigstring.Stable.V1.bin_t
    ~include_in_error_count:Only_on_exn
;;

let%expect_test "regression test: [flushed] becomes determined even if the connection \
                 closes before "
  =
  let%bind `Reader reader_fd, `Writer writer_fd =
    Unix.pipe (Info.of_string "rpc test connection")
  in
  let reader = Reader.create reader_fd in
  let writer = Writer.create writer_fd in
  let%bind circular_connection =
    Rpc.Connection.create ~connection_state:(const ()) reader writer >>| Result.ok_exn
  in
  let message_bigger_than_pipe_buffer =
    Bigstring.init (65536 * 4) ~f:(fun (_ : int) -> 'a')
  in
  let response =
    Rpc.Rpc.dispatch rpc circular_connection message_bigger_than_pipe_buffer
  in
  let flushed = Rpc.Connection.flushed circular_connection in
  let%bind () = Reader.close reader in
  let%bind () = response >>| [%sexp_of: _ Or_error.t] >>| print_s in
  [%expect
    {|
    (Error
     ((rpc_error
       (Connection_closed
        (("EOF or connection closed" (connection_description <created-directly>)))))
      (connection_description <created-directly>) (rpc_name test-rpc)
      (rpc_version 1)))
    |}];
  let%bind () = flushed in
  return ()
;;
