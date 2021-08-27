open! Core
open Poly
open! Async
open! Import
module Debug = Async_kernel_private.Debug

let make_tests ~make_transport ~transport_name =
  List.mapi
    ~f:(fun i f -> sprintf "rpc-%s-%d" transport_name i, f)
    [ test1 ~make_transport ~imp:[ pipe_count_imp ] ~state:() ~f:(fun _ conn ->
        let n = 3 in
        let%bind pipe_r, _id = Rpc.Pipe_rpc.dispatch_exn pipe_count_rpc conn n in
        let%bind x =
          Pipe.fold_without_pushback pipe_r ~init:0 ~f:(fun x i ->
            assert (x = i);
            i + 1)
        in
        [%test_result: int] ~expect:n x;
        Deferred.unit)
    ; test1 ~make_transport ~imp:[ pipe_count_imp ] ~state:() ~f:(fun _ conn ->
        let%bind result = Rpc.Pipe_rpc.dispatch pipe_count_rpc conn (-1) in
        match result with
        | Ok (Ok _) | Error _ -> assert false
        | Ok (Error `Argument_must_be_positive) -> Deferred.unit)
    ; (let ivar = Ivar.create () in
       test1 ~make_transport ~imp:[ pipe_wait_imp ivar ] ~state:() ~f:(fun conn1 conn2 ->
         (* Test that the pipe is flushed when the connection is closed. *)
         let%bind pipe_r, _id = Rpc.Pipe_rpc.dispatch_exn pipe_wait_rpc conn2 () in
         let%bind res = Pipe.read pipe_r in
         assert (res = `Ok ());
         don't_wait_for (Rpc.Connection.close conn1);
         Ivar.fill ivar ();
         let%bind res = Pipe.read pipe_r in
         assert (res = `Ok ());
         Deferred.unit))
    ]
;;

let%expect_test _ =
  let make_transport_std (fd_r, fd_w) : Rpc.Transport.t =
    { reader = Reader.create fd_r |> Rpc.Transport.Reader.of_reader ~max_message_size
    ; writer = Writer.create fd_w |> Rpc.Transport.Writer.of_writer ~max_message_size
    }
  in
  let make_transport_low_latency (fd_r, fd_w) : Rpc.Transport.t =
    { reader = Rpc.Low_latency_transport.Reader.create fd_r ~max_message_size
    ; writer = Rpc.Low_latency_transport.Writer.create fd_w ~max_message_size
    }
  in
  let%bind () =
    Deferred.List.iter
      ~f:(fun (name, f) ->
        print_s [%message name];
        f ())
      (make_tests ~make_transport:make_transport_std ~transport_name:"std"
       @ make_tests
           ~make_transport:make_transport_low_latency
           ~transport_name:"low-latency")
  in
  [%expect
    {|
    rpc-std-0
    rpc-std-1
    rpc-std-2
    rpc-low-latency-0
    rpc-low-latency-1
    rpc-low-latency-2 |}];
  return ()
;;

let%expect_test "[Connection.create] shouldn't raise" =
  let%bind `Reader r1, `Writer w1 = Unix.pipe (Info.of_string "rpc_test 1") in
  let%bind `Reader _, `Writer w2 = Unix.pipe (Info.of_string "rpc_test 2") in
  let result =
    Deferred.create (fun ivar ->
      Monitor.try_with
        ~run:
          `Schedule
        ~rest:`Log
        (fun () ->
           Rpc.Connection.create
             ~connection_state:(Fn.const ())
             (Reader.create r1)
             (Writer.create w2))
      >>> function
      | Error exn -> Ivar.fill ivar (`Raised exn)
      | Ok (Ok (_ : Rpc.Connection.t)) -> assert false
      | Ok (Error exn) -> Ivar.fill ivar (`Returned exn))
  in
  let writer1 = Writer.create w1 in
  (* We must write at least [Header.length] (8) bytes. *)
  Writer.write writer1 "failfail";
  let%bind () = Writer.flushed writer1 in
  let%bind result = result in
  print_s [%message "" ~_:(result : [ `Raised of Exn.t | `Returned of Exn.t ])];
  [%expect
    {|
    (Returned
     (connection.ml.Handshake_error.Handshake_error
      ((Reading_header_failed
        (monitor.ml.Error
         (Failure "unsafe_read_int64: value cannot be represented unboxed!")
         ("<backtrace elided in test>")))
       <created-directly>))) |}];
  return ()
;;

open! Rpc

let%test_unit "Open dispatches see connection closed error" =
  Thread_safe.block_on_async_exn (fun () ->
    let bin_t = Bin_prot.Type_class.bin_unit in
    let rpc =
      Rpc.create
        ~version:1
        ~name:"__TEST_Async_rpc.Rpc"
        ~bin_query:bin_t
        ~bin_response:bin_t
    in
    let serve () =
      let implementation = Rpc.implement rpc (fun () () -> Deferred.never ()) in
      let implementations =
        Implementations.create_exn
          ~implementations:[ implementation ]
          ~on_unknown_rpc:`Raise
      in
      Connection.serve
        ~initial_connection_state:(fun _ _ -> ())
        ~implementations
        ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
        ()
    in
    let client ~port =
      let%bind connection =
        Connection.client
          (Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port })
        >>| Result.ok_exn
      in
      let res = Rpc.dispatch rpc connection () in
      don't_wait_for (Connection.close connection);
      match%map res with
      | Ok () -> failwith "Dispatch should have failed"
      | Error err ->
        [%test_eq: string]
          (sprintf
             "((rpc_error (Connection_closed (Rpc.Connection.close)))\n\
             \ (connection_description (\"Client connected via TCP\" (localhost %d)))\n\
             \ (rpc_tag __TEST_Async_rpc.Rpc) (rpc_version 1))"
             port)
          (Error.to_string_hum err)
    in
    let%bind server = serve () in
    let port = Tcp.Server.listening_on server in
    let%bind () = client ~port in
    Tcp.Server.close server)
;;
