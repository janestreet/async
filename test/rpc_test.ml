open Core.Std
open Async.Std

module Debug = Async_kernel.Debug

let test ~imp1 ~imp2 ~state1 ~state2 ~f () =
  Unix.pipe (Info.of_string "rpc_test 1")
  >>= fun (`Reader r1, `Writer w2) ->
  Unix.pipe (Info.of_string "rpc_test 2")
  >>= fun (`Reader r2, `Writer w1) ->
  let r1 = Reader.create r1 in
  let r2 = Reader.create r2 in
  let w1 = Writer.create w1 in
  let w2 = Writer.create w2 in
  let s imp =
    if List.length imp > 0
    then Some (
      Rpc.Implementations.create_exn
        ~implementations:imp
        ~on_unknown_rpc:`Close_connection)
    else None
  in
  let s1 = s imp1 in
  let s2 = s imp2 in
  let conn1_ivar = Ivar.create () in
  let f2_done =
    Rpc.Connection.with_close ?implementations:s2 r2 w2
      ~dispatch_queries:(fun conn2 ->
        Ivar.read conn1_ivar >>= fun conn1 ->
        f conn1 conn2)
      ~connection_state:(fun _ -> state2)
      ~on_handshake_error:`Raise
  in
  Rpc.Connection.with_close ?implementations:s1 r1 w1
    ~dispatch_queries:(fun conn1 ->
      Ivar.fill conn1_ivar conn1;
      f2_done)
    ~connection_state:(fun _ -> state1)
    ~on_handshake_error:`Raise
;;

let test1 ~imp ~state ~f =
  test
    ~imp1:imp ~state1:state
    ~imp2:[] ~state2:() ~f
;;

module Pipe_count_error = struct
  type t = [`Argument_must_be_positive] with bin_io
end

let pipe_count_rpc =
  Rpc.Pipe_rpc.create
    ~name:"pipe_count"
    ~version:0
    ~bin_query:Int.bin_t
    ~bin_response:Int.bin_t
    ~bin_error:Pipe_count_error.bin_t
    ()
;;

let pipe_wait_rpc =
  Rpc.Pipe_rpc.create
    ~name:"pipe_wait"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:Unit.bin_t
    ~bin_error:Unit.bin_t
    ()
;;

let pipe_count_imp =
  Rpc.Pipe_rpc.implement pipe_count_rpc (fun () n ~aborted:_ ->
    if n < 0
    then return (Error `Argument_must_be_positive)
    else
      let pipe_r, pipe_w = Pipe.create () in
      upon
        (Deferred.List.iter (List.init n ~f:Fn.id) ~how:`Sequential ~f:(fun i ->
          Pipe.write pipe_w i))
        (fun () -> Pipe.close pipe_w);
      return (Ok pipe_r))
;;

let pipe_wait_imp ivar =
  Rpc.Pipe_rpc.implement pipe_wait_rpc (fun () () ~aborted:_ ->
    let pipe_r, pipe_w = Pipe.create () in
    (Pipe.write pipe_w ()
     >>> fun () ->
     Ivar.read ivar
     >>> fun () ->
     Pipe.write pipe_w ()
     >>> fun () ->
     Pipe.close pipe_w);
    return (Ok pipe_r))
;;

let tests =
  List.mapi ~f:(fun i f -> sprintf "rpc-%d" i, f)
    [ test1 ~imp:[pipe_count_imp] ~state:() ~f:(fun _ conn ->
        let n = 3 in
        Rpc.Pipe_rpc.dispatch_exn pipe_count_rpc conn n
        >>= fun (pipe_r, _id) ->
        Pipe.fold_without_pushback pipe_r ~init:0 ~f:(fun x i ->
          assert (x=i);
          i+1)
        >>= fun x ->
        <:test_result< int >> ~expect:n x;
        Deferred.unit)
    ; test1 ~imp:[pipe_count_imp] ~state:() ~f:(fun _ conn ->
        Rpc.Pipe_rpc.dispatch pipe_count_rpc conn (-1)
        >>= fun result ->
        match result with
        | Ok (Ok _) | Error _ -> assert false
        | Ok (Error `Argument_must_be_positive) -> Deferred.unit)
    ; let ivar = Ivar.create () in
      test1 ~imp:[pipe_wait_imp ivar] ~state:() ~f:(fun conn1 conn2 ->
        (* Test that the pipe is flushed when the connection is closed. *)
        Rpc.Pipe_rpc.dispatch_exn pipe_wait_rpc conn2 ()
        >>= fun (pipe_r, _id) ->
        Pipe.read pipe_r
        >>= fun res ->
        assert (res = `Ok ());
        don't_wait_for (Rpc.Connection.close conn1);
        Ivar.fill ivar ();
        Pipe.read pipe_r
        >>= fun res ->
        assert (res = `Ok ());
        Deferred.unit)
    ]
;;
