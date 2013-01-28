open Core.Std
open Async.Std

let test ~imp1 ~imp2 ~state1 ~state2 ~f1 ~f2 () =
  Unix.pipe ()
  >>= fun (`Reader r1, `Writer w2) ->
  Unix.pipe ()
  >>= fun (`Reader r2, `Writer w1) ->
  let r1 = Reader.create r1 in
  let r2 = Reader.create r2 in
  let w1 = Writer.create w1 in
  let w2 = Writer.create w2 in
  let s imp =
    if List.length imp > 0
    then Some (
      Rpc.Server.create_exn
        ~implementations:imp
        ~on_unknown_rpc:`Ignore)
    else None
  in
  let s1 = s imp1 in
  let s2 = s imp2 in
  Deferred.choose_ident
    [ Rpc.Connection.with_close ?server:s1 r1 w1 ~dispatch_queries:f1
        ~connection_state:state1
        ~on_handshake_error:`Raise
    ; Rpc.Connection.with_close ?server:s2 r2 w2 ~dispatch_queries:f2
        ~connection_state:state2
        ~on_handshake_error:`Raise
    ]

let test1 ~imp ~state ~f =
  test
    ~imp1:imp ~state1:state ~f1:(fun _ -> Deferred.never ())
    ~imp2:[] ~state2:() ~f2:f

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

let pipe_count_imp =
  Rpc.Pipe_rpc.implement pipe_count_rpc (fun () n ~aborted:_ ->
    if n < 0
    then return (Error `Argument_must_be_positive)
    else
      let pipe_r, pipe_w = Pipe.create () in
      begin
        Deferred.List.iter (List.init n ~f:Fn.id) ~how:`Sequential ~f:(fun i ->
          Pipe.write pipe_w i)
        >>> fun () ->
        Pipe.close pipe_w
      end;
      return (Ok pipe_r))

let tests =
  List.mapi ~f:(fun i f -> sprintf "rpc-%d" i, f)
    [ test1 ~imp:[pipe_count_imp] ~state:() ~f:(fun conn ->
      let n = 3 in
      Rpc.Pipe_rpc.dispatch_exn pipe_count_rpc conn n
      >>= fun (pipe_r, _id) ->
      Pipe.fold pipe_r ~init:0 ~f:(fun x i ->
        assert (x=i);
        i+1)
      >>= fun x ->
      assert (x=n);
      Deferred.unit)
    ; test1 ~imp:[pipe_count_imp] ~state:() ~f:(fun conn ->
      Rpc.Pipe_rpc.dispatch pipe_count_rpc conn (-1)
      >>= fun result ->
      match result with
      | Ok (Ok _) | Error _ -> assert false
      | Ok (Error `Argument_must_be_positive) ->
        Deferred.unit)
    ]
