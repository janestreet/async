open Core
open Async

let () = Backtrace.elide := true
let max_message_size = 1_000_000

let print_trace (conn : Rpc.Connection.t) source =
  Bus.iter_exn
    (Async_rpc_kernel.Async_rpc_kernel_private.Connection.events conn)
    [%here]
    ~f:(fun (event : Async_rpc_kernel.Tracing_event.t) ->
    let%tydi { event; rpc; id = _; payload_bytes } = event in
    let name =
      match rpc with
      | None -> "<unknown>"
      | Some { name; version = _ } -> String.globalize name
    in
    let header = sprintf !"%s (%s)" source name in
    Core.printf
      !"%-16s %3dB %{sexp: Async_rpc_kernel.Tracing_event.Event.t}\n%!"
      header
      payload_bytes
      ([%globalize: Async_rpc_kernel.Tracing_event.Event.t] event))
;;

let test ~trace ~make_transport ~make_transport2 ~imp1 ~imp2 ~state1 ~state2 ~f () =
  let%bind `Reader r1, `Writer w2 = Unix.pipe (Info.of_string "rpc_test 1") in
  let%bind `Reader r2, `Writer w1 = Unix.pipe (Info.of_string "rpc_test 2") in
  let t1 = make_transport (r1, w1) in
  let t2 = make_transport2 (r2, w2) in
  let s imp =
    if List.length imp > 0
    then
      Some
        (Rpc.Implementations.create_exn
           ~implementations:imp
           ~on_unknown_rpc:`Close_connection)
    else None
  in
  let s1 = s imp1 in
  let s2 = s imp2 in
  let conn1_ivar = Ivar.create () in
  let f2_done =
    Async_rpc_kernel.Rpc.Connection.with_close
      ?implementations:s2
      t2
      ~dispatch_queries:(fun conn2 ->
        if trace then print_trace conn2 "B";
        let%bind conn1 = Ivar.read conn1_ivar in
        f conn1 conn2)
      ~connection_state:(fun _ -> state2)
      ~on_handshake_error:`Raise
  in
  Async_rpc_kernel.Rpc.Connection.with_close
    ?implementations:s1
    t1
    ~dispatch_queries:(fun conn1 ->
      if trace then print_trace conn1 "A";
      Ivar.fill_exn conn1_ivar conn1;
      f2_done)
    ~connection_state:(fun _ -> state1)
    ~on_handshake_error:`Raise
;;

let test1 ~trace ~make_transport ~make_client_transport ~imp ~state ~f =
  test
    ~make_transport2:make_client_transport
    ~trace
    ~make_transport
    ~imp1:imp
    ~state1:state
    ~imp2:[]
    ~state2:()
    ~f
;;

module Pipe_count_error = struct
  type t = [ `Argument_must_be_positive ] [@@deriving bin_io]
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

(* takes (n,str) and outputs a pipe of str, strstr, strstrstr, ... for n events. *)
let pipe_triangle_rpc =
  Rpc.Pipe_rpc.create
    ~name:"pipe_tri"
    ~version:0
    ~bin_query:[%bin_type_class: int * string]
    ~bin_response:String.bin_t
    ~bin_error:Unit.bin_t
    ()
;;

let replicate_rpc =
  Rpc.Rpc.create
    ~name:"replicate"
    ~version:0
    ~bin_query:[%bin_type_class: int * string]
    ~bin_response:String.bin_t
    ~include_in_error_count:Only_on_exn
;;

let pipe_count_imp =
  Rpc.Pipe_rpc.implement pipe_count_rpc (fun () n ->
    if n < 0
    then return (Error `Argument_must_be_positive)
    else (
      let pipe_r, pipe_w = Pipe.create () in
      upon
        (Deferred.List.iter (List.init n ~f:Fn.id) ~how:`Sequential ~f:(fun i ->
           Pipe.write pipe_w i))
        (fun () -> Pipe.close pipe_w);
      return (Ok pipe_r)))
;;

let pipe_wait_imp ivar =
  Rpc.Pipe_rpc.implement pipe_wait_rpc (fun () () ->
    let pipe_r, pipe_w = Pipe.create () in
    (Pipe.write pipe_w ()
     >>> fun () ->
     Ivar.read ivar >>> fun () -> Pipe.write pipe_w () >>> fun () -> Pipe.close pipe_w);
    return (Ok pipe_r))
;;

let pipe_triangle_imp =
  Rpc.Pipe_rpc.implement pipe_triangle_rpc (fun () (n, str) ->
    if n < 0 then failwith "n < 0";
    let rec loop pipe s i =
      if i > n
      then (
        Pipe.close pipe;
        return ())
      else (
        let to_send = s ^ str in
        let%bind () = Pipe.write pipe to_send in
        loop pipe to_send (i + 1))
    in
    let pipe = Pipe.create_reader ~close_on_exception:true (fun pipe -> loop pipe "" 1) in
    return (Ok pipe))
;;

let replicate_imp =
  Rpc.Rpc.implement' replicate_rpc (fun () (n, str) ->
    List.init n ~f:(fun (_ : int) -> str) |> String.concat)
;;
