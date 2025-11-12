open Core
open Async

let () = Dynamic.set_root Backtrace.elide true
let max_message_size = 1_000_000

let print_trace (conn : Rpc.Connection.t) ?filter_events source =
  Bus.subscribe_permanently_exn
    (Async_rpc_kernel.Async_rpc_kernel_private.Connection.tracing_events conn)
    ~f:(fun (local_ (event : Async_rpc_kernel.Tracing_event.t)) ->
      match filter_events with
      | Some matches_filter when matches_filter event -> ()
      | None | Some _ ->
        let%tydi { event; rpc; id = _; payload_bytes } = event in
        let header = sprintf !"%s (%s)" source rpc.name in
        Core.printf
          !"%-16s %3dB %{sexp: Async_rpc_kernel.Tracing_event.Event.t}\n%!"
          header
          payload_bytes
          ([%globalize: Async_rpc_kernel.Tracing_event.Event.t] event))
;;

let test
  ?filter_events
  ?(trace = false)
  ()
  ~on_handshake_error
  ~make_transport
  ~make_client_transport
  ~server_implementations
  ~client_implementations
  ~server_state
  ~client_state
  ~f
  =
  let%bind `Reader r1, `Writer w2 = Unix.pipe (Info.of_string "rpc_test 1") in
  let%bind `Reader r2, `Writer w1 = Unix.pipe (Info.of_string "rpc_test 2") in
  let server_transport = make_transport (r1, w1) in
  let client_transport = make_client_transport (r2, w2) in
  let create_implementations implementations =
    if List.length implementations > 0
    then
      Some
        (Rpc.Implementations.create_exn
           ~implementations
           ~on_unknown_rpc:`Close_connection
           ~on_exception:Close_connection)
    else None
  in
  let server_implementations = create_implementations server_implementations in
  let client_implementations = create_implementations client_implementations in
  let server_side_connection_ivar = Ivar.create () in
  let client_done =
    Async_rpc_kernel.Rpc.Connection.with_close
      ?implementations:client_implementations
      client_transport
      ~dispatch_queries:(fun client_side_connection ->
        if trace then print_trace ?filter_events client_side_connection "B";
        let%bind server_side_connection = Ivar.read server_side_connection_ivar in
        f ~server_side_connection ~client_side_connection)
      ~connection_state:(fun _ -> client_state)
      ~on_handshake_error
  in
  Async_rpc_kernel.Rpc.Connection.with_close
    ?implementations:server_implementations
    server_transport
    ~dispatch_queries:(fun server_side_connection ->
      if trace then print_trace ?filter_events server_side_connection "A";
      Ivar.fill_exn server_side_connection_ivar server_side_connection;
      client_done)
    ~connection_state:(fun _ -> server_state)
    ~on_handshake_error
;;

let test1
  ?filter_events
  ?trace
  ~make_transport
  ~make_client_transport
  ~on_handshake_error
  ~server_implementations
  ~server_state
  ~f
  =
  test
    ?filter_events
    ?trace
    ~on_handshake_error
    ~make_transport
    ~make_client_transport
    ~server_implementations
    ~server_state
    ~client_implementations:[]
    ~client_state:()
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
