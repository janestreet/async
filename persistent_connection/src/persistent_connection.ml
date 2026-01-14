open! Core
open Async_kernel
open Async_log_kernel
open Async_rpc_kernel
open Async_unix
open! Import
open! Require_explicit_time_source
include Persistent_connection_intf
module Connect_context = Persistent_connection_kernel.Connect_context

module Make' (Conn_err : Connection_error) (Conn : Closable) = struct
  include Persistent_connection_kernel.Make' (Conn_err) (Conn)

  let make_on_event
    (type address)
    ~address:(module Address : Address with type t = address)
    ~created_at
    ~server_name
    ~log
    ~on_event
    event
    =
    Option.iter log ~f:(fun log ->
      let open Ppx_log_syntax in
      let created_at =
        if Source_code_position.equal created_at Lexing.dummy_pos
        then None
        else Some created_at
      in
      [%log.t
        log
          (Persistent_connection_kernel.Event.Variants.to_name event)
          (event : Address.t Event.t)
          ~persistent_connection_to:(server_name : string)
          (created_at
           : (Source_code_position.t Sexp_hidden_in_test.t option[@sexp.option]))
        [@@level Some (Persistent_connection_kernel.Event.log_level event)]]);
    on_event event
  ;;

  let create
    ~(created_at : [%call_pos])
    ~server_name
    ?log
    ?(on_event = fun _ -> Deferred.unit)
    ?retry_delay
    ?random_state
    ?time_source
    ~connect
    ~address
    get_address
    =
    let retry_delay =
      Option.map retry_delay ~f:(fun f () ->
        f () |> Time_ns.Span.of_span_float_round_nearest)
    in
    let on_event = make_on_event ~address ~created_at ~server_name ~log ~on_event in
    create
      ~server_name
      ~on_event
      ?retry_delay
      ?random_state
      ?time_source
      ~connect
      ~address
      get_address
  ;;

  let create_with_connect_context
    ~(created_at : [%call_pos])
    ~server_name
    ?log
    ?(on_event = fun _ -> Deferred.unit)
    ?retry_delay
    ?random_state
    ?time_source
    ~connect
    ~address
    get_address
    =
    let retry_delay =
      Option.map retry_delay ~f:(fun f () ->
        f () |> Time_ns.Span.of_span_float_round_nearest)
    in
    let on_event = make_on_event ~address ~created_at ~server_name ~log ~on_event in
    create_with_connect_context
      ~server_name
      ~on_event
      ?retry_delay
      ?random_state
      ?time_source
      ~connect
      ~address
      get_address
  ;;
end

module Make (Conn : Closable) = struct
  include Make' (Persistent_connection_kernel.Default_connection_error) (Conn)
end

let create_convenience_wrapper
  ~(create : created_at:[%call_pos] -> server_name:string -> _)
  ~connection_of_rpc_connection
  ~server_name
  ~created_at
  ?log
  ?on_event
  ?retry_delay
  ?random_state
  ?time_source
  ?bind_to_address
  ?implementations
  ?max_message_size
  ?make_transport
  ?handshake_timeout
  ?heartbeat_config
  get_address
  =
  let connect host_and_port =
    let%bind.Deferred.Or_error conn =
      Async_rpc.Rpc.Connection.client
        (Tcp.Where_to_connect.of_host_and_port ?bind_to_address host_and_port)
        ?implementations
        ?max_message_size
        ?make_transport
        ?handshake_timeout
        ?heartbeat_config
        ~description:(Info.of_string ("persistent connection to " ^ server_name))
      >>| Or_error.of_exn_result
    in
    connection_of_rpc_connection conn
  in
  create
    ~server_name
    ?log
    ?on_event
    ?retry_delay
    ?random_state
    ?time_source
    ~connect
    ~address:(module Host_and_port : Address with type t = Host_and_port.t)
    ~created_at
    get_address
;;

module Versioned_rpc = struct
  include Make (Async_rpc_kernel.Persistent_connection.Versioned_rpc_conn)

  let create' ~(created_at : [%call_pos]) ~server_name =
    create_convenience_wrapper
      ~server_name
      ~create
      ~created_at
      ~connection_of_rpc_connection:Versioned_rpc.Connection_with_menu.create
  ;;
end

module Rpc = struct
  include Make (Async_rpc_kernel.Persistent_connection.Rpc_conn)

  let create' ~(created_at : [%call_pos]) ~server_name =
    create_convenience_wrapper
      ~server_name
      ~create
      ~created_at
      ~connection_of_rpc_connection:Deferred.Or_error.return
  ;;
end
