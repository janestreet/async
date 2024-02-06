open! Core
open! Async
open! Import
open! Require_explicit_time_source
include Persistent_connection_intf

module Make' (Conn_err : Connection_error) (Conn : Closable) = struct
  include Persistent_connection_kernel.Make' (Conn_err) (Conn)

  let create
    (type address)
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
    let (module Address : Address with type t = address) = address in
    let retry_delay =
      Option.map retry_delay ~f:(fun f () ->
        f () |> Time_ns.Span.of_span_float_round_nearest)
    in
    let on_event event =
      Option.iter log ~f:(fun log ->
        if Log.would_log log (Some (Persistent_connection_kernel.Event.log_level event))
        then
          [%log.sexp
            log (event : Address.t Event.t) [@@tags
                                              [ "persistent-connection-to", server_name ]]
                                            [@@level
                                              Some
                                                (Persistent_connection_kernel.Event
                                                 .log_level
                                                   event)]]);
      on_event event
    in
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
end

module Make (Conn : Closable) = struct
  include
    Make'
      (struct
        type t = Error.t [@@deriving equal, sexp_of]

        let to_error e = e
        let of_exception_error e = e
      end)
      (Conn)
end

let create_convenience_wrapper
  ~create
  ~connection_of_rpc_connection
  ~server_name
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
      Rpc.Connection.client
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
    get_address
;;

module Versioned_rpc = struct
  include Make (struct
    type t = Versioned_rpc.Connection_with_menu.t

    let rpc_connection = Versioned_rpc.Connection_with_menu.connection
    let close t = Rpc.Connection.close (rpc_connection t)
    let is_closed t = Rpc.Connection.is_closed (rpc_connection t)
    let close_finished t = Rpc.Connection.close_finished (rpc_connection t)
  end)

  let create' ~server_name =
    create_convenience_wrapper
      ~server_name
      ~create
      ~connection_of_rpc_connection:Versioned_rpc.Connection_with_menu.create
  ;;
end

module Rpc = struct
  include Make (struct
    type t = Rpc.Connection.t

    let close t = Rpc.Connection.close t
    let is_closed t = Rpc.Connection.is_closed t
    let close_finished t = Rpc.Connection.close_finished t
  end)

  let create' ~server_name =
    create_convenience_wrapper
      ~server_name
      ~create
      ~connection_of_rpc_connection:Deferred.Or_error.return
  ;;
end
