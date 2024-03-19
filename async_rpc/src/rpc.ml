open Core
open Import
module Transport = Rpc_transport
module Low_latency_transport = Rpc_transport_low_latency
module Any = Rpc_kernel.Any
module Description = Rpc_kernel.Description
module How_to_recognise_errors = Rpc_kernel.How_to_recognise_errors
module Implementation = Rpc_kernel.Implementation
module Implementations = Rpc_kernel.Implementations
module On_exception = Rpc_kernel.On_exception
module One_way = Rpc_kernel.One_way
module Pipe_rpc = Rpc_kernel.Pipe_rpc
module Rpc = Rpc_kernel.Rpc
module State_rpc = Rpc_kernel.State_rpc
module Pipe_close_reason = Rpc_kernel.Pipe_close_reason

module Connection = struct
  include Rpc_kernel.Connection

  let create
    ?implementations
    ~connection_state
    ?max_message_size
    ?handshake_timeout
    ?heartbeat_config
    ?description
    ?identification
    reader
    writer
    =
    create
      ?implementations
      ~connection_state
      ?handshake_timeout:
        (Option.map handshake_timeout ~f:Time_ns.Span.of_span_float_round_nearest)
      ?heartbeat_config
      ?description
      ?identification
      (Transport.of_reader_writer reader writer ?max_message_size)
  ;;

  let contains_magic_prefix reader =
    match%map
      Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
        Reader.peek_bin_prot reader contains_magic_prefix)
    with
    | Error _ | Ok `Eof -> false
    | Ok (`Ok b) -> b
  ;;

  let with_close
    ?implementations
    ?max_message_size
    ?handshake_timeout
    ?heartbeat_config
    ?description
    ~connection_state
    reader
    writer
    ~dispatch_queries
    ~on_handshake_error
    =
    with_close
      ?implementations
      ?handshake_timeout:
        (Option.map handshake_timeout ~f:Time_ns.Span.of_span_float_round_nearest)
      ?heartbeat_config
      ?description
      ~connection_state
      (Transport.of_reader_writer reader writer ?max_message_size)
      ~dispatch_queries
      ~on_handshake_error
  ;;

  let server_with_close
    ?max_message_size
    ?handshake_timeout
    ?heartbeat_config
    ?description
    reader
    writer
    ~implementations
    ~connection_state
    ~on_handshake_error
    =
    server_with_close
      ?handshake_timeout:
        (Option.map handshake_timeout ~f:Time_ns.Span.of_span_float_round_nearest)
      ?heartbeat_config
      ?description
      (Transport.of_reader_writer reader writer ?max_message_size)
      ~implementations
      ~connection_state
      ~on_handshake_error
  ;;

  let collect_errors (transport : Transport.t) ~f =
    let monitor = Transport.Writer.monitor transport.writer in
    (* don't propagate errors up, we handle them here *)
    ignore (Monitor.detach_and_get_error_stream monitor);
    choose
      [ choice (Monitor.get_next_error monitor) (fun e -> Error e)
      ; choice
          (Monitor.try_with
             ~run:`Schedule
             ~rest:`Log
             ~name:"Rpc.Connection.collect_errors"
             f)
          Fn.id
      ]
  ;;

  type transport_maker = Fd.t -> max_message_size:int -> Transport.t

  let serve_with_transport
    ?identification
    transport
    ~handshake_timeout
    ~heartbeat_config
    ~implementations
    ~description
    ~connection_state
    ~on_handshake_error
    ~client_addr
    =
    let%bind res =
      collect_errors transport ~f:(fun () ->
        match%bind
          Rpc_kernel.Connection.create
            ?handshake_timeout:
              (Option.map handshake_timeout ~f:Time_ns.Span.of_span_float_round_nearest)
            ?heartbeat_config
            ?identification
            ~implementations
            ~description
            ~connection_state
            transport
        with
        | Ok t -> close_finished t
        | Error handshake_error ->
          (match on_handshake_error with
           | `Call f -> f client_addr handshake_error
           | `Raise -> raise handshake_error
           | `Ignore -> ());
          Deferred.unit)
    in
    let%map () = Transport.close transport in
    Result.ok_exn res
  ;;

  let connection_description ?description ~server_addr ~client_addr () =
    let server_addr = (server_addr :> Socket.Address.t) in
    let client_addr = (client_addr :> Socket.Address.t) in
    let connection_description =
      Info.create_s
        [%message
          "TCP server" (server_addr : Socket.Address.t) (client_addr : Socket.Address.t)]
    in
    match description with
    | None -> connection_description
    | Some description -> Info.of_list [ description; connection_description ]
  ;;

  let default_on_handshake_error = `Ignore

  let make_serve_func
    serve_with_transport_handler
    ~implementations
    ~initial_connection_state
    ~where_to_listen
    ?max_connections
    ?backlog
    ?drop_incoming_connections
    ?time_source
    ?max_message_size
    ?make_transport
    ?handshake_timeout
    ?heartbeat_config
    ?auth
    ?(on_handshake_error = default_on_handshake_error)
    ?on_handler_error
    ?description
    ?identification
    ()
    =
    serve_with_transport_handler
      ~where_to_listen
      ?max_connections
      ?backlog
      ?drop_incoming_connections
      ?time_source
      ?max_message_size
      ?make_transport
      ?auth
      ?on_handler_error
      (fun ~client_addr ~server_addr transport ->
      serve_with_transport
        ~handshake_timeout
        ~heartbeat_config
        ~implementations
        ~description:(connection_description ?description ~server_addr ~client_addr ())
        ~connection_state:(fun conn -> initial_connection_state client_addr conn)
        ~on_handshake_error
        ~client_addr
        ?identification
        transport)
  ;;

  (* eta-expand [implementations] to avoid value restriction. *)
  let serve ~implementations = make_serve_func Rpc_transport.Tcp.serve ~implementations

  (* eta-expand [implementations] to avoid value restriction. *)
  let serve_inet ~implementations =
    make_serve_func Rpc_transport.Tcp.serve_inet ~implementations
  ;;

  let serve_unix
    ~implementations
    ~initial_connection_state
    ~where_to_listen
    ?max_connections
    ?backlog
    ?drop_incoming_connections
    ?time_source
    ?max_message_size
    ?make_transport
    ?handshake_timeout
    ?heartbeat_config
    ?auth
    ?(on_handshake_error = default_on_handshake_error)
    ?on_handler_error
    ?description
    ?identification
    ()
    =
    Rpc_transport.Tcp.serve_unix
      ~where_to_listen
      ?max_connections
      ?backlog
      ?drop_incoming_connections
      ?time_source
      ?max_message_size
      ?make_transport
      ?auth
      ?on_handler_error
      (fun ~client_addr ~server_addr peer_creds transport ->
      serve_with_transport
        ~handshake_timeout
        ~heartbeat_config
        ~implementations
        ~description:(connection_description ?description ~server_addr ~client_addr ())
        ~connection_state:(fun conn ->
          initial_connection_state client_addr peer_creds conn)
        ~on_handshake_error
        ~client_addr
        ?identification
        transport)
  ;;

  let default_handshake_timeout_float =
    Time_ns.Span.to_span_float_round_nearest
      Async_rpc_kernel.Async_rpc_kernel_private.default_handshake_timeout
  ;;

  let client'
    ?implementations
    ?max_message_size
    ?make_transport
    ?handshake_timeout:(handshake_timeout_float = default_handshake_timeout_float)
    ?heartbeat_config
    ?description
    ?identification
    where_to_connect
    =
    let handshake_timeout =
      Time_ns.Span.of_span_float_round_nearest handshake_timeout_float
    in
    let finish_handshake_by = Time_ns.add (Time_ns.now ()) handshake_timeout in
    match%bind
      Rpc_transport.Tcp.connect
        ?max_message_size
        ?make_transport
        ~tcp_connect_timeout:handshake_timeout
        where_to_connect
    with
    | Error _ as error -> return error
    | Ok (transport, sock_peername) ->
      let description =
        match description with
        | None ->
          Info.create
            "Client connected via TCP"
            where_to_connect
            [%sexp_of: _ Tcp.Where_to_connect.t]
        | Some desc ->
          Info.of_list
            [ desc
            ; Info.create_s
                [%message "via TCP" ~_:(where_to_connect : _ Tcp.Where_to_connect.t)]
            ]
      in
      let handshake_timeout = Time_ns.diff finish_handshake_by (Time_ns.now ()) in
      let%bind rpc_connection =
        match implementations with
        | None ->
          let (T { connection_state; implementations }) =
            Client_implementations.null ()
          in
          Rpc_kernel.Connection.create
            transport
            ~handshake_timeout
            ?heartbeat_config
            ?identification
            ~implementations
            ~description
            ~connection_state
        | Some (Client_implementations.T { connection_state; implementations }) ->
          Rpc_kernel.Connection.create
            transport
            ~handshake_timeout
            ?heartbeat_config
            ?identification
            ~implementations
            ~description
            ~connection_state
      in
      (match rpc_connection with
       | Ok t -> return (Ok (sock_peername, t))
       | Error _ as error ->
         let%bind () = Transport.close transport in
         return error)
  ;;

  let client
    ?implementations
    ?max_message_size
    ?make_transport
    ?handshake_timeout
    ?heartbeat_config
    ?description
    ?identification
    where_to_connect
    =
    client'
      ?implementations
      ?max_message_size
      ?make_transport
      ?handshake_timeout
      ?heartbeat_config
      ?description
      ?identification
      where_to_connect
    >>|? snd
  ;;

  let with_client'
    ?implementations
    ?max_message_size
    ?make_transport
    ?handshake_timeout
    ?heartbeat_config
    ?description
    ?identification
    where_to_connect
    f
    =
    client'
      ?implementations
      ?max_message_size
      ?make_transport
      ?handshake_timeout
      ?heartbeat_config
      ?description
      ?identification
      where_to_connect
    >>=? fun (remote_server, t) ->
    let%bind result =
      Monitor.try_with ~run:`Schedule ~rest:`Log (fun () -> f ~remote_server t)
    in
    let%map () = close t ~reason:(Info.of_string "Rpc.Connection.with_client finished") in
    result
  ;;

  let with_client
    ?implementations
    ?max_message_size
    ?make_transport
    ?handshake_timeout
    ?heartbeat_config
    ?description
    ?identification
    where_to_connect
    f
    =
    with_client'
      ?implementations
      ?max_message_size
      ?make_transport
      ?handshake_timeout
      ?heartbeat_config
      ?description
      ?identification
      where_to_connect
      (fun ~remote_server:_ -> f)
  ;;
end

module For_debugging = struct
  let default_path =
    lazy ("/dev/shm/rpc-message-reader-errors__pid_" ^ Pid.to_string (Core_unix.getpid ()))
  ;;

  let path_override = ref None

  let dump_deserialization_error buf ~pos =
    (* Dump the message synchronously, this is this ok as this is being used to debug why
       the application is seeing this error, and is likely about to shut down from the
       failed RPC anyway. *)
    let open_file () =
      let path =
        match !path_override with
        | None -> force default_path
        | Some path -> path
      in
      ( Core_unix.openfile
          path
          ~mode:
            [ O_CREAT
            ; O_WRONLY
            ; (let ensure_we_dont_fill_up_filesystem_if_program_is_emitting_many_of_these =
                 Core_unix.O_TRUNC
               in
               ensure_we_dont_fill_up_filesystem_if_program_is_emitting_many_of_these)
            ]
      , path )
    in
    let fd, path =
      try open_file () with
      | exn ->
        if Option.is_none !path_override
        then raise exn
        else (
          path_override := None;
          open_file ())
    in
    let result =
      match Bigstring_unix.really_write fd buf with
      | () ->
        sprintf "Dump of buffer written to %s. This message begins at offset %d" path pos
      | exception e ->
        sprintf
          "Tried and failed to dump internal buffer to %s (offset %d): %s"
          path
          pos
          (Exn.to_string e)
    in
    Core_unix.close fd;
    result
  ;;

  let enable_dumping_buffers_on_deserialization_errors ?path_override:override () =
    (match override with
     | None -> ()
     | Some override -> path_override := Some override);
    Async_rpc_kernel.Async_rpc_kernel_private.Util.dumper_for_deserialization_errors
      := dump_deserialization_error
  ;;

  let () =
    match Sys.getenv "ASYNC_RPC_DEBUG_DUMP_DESERIALIZATION_ERRORS" with
    | None ->
      (* support for old env var *)
      (match Sys.getenv "ASYNC_RPC_DEBUG_DUMP_MESSAGE_LENGTH_ERRORS" with
       | None | Some "" -> ()
       | Some _ -> enable_dumping_buffers_on_deserialization_errors ())
    | Some dump_to ->
      let path_override =
        match dump_to with
        | "" | "1" -> None
        | dump_to when String.contains dump_to '/' -> Some dump_to
        | dump_to -> Some ("/dev/shm" ^/ dump_to)
      in
      enable_dumping_buffers_on_deserialization_errors ?path_override ()
  ;;
end
