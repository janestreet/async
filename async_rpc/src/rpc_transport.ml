open Core
open Import
module Kernel_transport = Rpc_kernel.Transport
module Header = Kernel_transport.Header
module Handler_result = Kernel_transport.Handler_result
module Send_result = Kernel_transport.Send_result

let max_message_size_env_var = "ASYNC_RPC_MAX_MESSAGE_SIZE"

let max_message_size_from_environment =
  lazy
    (Option.try_with_join (fun () ->
       Sys.getenv max_message_size_env_var |> Option.map ~f:Int.of_string))
;;

let aux_effective_max_message_size ~max_message_size_from_environment ~proposed_max =
  let default =
    (* unfortunately, copied from reader0.ml *)
    100 * 1024 * 1024
  in
  match proposed_max, max_message_size_from_environment with
  | None, None -> default
  | Some x, None | None, Some x -> x
  | Some x, Some y -> Int.max x y
;;

let%expect_test " " =
  let test ~max_message_size_from_environment =
    List.iter
      [ None; Some 1; Some (200 * 1024 * 1024) ]
      ~f:(fun proposed_max ->
        let effective_max =
          aux_effective_max_message_size ~max_message_size_from_environment ~proposed_max
        in
        print_s [%message (proposed_max : int option) (effective_max : int)])
  in
  test ~max_message_size_from_environment:None;
  [%expect
    {|
    ((proposed_max ()) (effective_max 104857600))
    ((proposed_max (1)) (effective_max 1))
    ((proposed_max (209715200)) (effective_max 209715200))
    |}];
  test ~max_message_size_from_environment:(Some 1024);
  [%expect
    {|
    ((proposed_max ()) (effective_max 1024))
    ((proposed_max (1)) (effective_max 1024))
    ((proposed_max (209715200)) (effective_max 209715200))
    |}];
  test ~max_message_size_from_environment:(Some (300 * 1024 * 1024));
  [%expect
    {|
    ((proposed_max ()) (effective_max 314572800))
    ((proposed_max (1)) (effective_max 314572800))
    ((proposed_max (209715200)) (effective_max 314572800))
    |}]
;;

let effective_max_message_size ~proposed_max =
  let max_message_size_from_environment = force max_message_size_from_environment in
  aux_effective_max_message_size ~max_message_size_from_environment ~proposed_max
;;

module With_limit : sig
  type 'a t = private
    { t : 'a
    ; max_message_size : int
    ; mutable total_bytes : Int63.t
    }
  [@@deriving sexp_of]

  val create : 'a -> max_message_size:int option -> 'a t
  val message_size_ok : _ t -> payload_len:int -> bool
  val check_message_size : _ t -> payload_len:int -> unit
  val incr_total_bytes : _ t -> int -> unit
end = struct
  type 'a t =
    { t : 'a
    ; max_message_size : int
    ; mutable total_bytes : Int63.t
    }
  [@@deriving sexp_of]

  let create t ~max_message_size =
    let max_message_size = effective_max_message_size ~proposed_max:max_message_size in
    if max_message_size < 0
    then
      failwithf
        "Rpc_transport.With_limit.create got negative max message size: %d"
        max_message_size
        ();
    { t; max_message_size; total_bytes = Int63.zero }
  ;;

  let message_size_ok t ~payload_len =
    payload_len >= 0 && payload_len <= t.max_message_size
  ;;

  let check_message_size t ~payload_len =
    if not (message_size_ok t ~payload_len)
    then
      failwiths
        ~here:[%here]
        [%string
          "Rpc_transport: message is too large or has negative size. Try increasing the \
           size limit by setting the %{max_message_size_env_var} env var"]
        (`Message_size payload_len, `Max_message_size t.max_message_size)
        [%sexp_of: [ `Message_size of int ] * [ `Max_message_size of int ]]
  ;;

  let incr_total_bytes t bytes = t.total_bytes <- Int63.(t.total_bytes + of_int bytes)
end

module Unix_reader = struct
  open With_limit

  type t = Reader.t With_limit.t [@@deriving sexp_of]

  let create ~reader ~max_message_size = With_limit.create reader ~max_message_size
  let close t = Reader.close t.t
  let is_closed t = Reader.is_closed t.t
  let bytes_read t = t.total_bytes

  let all_unit_then_return l ret_val =
    match l with
    | [] -> return ret_val (* avoid deferred operations in the common case *)
    | _ ->
      let%map () = Deferred.all_unit l in
      ret_val
  ;;

  let read_forever t ~on_message ~on_end_of_batch =
    let finish_loop ~consumed ~need ~wait_before_reading =
      on_end_of_batch ();
      all_unit_then_return wait_before_reading (`Consumed (consumed, `Need need))
    in
    let rec loop buf ~pos ~len ~consumed ~wait_before_reading =
      if len < Header.length
      then finish_loop ~consumed ~need:Header.length ~wait_before_reading
      else (
        let payload_len = Header.unsafe_get_payload_length buf ~pos in
        let total_len = Header.length + payload_len in
        With_limit.check_message_size t ~payload_len;
        if len < total_len
        then finish_loop ~consumed ~need:total_len ~wait_before_reading
        else (
          let consumed = consumed + total_len in
          incr_total_bytes t payload_len;
          let result : _ Handler_result.t =
            on_message buf ~pos:(pos + Header.length) ~len:payload_len
          in
          match result with
          | Stop x ->
            all_unit_then_return wait_before_reading (`Stop_consumed (x, consumed))
          | Continue ->
            loop
              buf
              ~pos:(pos + total_len)
              ~len:(len - total_len)
              ~consumed
              ~wait_before_reading
          | Wait d ->
            let wait_before_reading =
              if Deferred.is_determined d
              then wait_before_reading
              else d :: wait_before_reading
            in
            loop
              buf
              ~pos:(pos + total_len)
              ~len:(len - total_len)
              ~consumed
              ~wait_before_reading))
    in
    let handle_chunk buf ~pos ~len =
      loop buf ~pos ~len ~consumed:0 ~wait_before_reading:[]
    in
    match%map Reader.read_one_chunk_at_a_time t.t ~handle_chunk with
    | `Eof | `Eof_with_unconsumed_data _ -> Error `Eof
    | `Stopped x -> Ok x
  ;;
end

module Unix_writer = struct
  open With_limit

  type t = Writer.t With_limit.t [@@deriving sexp_of]

  let create ~writer ~max_message_size =
    (* Prevent exceptions in the writer when the other side disconnects. Note that "stale
       data in buffer" exceptions are not an issue when the consumer leaves, since
       [Rpc_kernel.Connection] takes care of closing the transport when the consumer
       leaves. *)
    Writer.set_raise_when_consumer_leaves writer false;
    With_limit.create writer ~max_message_size
  ;;

  let close t = Writer.close t.t
  let is_closed t = Writer.is_closed t.t
  let monitor t = Writer.monitor t.t
  let bytes_to_write t = Writer.bytes_to_write t.t
  let bytes_written t = t.total_bytes
  let stopped t = Deferred.any [ Writer.close_started t.t; Writer.consumer_left t.t ]
  let flushed t = Writer.flushed t.t
  let ready_to_write = flushed

  let bin_write_payload_length buf ~pos x =
    Header.unsafe_set_payload_length buf ~pos x;
    pos + Header.length
  ;;

  let send_bin_prot_internal t (bin_writer : _ Bin_prot.Type_class.writer) x ~followup_len
    : _ Send_result.t
    =
    if not (Writer.is_closed t.t)
    then (
      let data_len = bin_writer.size x in
      let payload_len = data_len + followup_len in
      if message_size_ok t ~payload_len
      then (
        incr_total_bytes t payload_len;
        Writer.write_bin_prot_no_size_header
          t.t
          ~size:Header.length
          bin_write_payload_length
          payload_len;
        Writer.write_bin_prot_no_size_header t.t ~size:data_len bin_writer.write x;
        Sent { result = (); bytes = payload_len })
      else Message_too_big { size = payload_len; max_message_size = t.max_message_size })
    else Closed
  ;;

  let send_bin_prot t bin_writer x = send_bin_prot_internal t bin_writer x ~followup_len:0

  let send_bin_prot_and_bigstring t bin_writer x ~buf ~pos ~len : _ Send_result.t =
    match send_bin_prot_internal t bin_writer x ~followup_len:len with
    | Sent { result = (); bytes = (_ : int) } as result ->
      Writer.write_bigstring t.t buf ~pos ~len;
      result
    | error -> error
  ;;

  let send_bin_prot_and_bigstring_non_copying t bin_writer x ~buf ~pos ~len
    : _ Send_result.t
    =
    match send_bin_prot_internal t bin_writer x ~followup_len:len with
    | Sent { result = (); bytes } ->
      Writer.schedule_bigstring t.t buf ~pos ~len;
      Sent
        { result = Writer.flushed t.t
        ; bytes (* The response from [send_bin_prot_internal] includes [followup_len] *)
        }
    | (Closed | Message_too_big _) as r -> r
  ;;
end

module Reader = struct
  include Kernel_transport.Reader

  let of_reader ?max_message_size reader =
    pack (module Unix_reader) (Unix_reader.create ~reader ~max_message_size)
  ;;
end

module Writer = struct
  include Kernel_transport.Writer

  let of_writer ?max_message_size writer =
    pack (module Unix_writer) (Unix_writer.create ~writer ~max_message_size)
  ;;
end

type t = Kernel_transport.t =
  { reader : Reader.t
  ; writer : Writer.t
  }
[@@deriving sexp_of]

let close = Kernel_transport.close

let of_reader_writer ?max_message_size reader writer =
  { reader = Reader.of_reader ?max_message_size reader
  ; writer = Writer.of_writer ?max_message_size writer
  }
;;

let of_fd ?buffer_age_limit ?reader_buffer_size ?writer_buffer_size ~max_message_size fd =
  of_reader_writer
    ~max_message_size
    (Async_unix.Reader.create ?buf_len:reader_buffer_size fd)
    (Async_unix.Writer.create ?buf_len:writer_buffer_size ?buffer_age_limit fd)
;;

module Tcp = struct
  let default_transport_maker fd ~max_message_size = of_fd fd ~max_message_size

  let make_serve_func_with_fd
    tcp_creator
    ~where_to_listen
    ?max_connections
    ?backlog
    ?drop_incoming_connections
    ?time_source
    ?max_message_size:proposed_max
    ?(make_transport = default_transport_maker)
    ?(auth = fun _ -> true)
    ?(on_handler_error = `Ignore)
    handle_transport
    =
    tcp_creator
      ?max_connections
      ?max_accepts_per_batch:None
      ?backlog
      ?drop_incoming_connections
      ?socket:None
      ?time_source
      ~on_handler_error
      where_to_listen
      (fun client_addr socket ->
      match auth client_addr with
      | false -> return ()
      | true ->
        let max_message_size = effective_max_message_size ~proposed_max in
        let fd = Socket.fd socket in
        let transport = make_transport ~max_message_size fd in
        let%bind result =
          Monitor.try_with ~run:`Schedule ~rest:`Raise (fun () ->
            handle_transport
              fd
              ~client_addr
              ~server_addr:(Socket.getsockname socket)
              transport)
        in
        let%bind () = close transport in
        (match result with
         | Ok () -> return ()
         | Error exn -> raise exn))
  ;;

  let make_serve_func
    tcp_creator
    ~where_to_listen
    ?max_connections
    ?backlog
    ?drop_incoming_connections
    ?time_source
    ?max_message_size
    ?make_transport
    ?auth
    ?on_handler_error
    handle_transport
    =
    make_serve_func_with_fd
      tcp_creator
      ~where_to_listen
      ?max_connections
      ?backlog
      ?drop_incoming_connections
      ?time_source
      ?max_message_size
      ?make_transport
      ?auth
      ?on_handler_error
      (fun (_ : Fd.t) ~client_addr ~server_addr transport ->
      handle_transport ~client_addr ~server_addr transport)
  ;;

  (* eta-expand [where_to_listen] to avoid value restriction. *)
  let serve ~where_to_listen = make_serve_func Tcp.Server.create_sock ~where_to_listen

  (* eta-expand [where_to_listen] to avoid value restriction. *)
  let serve_inet ~where_to_listen =
    make_serve_func Tcp.Server.create_sock_inet ~where_to_listen
  ;;

  let serve_unix
    ~(where_to_listen : Tcp.Where_to_listen.unix)
    ?max_connections
    ?backlog
    ?drop_incoming_connections
    ?time_source
    ?max_message_size
    ?make_transport
    ?auth
    ?on_handler_error
    handle_transport
    =
    make_serve_func_with_fd
      Tcp.Server.create_sock
      ~where_to_listen
      ?max_connections
      ?backlog
      ?drop_incoming_connections
      ?time_source
      ?max_message_size
      ?make_transport
      ?auth
      ?on_handler_error
      (fun fd ~client_addr ~server_addr transport ->
      let peer_credentials =
        Or_error.try_with (fun () ->
          (ok_exn Linux_ext.peer_credentials) (Fd.file_descr_exn fd))
        |> Or_error.tag ~tag:"Error getting peer credentials of unix socket"
        |> ok_exn
      in
      handle_transport ~client_addr ~server_addr peer_credentials transport)
  ;;

  let connect
    ?max_message_size:proposed_max
    ?(make_transport = default_transport_maker)
    ?(tcp_connect_timeout =
      Async_rpc_kernel.Async_rpc_kernel_private.default_handshake_timeout)
    where_to_connect
    =
    let%bind sock =
      Monitor.try_with ~run:`Schedule ~rest:`Log (fun () ->
        Tcp.connect_sock
          ~timeout:(Time_ns.Span.to_span_float_round_nearest tcp_connect_timeout)
          where_to_connect)
    in
    match sock with
    | Error _ as error -> return error
    | Ok sock ->
      (match Socket.getpeername sock with
       | exception exn_could_be_raised_if_the_socket_is_diconnected_now ->
         Socket.shutdown sock `Both;
         don't_wait_for (Unix.close (Socket.fd sock));
         return (Error exn_could_be_raised_if_the_socket_is_diconnected_now)
       | sock_peername ->
         let max_message_size = effective_max_message_size ~proposed_max in
         let transport = make_transport (Socket.fd sock) ~max_message_size in
         return (Ok (transport, sock_peername)))
  ;;
end
