(** This module just re-exports lots of modules from [Async_rpc_kernel] and adds some
    Unix-specific wrappers in [Connection] (for using [Reader], [Writer], and [Tcp]).  For
    documentation, see {{!Async_rpc_kernel.Rpc}[Rpc]} and
    {{!Async_rpc_kernel__.Connection_intf}[Connection_intf]} in the
    {{!Async_rpc_kernel}[Async_rpc_kernel]} library.
*)

open! Core
open! Import
module Transport = Rpc_transport
module Low_latency_transport = Rpc_transport_low_latency
module Any = Rpc_kernel.Any
module Description = Rpc_kernel.Description
module On_exception = Rpc_kernel.On_exception
module Implementation = Rpc_kernel.Implementation
module Implementations = Rpc_kernel.Implementations
module One_way = Rpc_kernel.One_way
module Pipe_rpc = Rpc_kernel.Pipe_rpc
module Rpc = Rpc_kernel.Rpc
module State_rpc = Rpc_kernel.State_rpc
module Pipe_close_reason = Rpc_kernel.Pipe_close_reason

module Connection : sig
  include module type of struct
    include Rpc_kernel.Connection
  end


  (** These functions are mostly the same as the ones with the same names in
      [Async_rpc_kernel.Rpc.Connection]; see [Connection_intf] in that library for
      documentation. The differences are that:

      - they take an [Async_unix.Reader.t], [Async_unix.Writer.t] and
        [max_message_size] instead of a [Transport.t]
      - they use [Time] instead of [Time_ns] *)
  val create
    :  ?implementations:'s Implementations.t
    -> connection_state:(t -> 's)
    -> ?max_message_size:int
    -> ?handshake_timeout:Time_float.Span.t
    -> ?heartbeat_config:Heartbeat_config.t
    -> ?description:Info.t
    -> Reader.t
    -> Writer.t
    -> (t, Exn.t) Result.t Deferred.t

  (** As of Feb 2017, the RPC protocol started to contain a magic number so that one can
      identify RPC communication.  The bool returned by [contains_magic_prefix] says
      whether this magic number was observed.

      This operation is a "peek" that does not advance any pointers associated with the
      reader.  In particular, it makes sense to call [create] on a reader after calling
      this function.
  *)
  val contains_magic_prefix : Reader.t -> bool Deferred.t

  val with_close
    :  ?implementations:'s Implementations.t
    -> ?max_message_size:int
    -> ?handshake_timeout:Time_float.Span.t
    -> ?heartbeat_config:Heartbeat_config.t
    -> ?description:Info.t
    -> connection_state:(t -> 's)
    -> Reader.t
    -> Writer.t
    -> dispatch_queries:(t -> 'a Deferred.t)
    -> on_handshake_error:[ `Raise | `Call of Exn.t -> 'a Deferred.t ]
    -> 'a Deferred.t

  val server_with_close
    :  ?max_message_size:int
    -> ?handshake_timeout:Time_float.Span.t
    -> ?heartbeat_config:Heartbeat_config.t
    -> ?description:Info.t
    -> Reader.t
    -> Writer.t
    -> implementations:'s Implementations.t
    -> connection_state:(t -> 's)
    -> on_handshake_error:[ `Raise | `Ignore | `Call of Exn.t -> unit Deferred.t ]
    -> unit Deferred.t

  (** A function creating a transport from a file descriptor. It is responsible for
      setting the low-level parameters of the underlying transport.

      For instance to set up a transport using [Async.{Reader,Writer}] and set a buffer
      age limit on the writer, you can pass this to the functions of this module:

      {[
        ~make_transport:(fun fd ~max_message_size ->
          Rpc.Transport.of_fd fd ~max_message_size ~buffer_age_limit:`Unlimited)
      ]}
  *)
  type transport_maker = Fd.t -> max_message_size:int -> Transport.t


  (** [serve implementations ~port ?on_handshake_error ()] starts a server with the given
      implementation on [port].  The optional auth function will be called on all incoming
      connections with the address info of the client and will disconnect the client
      immediately if it returns false.  This auth mechanism is generic and does nothing
      other than disconnect the client -- any logging or record of the reasons is the
      responsibility of the auth function itself.
  *)
  val serve
    :  implementations:'s Implementations.t
    -> initial_connection_state:('address -> t -> 's)
    -> where_to_listen:('address, 'listening_on) Tcp.Where_to_listen.t
    -> ?max_connections:int
    -> ?backlog:int
    -> ?drop_incoming_connections:bool
    -> ?time_source:[> read ] Time_source.T1.t
    -> ?max_message_size:int
    -> ?make_transport:transport_maker
    -> ?handshake_timeout:Time_float.Span.t
    -> ?heartbeat_config:Heartbeat_config.t
    -> ?auth:('address -> bool) (** default is [`Ignore] *)
    -> ?on_handshake_error:[ `Raise | `Ignore | `Call of 'address -> exn -> unit ]
    (** default is [`Ignore] *)
    -> ?on_handler_error:[ `Raise | `Ignore | `Call of 'address -> exn -> unit ]
    -> unit
    -> ('address, 'listening_on) Tcp.Server.t Deferred.t

  (** As [serve], but only accepts IP addresses, not Unix sockets; returns server
      immediately rather than asynchronously. *)
  val serve_inet
    :  implementations:'s Implementations.t
    -> initial_connection_state:(Socket.Address.Inet.t -> t -> 's)
    -> where_to_listen:Tcp.Where_to_listen.inet
    -> ?max_connections:int
    -> ?backlog:int
    -> ?drop_incoming_connections:bool
    -> ?time_source:[> read ] Time_source.T1.t
    -> ?max_message_size:int
    -> ?make_transport:transport_maker
    -> ?handshake_timeout:Time_float.Span.t
    -> ?heartbeat_config:Heartbeat_config.t
    -> ?auth:(Socket.Address.Inet.t -> bool) (** default is [`Ignore] *)
    -> ?on_handshake_error:
         [ `Raise | `Ignore | `Call of Socket.Address.Inet.t -> exn -> unit ]
    (** default is [`Ignore] *)
    -> ?on_handler_error:
         [ `Raise | `Ignore | `Call of Socket.Address.Inet.t -> exn -> unit ]
    -> unit
    -> (Socket.Address.Inet.t, int) Tcp.Server.t

  (** [client where_to_connect ()] connects to the server at [where_to_connect] and
      returns the connection or an Error if a connection could not be made. It is the
      responsibility of the caller to eventually call [close].

      In [client] and [with_client], the [handshake_timeout] encompasses both the TCP
      connection timeout and the timeout for this module's own handshake. *)
  val client
    :  ?implementations:_ Client_implementations.t
    -> ?max_message_size:int
    -> ?make_transport:transport_maker
    -> ?handshake_timeout:Time_float.Span.t
    -> ?heartbeat_config:Heartbeat_config.t
    -> ?description:Info.t
    -> _ Tcp.Where_to_connect.t
    -> (t, Exn.t) Result.t Deferred.t

  (** Similar to [client], but additionally expose the [Socket.Address.t] of the RPC
      server that we connected to. *)
  val client'
    :  ?implementations:_ Client_implementations.t
    -> ?max_message_size:int
    -> ?make_transport:transport_maker
    -> ?handshake_timeout:Time_float.Span.t
    -> ?heartbeat_config:Heartbeat_config.t
    -> ?description:Info.t
    -> 'address Tcp.Where_to_connect.t
    -> ('address * t, Exn.t) Result.t Deferred.t


  (** [with_client where_to_connect f] connects to the server at [where_to_connect] and
      runs f until an exception is thrown or until the returned Deferred is fulfilled.

      NOTE:  As with [with_close], you should be careful when using this with [Pipe_rpc].
      See [with_close] for more information. *)
  val with_client
    :  ?implementations:_ Client_implementations.t
    -> ?max_message_size:int
    -> ?make_transport:transport_maker
    -> ?handshake_timeout:Time_float.Span.t
    -> ?heartbeat_config:Heartbeat_config.t
    -> _ Tcp.Where_to_connect.t
    -> (t -> 'a Deferred.t)
    -> ('a, Exn.t) Result.t Deferred.t

  (** Similar to [with_client], but additionally expose the [Socket.Address.t] of the RPC
      server that we connected to. *)
  val with_client'
    :  ?implementations:_ Client_implementations.t
    -> ?max_message_size:int
    -> ?make_transport:transport_maker
    -> ?handshake_timeout:Time_float.Span.t
    -> ?heartbeat_config:Heartbeat_config.t
    -> ?description:Info.t
    -> 'transport Tcp.Where_to_connect.t
    -> (remote_server:'transport -> t -> 'a Deferred.t)
    -> ('a, Exn.t) Result.t Deferred.t
end

module For_debugging : sig
  (** If called, then when a message is not the expected length, or when bin_io
      deserialization fails, dump the buffer being read to a file under /dev/shm for later
      debugging. The default is to not dump any buffers in these cases. The file may be
      overridden by passing [path_override]

      This is intended to be enabled in the background to debug applications that are
      occasionally seeing apparent message corruption, to help debug. It can also be
      enabled by setting the ASYNC_RPC_DEBUG_DUMP_DESERIALIZATION_ERRORS environment
      variable, in which case errors will be written to the path specified by the env var
      (relative to /dev/shm if the path contains no ‘/’). *)
  val enable_dumping_buffers_on_deserialization_errors
    :  ?path_override:Filename.t
    -> unit
    -> unit
end
