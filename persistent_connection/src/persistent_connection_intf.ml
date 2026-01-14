open! Core
open Async_kernel
open Async_log_kernel
open Async_rpc_kernel
open Async_unix
open! Import

module type Address = Persistent_connection_kernel.Address
module type Connection_error = Persistent_connection_kernel.Connection_error
module type Closable = Persistent_connection_kernel.Closable

(** The subset of [S'] that excludes the [create] functions. Useful for modules that wrap
    a persistent connection but don't want to expose the create functions. *)
module type S'_without_create = Persistent_connection_kernel.S'_without_create

module type S_without_create = S'_without_create with type conn_error := Error.t

module type S' = sig
  include S'_without_create

  val create
    :  created_at:[%call_pos]
    -> server_name:string
    -> ?log:Log.t
         (** If [~log] is supplied then all events that would be passed to [on_event] will
             be written there as well, with a "persistent-connection-to" tag value of
             [server_name], which should be the name of the server we are connecting to. *)
    -> ?on_event:('address Event.t -> unit Deferred.t)
    -> ?retry_delay:(unit -> Time_float.Span.t)
    -> ?random_state:[ `Non_random | `State of Random.State.t ]
         (** If a [~random_state] is supplied, randomization is applied to the result of
             [retry_delay] after each call; if not, no randomization will be applied. The
             default is [`State Random.State.default]. *)
    -> ?time_source:Time_source.t
    -> connect:('address -> (conn, conn_error) Result.t Deferred.t)
    -> address:(module Address with type t = 'address)
    -> (unit -> ('address, conn_error) Result.t Deferred.t)
    -> t

  (** Like [create], but passes a [Connect_context.t] to the [connect] callback. This
      allows the callback to abort the persistent connection from within the connect
      logic. *)
  val create_with_connect_context
    :  created_at:[%call_pos]
    -> server_name:string
    -> ?log:Log.t
    -> ?on_event:('address Event.t -> unit Deferred.t)
    -> ?retry_delay:(unit -> Time_float.Span.t)
    -> ?random_state:[ `Non_random | `State of Random.State.t ]
    -> ?time_source:Time_source.t
    -> connect:
         (Persistent_connection_kernel.Connect_context.t
          -> 'address
          -> (conn, conn_error) Result.t Deferred.t)
    -> address:(module Address with type t = 'address)
    -> (unit -> ('address, conn_error) Result.t Deferred.t)
    -> t
end

module type S = S' with type conn_error := Error.t

module type S_rpc = sig
  include S

  (** Like [create] but for Rpc-like connections (i.e. Async.Rpc and Async.Versioned_rpc)
      where there is an obvious default for [connect] -- with a handful of extra optional
      parameters to pass to the [Rpc.Connection] functions. *)
  val create'
    :  created_at:[%call_pos]
    -> server_name:string
    -> ?log:Log.t
    -> ?on_event:(Host_and_port.t Event.t -> unit Deferred.t)
    -> ?retry_delay:(unit -> Time_float.Span.t)
    -> ?random_state:[ `Non_random | `State of Random.State.t ]
    -> ?time_source:Time_source.t
    -> ?bind_to_address:Unix.Inet_addr.t
    -> ?implementations:Rpc.Connection.Client_implementations.t
    -> ?max_message_size:int
    -> ?make_transport:Async_rpc.Rpc.Connection.transport_maker
    -> ?handshake_timeout:Time_float.Span.t
    -> ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
    -> (unit -> Host_and_port.t Or_error.t Deferred.t)
    -> t
end

module type Persistent_connection = sig
  (** A persistent connection is one that is automatically reestablished whenever lost. *)

  module type Address = Address
  module type Closable = Closable
  module type Connection_error = Connection_error
  module type S'_without_create = S'_without_create
  module type S_without_create = S_without_create
  module type S = S
  module type S' = S'
  module type S_rpc = S_rpc

  module Connect_context = Persistent_connection_kernel.Connect_context

  module Make (Conn : Closable) :
    S with type conn = Conn.t and type t = Persistent_connection_kernel.Make(Conn).t

  module Make' (Conn_err : Connection_error) (Conn : Closable) :
    S'
    with type conn = Conn.t
     and type conn_error = Conn_err.t
     and type t = Persistent_connection_kernel.Make'(Conn_err)(Conn).t

  module Rpc :
    S_rpc
    with type conn = Rpc.Connection.t
     and type t =
      Persistent_connection_kernel.Make(Async_rpc_kernel.Persistent_connection.Rpc_conn).t

  module Versioned_rpc :
    S_rpc
    with type conn = Versioned_rpc.Connection_with_menu.t
     and type t =
      Persistent_connection_kernel.Make
        (Async_rpc_kernel.Persistent_connection.Versioned_rpc_conn)
      .t
end
