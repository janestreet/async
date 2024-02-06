open! Core
open! Async
open! Import

module type Address = Persistent_connection_kernel.Address
module type Connection_error = Persistent_connection_kernel.Connection_error
module type Closable = Persistent_connection_kernel.Closable

module type S' = sig
  include Persistent_connection_kernel.S'

  val create
    :  server_name:string
    -> ?log:Log.t
         (** If [~log] is supplied then all events that would be passed to [on_event] will be
        written there as well, with a "persistent-connection-to" tag value of
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
end

module type S = S' with type conn_error := Error.t

module type S_rpc = sig
  include S

  (** Like [create] but for Rpc-like connections (i.e. Async.Rpc and Async.Versioned_rpc)
      where there is an obvious default for [connect] -- with a handful of extra optional
      parameters to pass to the [Rpc.Connection] functions. *)
  val create'
    :  server_name:string
    -> ?log:Log.t
    -> ?on_event:(Host_and_port.t Event.t -> unit Deferred.t)
    -> ?retry_delay:(unit -> Time_float.Span.t)
    -> ?random_state:[ `Non_random | `State of Random.State.t ]
    -> ?time_source:Time_source.t
    -> ?bind_to_address:Unix.Inet_addr.t
    -> ?implementations:Rpc.Connection.Client_implementations.t
    -> ?max_message_size:int
    -> ?make_transport:Rpc.Connection.transport_maker
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
  module type S = S
  module type S' = S'
  module type S_rpc = S_rpc

  module Make (Conn : Closable) : S with type conn = Conn.t

  module Make' (Conn_err : Connection_error) (Conn : Closable) :
    S' with type conn = Conn.t and type conn_error = Conn_err.t

  module Rpc : S_rpc with type conn = Rpc.Connection.t
  module Versioned_rpc : S_rpc with type conn = Versioned_rpc.Connection_with_menu.t
end
