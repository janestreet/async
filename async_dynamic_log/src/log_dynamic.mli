module Global_dynamic : sig
  open! Async
  include Async_unix.Log.Global_intf

  val set_listener : string -> unit Deferred.t
end
