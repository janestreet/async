open! Async_unix
open! Async

type t

val create
  :  level:Log.Level.t
  -> output:Log.Output.t list
  -> on_error:[ `Raise | `Call of Core.Error.t -> unit ]
  -> ?time_source:Async_kernel.Synchronous_time_source.t
  -> ?transform:(Log.Message.t -> Log.Message.t)
  -> ?listener_file_path:string
  -> unit
  -> t

module Make_global_dynamic () : Async_unix.Log.Global_intf

val debug_s
  :  ?time:Core.Time_float.t
  -> ?tags:(string * string) list
  -> Lexing.position
  -> t
  -> Sexp.t
  -> unit

val info_s
  :  ?time:Core.Time_float.t
  -> ?tags:(string * string) list
  -> Lexing.position
  -> t
  -> Sexp.t
  -> unit

val error_s
  :  ?time:Core.Time_float.t
  -> ?tags:(string * string) list
  -> Lexing.position
  -> t
  -> Sexp.t
  -> unit

val set_control_file : string -> unit Deferred.t

module Global_dynamic_log : sig
  include Async_unix.Log.Global_intf

  val set_listener : string -> unit Deferred.t
end