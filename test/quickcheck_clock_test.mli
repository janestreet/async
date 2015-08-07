open Core.Std
open Async.Std

val tests : (string * (unit -> unit Deferred.t)) list
