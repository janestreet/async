open Core.Std
open Async.Std

module Expect : sig
  type t with sexp_of

  val ok : t
  val error : t
  val no_output : t

  val custom : string -> (unit -> unit Or_error.t Deferred.t) -> t

  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
  val not : t -> t
end

val add_test
  :  Source_code_position.t
  -> Expect.t
  -> (unit -> unit Deferred.t)
  -> unit

val main : unit -> never_returns
