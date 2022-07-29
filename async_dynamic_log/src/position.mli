open! Core

type t =
  { file : string
  ; line : int
  }
[@@deriving compare, sexp, hash]

val create : string -> int -> t
val create_t : Lexing.position -> t

module Map : Map.S