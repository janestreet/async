open! Core

module T : sig
  type t =
    { file : string
    ; line : int
    }
  [@@deriving compare, sexp, hash]

  val compare : t -> t -> int
  val create : string -> int -> t
  val create_t : Lexing.position -> t
end

type t =
  { file : string
  ; line : int
  }
[@@deriving compare, sexp, hash]

val create : string -> int -> t
val create_t : Lexing.position -> t
val compare : t -> t -> int

type comparator_witness = Base.Comparator.Make(T).comparator_witness

val comparator : (t, comparator_witness) Comparator.t
