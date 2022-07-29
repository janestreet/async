open! Core

module T = struct
  type t =
    { file : string
    ; line : int
    }
  [@@deriving compare, sexp, hash]

  let create (file : string) (line : int) : t = { file; line }

  let create_t (lexing : Lexing.position) : t =
    { file = lexing.pos_fname; line = lexing.pos_lnum }
  ;;
end

include T
module Map = Map.Make (T)
