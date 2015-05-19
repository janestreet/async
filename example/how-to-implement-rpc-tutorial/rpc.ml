open Core.Std
open Async.Std

module type Z = sig

  module Path : Identifiable

  module Filesystem : sig

    val listdir     : Path.t -> Path.t list Or_error.t
    val read_file   : Path.t -> string Or_error.t
    val move        : Path.t -> Path.t -> unit Or_error.t
    val put_file    : Path.t -> string -> unit Or_error.t
    val file_size   : Path.t -> int Or_error.t
    val file_exists : Path.t -> bool
  end

  module Connection : sig
    type t
    val create : Unix.Inet_addr.t -> port:int -> t Deferred.t
    val close  : t -> unit
    val send   : t -> Sexp.t -> unit Deferred.t
    val recv   : t -> Sexp.t Deferred.t
  end

  module Request : sig
    type t =
      | Listdir     of Path.t
      | Read_file   of Path.t
      | Move        of Path.t * Path.t
      | Put_file    of Path.t * string
      | File_size   of Path.t
      | File_exists of Path.t
    with sexp
  end

  module Response : sig
    type t =
      | Ok
      | Error
      | File_size   of int
      | Contents    of string
      | Paths       of Path.t list
      | File_exists of bool
    with sexp
  end

end

module M(Z:Z) = struct
  open Z

  let handle_query conn =
    Connection.recv conn
    >>= fun sexp ->
    let module Fs = Filesystem in
    let with_err x wrap : Response.t =
      match x with
      | Error _ -> Error
      | Ok x -> wrap x
    in
    let response : Response.t =
      match Request.t_of_sexp sexp with
      | Listdir p      -> with_err (Fs.listdir p)        (fun x -> Paths x)
      | Read_file p    -> with_err (Fs.read_file p)      (fun x -> Contents x)
      | Move (p1,p2)   -> with_err (Fs.move p1 p2)       (fun () -> Ok)
      | Put_file (p,c) -> with_err (Fs.put_file p c)     (fun () -> Ok)
      | File_size p    -> with_err (Fs.file_size p)      (fun x -> File_size x)
      | File_exists p  -> File_exists (Fs.file_exists p)
    in
    Connection.send conn (Response.sexp_of_t response)
  ;;


  let rpc_listdir conn path =
    Connection.send conn (Request.sexp_of_t (Listdir path))
    >>= fun () ->
    Connection.recv conn
    >>= fun sexp ->
    match Response.t_of_sexp sexp with
    | Paths x -> return (Ok x)
    | Error -> return (Or_error.error_string "Failed!")
    | _ -> assert false (* we didn't mean that here *)


  (* Another approach..... *)

  module Embedding = struct
    type 'a t = { inj: 'a -> Sexp.t
                ; prj: Sexp.t -> 'a
                }
  end

  module Rpc = struct
    type ('a,'b) t =
      { name     : string
      ; request  : 'a Embedding.t
      ; response : 'b Embedding.t
      }
  end


  let listdir_rpc =
    { Rpc.
      name = "listdir"
    ; request = { inj = <:sexp_of<Path.t>>
                ; prj = <:of_sexp<Path.t>>
                }
    ; response = { inj = <:sexp_of<Path.t list Or_error.t>>
                 ; prj = <:of_sexp<Path.t list Or_error.t>>
                 }
    }

  let read_file_rpc =
    { Rpc.
      name = "read_file"
    ; request = { inj = <:sexp_of<Path.t>>
                ; prj = <:of_sexp<Path.t>>
                }
    ; response = { inj = <:sexp_of<string Or_error.t>>
                 ; prj = <:of_sexp<string Or_error.t>>
                 }
    }


  type full_query = string * Sexp.t with sexp

  module Handler : sig
    type t
    val implement : ('a,'b) Rpc.t -> ('a -> 'b) -> t
    val handle : t list -> Sexp.t -> Sexp.t
  end = struct

    type t = { name: string
             ; handle : Sexp.t -> Sexp.t }

    let implement (rpc:(_,_) Rpc.t) f =
      let handle sexp =
        sexp
        |> rpc.request.prj
        |> f
        |> rpc.response.inj
      in
      { name = rpc.Rpc.name
      ; handle
      }

    let handle (handlers : t list) sexp =
      let (name,query_sexp) = full_query_of_sexp sexp in
      let handler =
        List.find_exn handlers ~f:(fun t -> t.name = name)
      in
      handler.handle query_sexp
  end


  let server_handle_query conn =
    Connection.recv conn
    >>= fun sexp ->
    let resp  =
      Handler.handle [ Handler.implement listdir_rpc   Filesystem.listdir
                     ; Handler.implement read_file_rpc Filesystem.read_file
                     ]
        sexp
    in
    Connection.send conn resp

  let dispatch (rpc:(_,_) Rpc.t) conn arg =
    let query_sexp = rpc.request.inj arg in
    Connection.send conn (sexp_of_full_query (rpc.name,query_sexp))
    >>= fun () ->
    Connection.recv conn
    >>= fun sexp ->
    return (rpc.response.prj sexp)

  module Client : sig
    val listdir : Connection.t -> Path.t -> Path.t list Or_error.t Deferred.t
    val read_file : Connection.t -> Path.t -> string Or_error.t Deferred.t
  end = struct
    let listdir conn x   = dispatch listdir_rpc conn x
    let read_file conn x = dispatch read_file_rpc conn x
  end

end


