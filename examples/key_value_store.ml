open Core.Std
open Async.Std

module Protocol = struct
  module Client_message = struct
    type t = Req_help
           | Req_add    of string * string
           | Req_find   of string
           | Req_remove of string
           | Req_invalid
    with sexp


    let from_raw msg = match String.split msg ~on:' ' with
      | ("help" :: _          ) -> Req_help
      | ("add"  :: k :: v :: _) -> Req_add (k, v)
      | ("find" :: k :: _     ) -> Req_find k
      | ("remove" :: k :: _   ) -> Req_remove k
      | _                       -> Req_invalid
  end

  module Server_message = struct
    type t = Rep_fail of string
           | Rep_succ of string
           | Rep_help
    with sexp


    let to_raw t = match t with
      | Rep_fail msg -> "[ERROR] " ^ msg
      | Rep_succ msg -> msg
      | Rep_help     -> "Valid commands: help, add, find, remove"
  end

  module Transport = struct
    type t = Reader.t * Writer.t

    let create r w = return (r, w)
    let close   (r, _) = Reader.close r
    let read    (r, _) =
      Reader.read_line r
      >>| function
        | `Eof -> `Eof
        | `Ok rawmsg -> `Ok (Client_message.from_raw rawmsg)

    let write (_, w) msg =
      Writer.write w (Server_message.to_raw msg);
      Writer.newline w
    let flushed_time (_, w) = Writer.flushed_time w
  end
end

module Server = Typed_tcp.Make(Protocol)

module Key_value_store = struct

  type t = {
    store  : (string, string) Hashtbl.t;
  }

  let create () = {
    store  = Hashtbl.Poly.create ();
  }

  let process t req =
    let module R = Protocol.Client_message in
    let module P = Protocol.Server_message in
    match req with
    |(R.Req_help |
      R.Req_invalid)   -> P.Rep_help
    | R.Req_add (k, v) ->
        Hashtbl.replace t.store ~key:k ~data:v;
        P.Rep_succ ("New pair stored: "^k^" -> "^v)
    | R.Req_find k     ->
        begin match Hashtbl.find t.store k with
        | None   -> P.Rep_fail ("No value found for: "^k)
        | Some v -> P.Rep_succ v
        end
    | R.Req_remove k   ->
        begin match Hashtbl.find t.store k with
        | None   -> P.Rep_fail ("No value found for: "^k)
        | Some _ ->
          Hashtbl.remove t.store k;
          P.Rep_succ ("Key removed: "^k)
        end
end

let main () =
  let kvstore = Key_value_store.create () in
  Server.create ~port:12321 ~auth:(fun _ _ -> return `Allow) ()
  >>> fun svr ->
  let echo (clt, msg) = Server.send_ignore_errors
                          svr
                          clt
                          (Key_value_store.process kvstore msg) in
  whenever (Pipe.iter_without_pushback ~f:echo (Server.listen_ignore_errors svr))

let () =
  main ();
  never_returns (Scheduler.go ())
