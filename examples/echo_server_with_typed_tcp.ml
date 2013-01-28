open Core.Std
open Async.Std

module Protocol = struct
  module Client_message = String

  module Server_message = String

  module Transport = struct
    type t = Reader.t * Writer.t

    let create r w = return (r, w)

    let close (r, w) = Writer.close w >>= fun () -> Reader.close r

    let read (r, _) = Reader.read_line r

    let write (_, w) msg = Writer.write w msg; Writer.newline w

    let flushed_time (_, w) = Writer.flushed_time w
  end
end

module Server = Typed_tcp.Make(Protocol)

let main () =
  Server.create ~port:12321 ~auth:(fun _ _ -> return `Allow) ()
  >>> fun svr ->
  let echo (clt, msg) = Server.send_ignore_errors svr clt msg in
  let strm = Server.listen_ignore_errors svr in
  whenever (Pipe.iter_without_pushback ~f:echo strm)

let () =
  main ();
  never_returns (Scheduler.go ())
