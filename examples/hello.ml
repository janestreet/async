open Core.Std
open Async.Std

let handler _ reader writer =
  Deferred.create (fun i ->
    let write () = Writer.write writer
      "HTTP/1.1 200 OK\nContent-length: 12\nContent-type: text/plain\n\nHello World!" in
    let rec read () = Reader.read_line reader >>>
      function
        (* Blows up horribly if it's a POST
         * (or anything with Content-length /= 0 *)
        | `Ok "" -> write (); read ()
        | `Eof   -> write (); Ivar.fill i ()
        | _      -> read ()
    in read () )
let () = ignore (Tcp.serve ~port:55555 ~on_handler_error:`Ignore handler)
let () = never_returns (Scheduler.go ())
