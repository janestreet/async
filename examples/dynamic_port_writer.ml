open Core.Std
open Async.Std

let test_message = "This is a message sent from the child process to the parent process."

let parent =
  "parent",
  Command.async ~summary:"parent" Command.Spec.empty
    (fun () ->
       Dynamic_port_writer.create ()
       >>= fun (dynamic_port_writer, port_d) ->
       let prog = Sys.executable_name in
       Unix.fork_exec ~prog ()
         ~args:([prog; "child"] @ Dynamic_port_writer.flag_args dynamic_port_writer)
       >>= fun child_pid ->
       port_d
       >>= fun r ->
       let `Port port = ok_exn r in
       Tcp.connect (Tcp.to_host_and_port "localhost" port)
       >>= fun (_, reader, _) ->
       Reader.contents reader
       >>= fun message ->
       assert (String.equal message test_message);
       print_endline message;
       Unix.waitpid_exn child_pid
       >>= fun _ ->
       return ())
;;

let child =
  "child",
  Command.async
    ~summary:"child"
    (Command.Spec.(empty +> Dynamic_port_writer.flag))
    (fun dynamic_port_writer () ->
       let close = Ivar.create () in
       Tcp.Server.create
         (Dynamic_port_writer.where_to_listen dynamic_port_writer)
         (fun _ _ writer ->
            Writer.write writer test_message;
            Ivar.fill close ();
            return ())
       >>= fun server ->
       Ivar.read close
       >>= fun () ->
       Tcp.Server.close server)
;;

let () =
  Command.run
    (Command.group ~summary:"dynamic port writer test" [child; parent])
;;
