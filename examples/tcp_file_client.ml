open Core.Std
open Async.Std

let tail host port files =
  Tcp_file.Client.connect ~host ~port
  >>| Result.ok_exn
  >>= fun client ->
  Deferred.List.map files ~how:`Parallel ~f:(fun file ->
    Tcp_file.Client.tail client file
    >>= fun pipe ->
    Pipe.iter_without_pushback pipe ~f:(function
      | Ok message ->
        begin match Tcp_file.Client.Message.to_string message with
        | None -> eprintf "[%s] message too big\n" file;
        | Some msg -> printf "[%s]%s\n" file msg;
        end
      | Error e ->
        eprintf "[%s] %s\n" file (Tcp_file.Client.Error.to_string e);
    ))
  >>| fun (_ : unit list) -> ()
;;

let cmd =
  Command.async
    ~summary:"tail files using tcp_file"
    Command.Spec.
    (empty
     +> flag "host" (required string) ~doc:"HOST tcp_file server host"
     +> flag "port" (required int)    ~doc:"PORT tcp_file server port"
     +> flag "num-clients" (optional_with_default 1 int)
          ~doc:"NUM run this many clients concurrently (default 1)"
     +> anon (sequence ("FILE" %: string)))
    (fun host port num_clients files () ->
       let num_clients = Int.max 1 num_clients in
       Deferred.List.map ~how:`Parallel
         (List.range ~start:`inclusive 1 ~stop:`inclusive num_clients)
         ~f:(fun _ -> tail host port files)
       >>| fun (_ : unit list) -> ())
;;

let () = Command.run cmd
