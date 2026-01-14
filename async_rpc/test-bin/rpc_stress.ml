open! Core
open! Async

module Protocol = struct
  module Stress = struct
    type query = int [@@deriving bin_io]
    type response = int array [@@deriving bin_io]

    let rpc =
      Rpc.Pipe_rpc.create
        ~name:"pipe-rpc-stress-test"
        ~version:1
        ~bin_query
        ~bin_response
        ~bin_error:Nothing.bin_t
        ()
    ;;
  end
end

(* Arbitrary value to send between client and server *)
let send_value = 4343

let receive_arrays conn arr_size =
  match%bind Rpc.Pipe_rpc.dispatch Protocol.Stress.rpc conn arr_size with
  | Error error ->
    print_s [%message (error : Error.t)];
    return ()
  | Ok (Ok (pipe, (_ : Rpc.Pipe_rpc.Metadata.t))) ->
    Pipe.iter_without_pushback pipe ~f:(fun arr ->
      Array.iter arr ~f:(fun i -> assert (i = send_value));
      print_endline [%string "Received array of size %{Array.length arr#Int}"])
  | Ok (Error t) -> Nothing.unreachable_code t
;;

let implementations ~freq ~which =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Raise
    ~implementations:
      [ Rpc.Pipe_rpc.implement Protocol.Stress.rpc (fun conn arr_size ->
          (match which with
           | `Client -> ()
           | `Server -> don't_wait_for (receive_arrays conn arr_size));
          let r, w = Pipe.create () in
          Clock_ns.every' freq (fun () ->
            if not (Pipe.is_closed w)
            then (
              let arr = Array.create ~len:arr_size send_value in
              Pipe.write w arr)
            else return ());
          return (Ok r))
      ]
;;

let port_flag = Command.Param.(flag "port" (required int) ~doc:"port")

let heartbeat_config_flag =
  let%map_open.Command send_every =
    flag "beat-every" (required Time_ns.Span.arg_type) ~doc:"hearbeat freq"
  in
  Rpc.Connection.Heartbeat_config.create ~send_every ()
;;

let client_command =
  Command.async
    ~summary:"stress client"
    (let%map_open.Command msg_size =
       flag
         "msg-size"
         (required int)
         ~doc:"size of message sent in pipe rpc in (number of ints)"
     and freq = flag "freq" (required Time_ns.Span.arg_type) ~doc:"message frequency"
     and host =
       flag_optional_with_default_doc_sexp
         "host"
         string
         sexp_of_string
         ~default:"localhost"
         ~doc:"host"
     and port = port_flag
     and heartbeat_config = heartbeat_config_flag
     and close_after =
       flag
         "close-after"
         (required Time_ns.Span.arg_type)
         ~doc:"SPAN close after this interval"
     and close_reason =
       flag
         "close-reason"
         (required string)
         ~doc:"STRING reason for closing the connection, to send to the peer"
     in
     fun () ->
       let where_to_connect =
         Tcp.Where_to_connect.of_host_and_port (Host_and_port.create ~host ~port)
       in
       let implementations =
         Rpc.Connection.Client_implementations.T
           { connection_state =
               (fun conn ->
                 don't_wait_for
                   (let%bind () = Clock_ns.after close_after in
                    Rpc.Connection.close conn ~reason:(Info.of_string close_reason));
                 conn)
           ; implementations =
               implementations
                 ~on_exception:(Raise_to_monitor Monitor.main)
                 ~freq
                 ~which:`Client
           }
       in
       Rpc.Connection.with_client
         ~implementations
         ~heartbeat_config
         where_to_connect
         (fun conn -> receive_arrays conn msg_size)
       >>| Result.ok_exn)
;;

let server_command =
  Command.async
    ~summary:"stress server"
    (let%map_open.Command freq =
       flag "freq" (required Time_ns.Span.arg_type) ~doc:"message frequency"
     and port = port_flag
     and heartbeat_config = heartbeat_config_flag in
     fun () ->
       let server =
         Rpc.Connection.serve_inet
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ~heartbeat_config
           ~initial_connection_state:(fun (_ : Socket.Address.Inet.t) conn ->
             Deferred.upon
               (Rpc.Connection.close_reason conn ~on_close:`finished)
               (fun reason -> print_s [%message "Connection closed" (reason : Info.t)]);
             conn)
           ~implementations:
             (implementations
                ~on_exception:(Raise_to_monitor Monitor.main)
                ~freq
                ~which:`Server)
           ()
       in
       Tcp.Server.close_finished server)
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:
         "A dummy RPC server/client for testing RPC behavior under various message \
          frequencies, message sizes, and heartbeat frequencies."
       [ "client", client_command; "server", server_command ])
;;
