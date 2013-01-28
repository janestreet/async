open Core.Std
open Async.Std

let listen_to_named_pipe file ~add_fd =
  let file_gone =
    Sys.file_exists file
    >>= function
      | `Yes -> Unix.unlink file
      | `Unknown | `No -> Deferred.unit
  in
  file_gone
  >>= fun () ->
  let socket = Socket.create Socket.Type.unix in
  add_fd (Socket.fd socket);
  Socket.bind socket (Socket.Address.Unix.create file)
  >>| fun sock ->
  add_fd (Socket.fd sock);
  Socket.listen sock ~max_pending_connections:20

(* Check to see if the sexp representation of file descriptors contain
   unprintable characters. *)
let printable_sexp () =
  let file = Filename.dirname (Sys.executable_name) ^/ "socket_test" in
  let to_close = ref [] in
  let add_fd fd =
    to_close := fd::!to_close
  in
  Monitor.protect (fun () ->
    listen_to_named_pipe file ~add_fd
    >>= fun local_socket ->
    add_fd (Socket.fd local_socket);
    let rec loop = function
      | 0 -> return ()
      | remaining ->
        don't_wait_for (
          Socket.accept local_socket
          >>| function
            | `Socket_closed -> assert false
            | `Ok (socket, address) ->
              add_fd (Socket.fd socket);
              let sexp = (<:sexp_of< Socket.Address.t>> (address :> Socket.Address.t)) in
              let address_string = Sexp.to_string sexp in
              if String.exists address_string ~f:(fun c ->
                not (Char.is_print c)) then
                  Error.raise (Error.create "Sexp contains unprintable characters" sexp Fn.id));
        Tcp.connect_sock (Tcp.to_file file)
        >>= fun client ->
          add_fd (Socket.fd client);
          loop (remaining - 1)
    in
    loop 2)
    ~finally:(fun () ->
        after (Time.Span.of_sec 1.)
        >>= fun () ->
        Deferred.List.iter !to_close ~f:Unix.close)

let tests = [
  "Socket_test.printable_sexp", printable_sexp;
]
