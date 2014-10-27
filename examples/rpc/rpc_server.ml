open Core.Std
open Async.Std


(* The list of implementations supported by the server.  The server state is simply a
   counter used for allocating unique ids. *)
let implementations =
  [ Rpc.Rpc.implement Rpc_intf.get_unique_id
      (fun ctr () ->
        printf ".%!";
        incr ctr;
        return !ctr
      )
  ; Rpc.Rpc.implement Rpc_intf.set_id_counter
    (fun ctr i  ->
      printf "!%!";
      if i = 0 then failwith "Can't set counter back to zero";
      return (ctr := i)
    )

  ; Rpc.Pipe_rpc.implement Rpc_intf.counter_values
    (fun ctr () ~aborted ->
      let (r,w) = Pipe.create () in
      let last_value = ref !ctr in
      let send () =
        last_value := !ctr;
        Pipe.write w !ctr
      in
      don't_wait_for (send ());
      Clock.every' ~stop:aborted (sec 0.1) (fun () ->
        if !last_value <> !ctr
        then send () else return ()
      );
      return (Ok r)
    )
  ]

let main () =
  let counter = ref 0 in
  let implementations =
    Rpc.Implementations.create ~implementations ~on_unknown_rpc:`Close_connection
  in
  match implementations with
  | Error (`Duplicate_implementations _descrs) -> assert false
  | Ok implementations ->
    let server =
      Tcp.Server.create (Tcp.on_port 8080) ~on_handler_error:`Ignore
        (fun _addr reader writer ->
           Rpc.Connection.server_with_close reader writer ~implementations
             ~connection_state:(fun _ -> counter)
             ~on_handshake_error:`Ignore)
    in
    ignore (server : (_,_) Tcp.Server.t  Deferred.t);
    Deferred.never ()

let () =
  Command.run
    (Command.async_basic
       ~summary:"A trivial Async-RPC server"
       Command.Spec.empty
       main)
