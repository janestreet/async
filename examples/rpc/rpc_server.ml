open Core.Std
open Core_extended.Std
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
      whenever (send ());
      Clock.every' ~stop:aborted (sec 0.1) (fun () ->
        if !last_value <> !ctr
        then send () else return ()
      );
      return (Ok r)
    )
  ]

let main () =
  let counter = ref 0 in
  let server = Rpc.Server.create ~implementations ~on_unknown_rpc:`Ignore in
  match server with
  | Error (`Duplicate_implementations _descrs) -> assert false
  | Ok server ->
    whenever
      (Tcp.serve ~port:8080 ~on_handler_error:`Ignore
         (fun _addr reader writer ->
           Rpc.Connection.server_with_close reader writer ~server
             ~connection_state:counter
             ~on_handshake_error:`Ignore));
    never_returns (Scheduler.go ())

let () =
  Version_util_extended.Command.run
    (Fcommand.cmd
       ~summary:"A trivial Async-RPC server"
       (Fcommand.const ())
       main)
