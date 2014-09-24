open Core.Std
open Async.Std

let dispatch_gen dispatch rpc arg =
  Tcp.with_connection (Tcp.to_host_and_port "127.0.0.1" 8080)
    (fun _ reader writer ->
       Rpc.Connection.create reader writer ~connection_state:()
       >>= function
       | Error exn -> raise exn
       | Ok conn ->
         dispatch rpc conn arg
         >>= fun x ->
         shutdown 0;
         return x
    )

let dispatch rpc arg =
  dispatch_gen Rpc.Rpc.dispatch_exn rpc arg

let pipe_dispatch rpc arg =
  dispatch_gen Rpc.Pipe_rpc.dispatch_exn rpc arg

let set_id_counter new_id =
  dispatch Rpc_intf.set_id_counter new_id

let set_id_counter_v0 new_id_pair =
  dispatch Rpc_intf.set_id_counter_v0 new_id_pair

let get_unique_id () =
  dispatch Rpc_intf.get_unique_id ()
  >>= fun id ->
  printf "UNIQUE ID: %d\n" id;
  return ()

let counter_values () =
  pipe_dispatch Rpc_intf.counter_values ()
  >>= fun (reader,_id) ->
  Pipe.iter_without_pushback reader ~f:(fun i ->
    printf "COUNTER: %d\n%!" i)

(* Setting up the command-line interface *)

let get_unique_id_cmd =
  Command.async_basic
    ~summary:"get unique id from server"
    Command.Spec.empty
    (fun () -> get_unique_id ())

let set_id_counter_cmd =
  Command.async_basic
    ~summary:"forcibly set the unique id counter.  DANGEROUS"
    Command.Spec.(
      empty
      +> anon ("counter" %: int)
    )
    (fun i () -> set_id_counter i)

(* This one is actually unsupported by the server, so using it will trigger an error. *)
let set_id_counter_cmd_v0 =
  Command.async_basic
    ~summary:"forcibly set the unique id counter.  DANGEROUS"
    Command.Spec.(
      empty
      +> anon ("counter1" %: int)
      +> anon ("counter2" %: int)
    )
    (fun id1 id2 () -> set_id_counter_v0 (id1,id2))

let counter_values_cmd =
  Command.async_basic
    ~summary:"subscribe to changes to counter id"
    Command.Spec.empty
    (fun () -> counter_values ())

let () =
  Command.run
    (Command.group ~summary:"Client for trivial Async-RPC server"
       [ "get-unique-id"    , get_unique_id_cmd
       ; "set-id-counter"   , set_id_counter_cmd
       ; "set-id-counter-v0", set_id_counter_cmd_v0
       ; "counter-values"   , counter_values_cmd
       ]
    )
