open Core.Std
open Core_extended.Std
open Async.Std


let with_rpc_connection f =
  Tcp.with_connection ~host:"127.0.0.1" ~port:8080
    (fun reader writer ->
      Rpc.Connection.create reader writer ~connection_state:()
      >>= function
        | Error exn -> raise exn
        | Ok conn ->
          f conn
          >>= fun () ->
          shutdown 0;
          return ()
    )

let set_id_counter new_id =
  with_rpc_connection (fun conn ->
    Rpc.Rpc.dispatch_exn Rpc_intf.set_id_counter conn new_id
  )

let set_id_counter_v0 new_id_pair =
  with_rpc_connection (fun conn ->
    Rpc.Rpc.dispatch_exn Rpc_intf.set_id_counter_v0 conn new_id_pair
  )

let get_unique_id () =
  with_rpc_connection (fun conn ->
    Rpc.Rpc.dispatch_exn Rpc_intf.get_unique_id conn ()
    >>= fun id ->
    printf "UNIQUE ID: %d\n" id;
    return ()
  )

let counter_values () =
  with_rpc_connection (fun conn ->
    Rpc.Pipe_rpc.dispatch_exn Rpc_intf.counter_values conn ()
    >>= fun (reader,_id) ->
    Pipe.iter_without_pushback reader ~f:(fun i ->
      printf "COUNTER: %d\n%!" i)
  )


(* Setting up the command-line interface *)

let in_async f = whenever (f ()); never_returns (Scheduler.go ())

let get_unique_id_cmd =
  Fcommand.(
    cmd ~summary:"get unique id from server"
      (const ())
      (fun () -> in_async get_unique_id)
  )

let set_id_counter_cmd =
  Fcommand.(
    cmd ~summary:"forcibly set the unique id counter.  DANGEROUS"
      (anon ("counter" %: int))
      (fun i -> in_async (fun () -> set_id_counter i))
  )

(* This one is actually unsupported by the server, so using it will trigger an error. *)
let set_id_counter_cmd_v0 =
  Fcommand.(
    cmd ~summary:"forcibly set the unique id counter.  DANGEROUS"
      (anon ("counter1" %: int) ++ anon ("counter2" %: int))
      (fun id1 id2 -> in_async (fun () -> set_id_counter_v0 (id1,id2)))
  )


let counter_values_cmd =
  Fcommand.(
    cmd ~summary:"subscribe to changes to counter id"
      (const ())
      (fun () -> in_async counter_values)
  )


let () =
  Version_util_extended.Command.run
    (Command.group
       ~summary:"Client for trivial Async-RPC server"
       [ "get-unique-id"    , get_unique_id_cmd
       ; "set-id-counter"   , set_id_counter_cmd
       ; "set-id-counter-v0", set_id_counter_cmd_v0
       ; "counter-values"   , counter_values_cmd
       ]
    )
