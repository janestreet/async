open Core.Std
open Qtest_lib.Std
open Async.Std

let global_dir = "/mnt/global/base/bin/"

let filename_filter filename =
  let prefix = "rpc-canary-" in
  let suffix = ".exe" in
  String.is_prefix ~prefix filename && String.is_suffix ~suffix filename
  &&
    let interior =
      String.slice filename (String.length prefix) (- String.length suffix)
    in
    match String.lsplit2 interior ~on:'.' with
    | None -> false
    | Some (s1, s2) ->
      List.for_all ~f:(String.for_all ~f:Char.is_digit) [s1; s2]
;;

let canary_exe_names =
  List.filter (Array.to_list (Core.Std.Sys.readdir global_dir)) ~f:filename_filter
;;

let local_port =
  Random.self_init ();
  10000 + Random.int 10000

let global_progs_and_ports =
  List.mapi canary_exe_names ~f:(fun i exe -> global_dir ^/ exe, local_port + i + 1)

let local_prog = "../../../tools/rpc_canary/rpc_canary.exe"

exception Global_as_server of string * exn with sexp
exception Global_as_client of string * exn with sexp

let make_test ~running_prog ~exn_wrapper port =
  try_with (fun () ->
    Utils.create_process ~prog:running_prog ~args:["client"; Int.to_string port] ())
  >>| function
    | Ok () -> ()
    | Error e -> raise (exn_wrapper e)
;;

let global_as_server ~local_prog ~global_prog port =
  make_test ~exn_wrapper:(fun e -> Global_as_server (global_prog, e))
    ~running_prog:local_prog port
;;

let global_as_client ~global_prog port =
  make_test ~exn_wrapper:(fun e -> Global_as_client (global_prog, e))
    ~running_prog:global_prog port
;;

let all_canaries () =
  let start_server ~prog port =
    Unix.fork_exec ~prog ~args:[prog; "server"; Int.to_string port] ()
  in
  (* spawn all the servers and keep track of their pids so they can be killed later *)
  Deferred.List.map global_progs_and_ports
    ~f:(fun (prog, port) -> start_server ~prog port)
  >>= fun global_server_pids ->
  start_server ~prog:local_prog local_port
  >>= fun local_server_pid ->
  (* give the servers time to initialize *)
  Clock.after (Time.Span.of_sec 10.)
  >>= fun () ->
  try_with (fun () -> Deferred.List.iter ~how:`Parallel global_progs_and_ports
    ~f:(fun (global_prog, port) -> Deferred.all_unit
        [ global_as_server ~local_prog ~global_prog port;
          global_as_client ~global_prog port;
        ]))
  >>| fun result ->
  (* kill all servers; lingering clients will die on their own *)
  let kill pid = Signal.send_i Signal.kill (`Pid pid) in
  kill local_server_pid;
  List.iter global_server_pids ~f:kill;
  match result with
  | Ok () -> ()
  | Error e -> raise e
;;

let tests = [ "Rpc_canary_test_all_canaries", all_canaries ]
