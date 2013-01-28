open Core.Std
open Async.Std
open Qtest_lib.Std

let fast_child () = Unix.fork_exec ~prog:"/bin/true"  ~args:[]    ()
let slow_child () =
  let prog = "/bin/sleep" in
  Unix.fork_exec ~prog ~args:[ prog; "1" ] ()
;;

let raises_echild f =
  try ignore (f ()); false with
  | Unix.Unix_error (Unix.ECHILD, _, _) -> true
;;

let test_raises_echild () =
  assert (raises_echild (fun () -> Unix.wait_nohang          `Any     ));
  assert (raises_echild (fun () -> Unix.wait_nohang          `My_group));
  assert (raises_echild (fun () -> Unix.wait_nohang_untraced `Any     ));
  assert (raises_echild (fun () -> Unix.wait_nohang_untraced `My_group));
  Deferred.unit

let wait_on_fast_child wait_on =
  fast_child ()
  >>= fun pid ->
  Unix.wait (wait_on pid)
  >>| fun (pid', exit_or_signal) ->
  assert (Result.is_ok exit_or_signal);
  assert (pid = pid')

let wait_on_slow_child wait_on =
  Deferred.List.map (List.init 10 ~f:(fun _i -> slow_child ())) ~f:Fn.id
  >>= fun pids ->
  Deferred.List.iter ~how:`Parallel pids ~f:(fun pid ->
    let wait_on = wait_on pid in
    Monitor.try_with (fun () ->
      Unix.wait wait_on
      >>| fun (_, exit_or_signal) ->
        if not (Result.is_ok exit_or_signal) then
          failwith (Unix.Exit_or_signal.to_string_hum exit_or_signal))
    >>| function
    | Ok () -> ()
    | Error exn ->
      failwiths "wait for slow child" (wait_on, exn)
        (<:sexp_of< Unix.wait_on * exn >>))

let unsafe_tests =
  ("Wait_test.raises_echild", test_raises_echild)
  :: (List.concat_map [
      ("any", (fun _ -> `Any));
      ("my_group", (fun _ -> `My_group));
      ("pid", (fun pid -> `Pid pid));
    ] ~f:(fun (name, wait_on) -> [
      "Wait_test.fast_child_" ^ name, (fun () -> wait_on_fast_child wait_on);
      "Wait_test.slow_child_" ^ name, (fun () -> wait_on_slow_child wait_on);
    ]))
;;

(* Clear wait-status for any children spawned by previous tests. *)
let rec clear_wait_status () =
  let continue =
    try
      ignore (Unix.wait_nohang `Any);
      true
    with _ -> false
  in
  if continue then clear_wait_status ()

let tests = List.map unsafe_tests ~f:(fun (name, test) -> name, (fun () ->
  clear_wait_status ();
  test ()))
;;
