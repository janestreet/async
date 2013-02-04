open Core.Std
open Async.Std
open Async_extended.Std

let test1 () =
  Process.backtick_new_exn ~prog:"ls" ~args:[] ()
  >>| fun _ls_output_string ->
  ()
;;

let test2 () =
  Process.backtick_status ~prog:"ls" ~args:[] ()
  >>| fun ({ Process.Output. stdout = _; stderr }, exit_or_signal) ->
  assert (Result.is_ok exit_or_signal);
  assert (stderr = "");
;;

(* --------------------------------------------------------------------------------
   [Async.Std.Process] tests below here.
*)

module Process = Async.Std.Process

let test3 () =
  Process.create ~prog:"ls" ~args:[] ()
  >>= function
  | Error error -> Error.raise error
  | Ok process ->
    Process.wait process
    >>| fun { Process.Output. stdout = _; stderr; exit_status } ->
    assert (Result.is_ok exit_status);
    assert (stderr = "");
;;

let test4 () =
  Process.run ~prog:"ls" ~args:[] ()
  >>| function
  | Error error -> Error.raise error
  | Ok _ ->  ()
;;

let test5 () =
  let prog = "/bin/zzz" in
  Process.run ~prog ~args:[] ()
  >>| function
  | Error _ -> ()
  | Ok _ -> failwithf "somehow ran %s" prog ()
;;

let test6 () =
  let num_rounds = 100 in
  let num_processes_in_parallel = 10 in
  Deferred.repeat_until_finished 0 (fun i ->
    if i = num_rounds
    then return (`Finished ())
    else begin
      Deferred.all_unit
        (List.init num_processes_in_parallel ~f:(fun j ->
          let i = Int.to_string i in
          let j = Int.to_string j in
          Process.run ~prog:"echo" ~args:[ i; j ] ()
          >>| function
          | Error e -> Error.raise e
          | Ok s ->
            assert (s = String.concat [ i; " "; j; "\n" ])))
      >>| fun () ->
      `Repeat (i + 1)
    end)
;;

let test7 () =
  Process.create ~prog:"sleep" ~args:["1000"] ()
  >>= function
  | Error e -> Error.raise e
  | Ok process ->
    let wait = Process.wait process in
    let signal = Signal.usr1 in
    Signal.send_i signal (`Pid (Process.pid process));
    wait
    >>| fun { Process.Output. exit_status; _ } ->
    match exit_status with
    | Error (`Signal signal') -> assert (signal = signal')
    | _ -> assert false
;;

let tests =
  [ "Process_test1", test1;
    "Process_test2", test2;
    "Process_test3", test3;
    "Process_test4", test4;
    "Process_test5", test5;
    "Process_test6", test6;
    "Process_test7", test7;
  ]
;;
