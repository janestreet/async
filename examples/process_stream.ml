open Jane.Std
open Async.Std

let printf = Print.printf

let () = 
  let (_, out, _) = 
    In_process.full 
      (fun ~input:_ ~output ->
         Stream.iter (Clock.at_intervals (sec 1.0)) ~f:(fun _ ->
           Tail.extend output (Time.now ()));
         Deferred.never ())
  in
  let _died = 
    In_thread.run (fun () ->
      let (pid, status) = Std_unix.wait () in
      printf "worker died! %s\n"
        (match status with
        | Std_unix.WEXITED s -> (string_of_int pid) ^ ": " ^ (string_of_int s)
        | Std_unix.WSIGNALED s -> (string_of_int pid) ^ ": " ^ (string_of_int s)
        | _ -> ""))
  in
  Stream.iter out ~f:(fun x ->
    printf "the time is: %s\n" (Time.to_string x));
  never_returns (Scheduler.go ())
