open Jane.Std
open Async.Std

let printf = Print.printf

let () = 
  let f () = 
    upon (In_process.run (fun () -> 1 + 1)) (fun x ->
      printf "the answer is %d\n" x)
  in
  Stream.iter (Clock.at_intervals (sec 1.0)) ~f:(fun _ -> f ());
  never_returns (Scheduler.go ())
