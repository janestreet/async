open Core.Std
open Async.Std

let tests : (string * (unit -> unit Deferred.t)) list ref = ref []

let test name f =
  tests := (String.concat [ "Thread_safe: "; name ], f) :: !tests
;;

let () =
  test "run_in_async_with_optional_cycle must be in thread" (fun () ->
    begin try
      ignore (Thread_safe.run_in_async_with_optional_cycle (fun () -> (`Run_a_cycle, ()))
                : (unit, exn) Result.t);
      assert false;
    with _ -> ()
    end;
    Deferred.unit)
;;

let () =
  test "not am_holding_async_lock in a thread" (fun () ->
    assert (Thread_safe.am_holding_async_lock ());
    In_thread.run (fun () ->
      assert (not (Thread_safe.am_holding_async_lock ()))))
;;

let () =
  test "run_in_async_exn am_holding_async_lock" (fun () ->
    assert
      (try Thread_safe.run_in_async_exn (fun () -> assert false); false
       with _ -> true);
    In_thread.run (fun () ->
      Thread_safe.run_in_async_exn (fun () ->
        assert (Thread_safe.am_holding_async_lock ())));
  )
;;

let () =
  test "run_in_async_exn" (fun () ->
    let message = "foo" in
    Monitor.try_with (fun () ->
      In_thread.run (fun () ->
        Thread_safe.run_in_async_exn (fun () -> failwith message)))
    >>| function
    | Ok () -> assert false;
    | Error exn ->
      match Monitor.extract_exn exn with
      | Failure message' -> assert (message = message')
      | _ -> assert false
  )
;;

let () =
  test "deferred" (fun () ->
    In_thread.run (fun () ->
      let (d, put) = Thread_safe.deferred () in
      put 13;
      d)
    >>= fun d ->
    d
    >>| fun i ->
    assert (i = 13);
  )
;;

let () =
  test "deferred2" (fun () ->
    In_thread.run (fun () -> Thread_safe.deferred ())
    >>= fun (d, put) ->
    assert (try put 13; false with _ -> true);
    don't_wait_for (In_thread.run (fun () -> put 13));
    d
    >>| fun i ->
    assert (i = 13);
  )

let log_string = Async_kernel.Debug.log_string

let () =
  test "Thread_safe_pipe" (fun () ->
    let module P = Thread_safe_pipe in
    let r, p = P.create () in
    assert (try P.write p 13 ~if_closed:Raise; false with _ -> true);
    let throttle = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1 in
    let run_in_thread f =
      don't_wait_for (Throttle.enqueue throttle (fun () -> In_thread.run f))
    in
    let num_elts = 100 in
    for i = 0 to num_elts - 1; do
      run_in_thread (fun () -> P.write p i ~if_closed:Raise)
    done;
    run_in_thread (fun () -> P.close p);
    Pipe.to_list r
    >>| fun list ->
    assert (list = List.init num_elts ~f:Fn.id);
  )
;;

let () =
  test "Thread_safe_pipe2" (fun () ->
    In_thread.run (fun () -> Thread_safe_pipe.create ())
    >>= fun (pipe_reader, pipe_writer) ->
    assert (* [write] raises if we're in Async. *)
      (try
         Thread_safe_pipe.write pipe_writer 13 ~if_closed:Raise; false
       with _ ->
         true);
    let throttle = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1 in
    let run_in_thread f =
      don't_wait_for (Throttle.enqueue throttle (fun () -> In_thread.run f))
    in
    let num_elts = 100 in
    for i = 0 to num_elts - 1; do
      run_in_thread (fun () ->
        Thread_safe_pipe.write_without_pushback pipe_writer i ~if_closed:Raise)
    done;
    run_in_thread (fun () -> Thread_safe_pipe.close pipe_writer);
    Pipe.to_list pipe_reader
    >>| fun list ->
    assert (list = List.init num_elts ~f:Fn.id);
  )
;;

let tests = !tests
