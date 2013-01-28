open Core.Std
open Async.Std

let tests : (unit -> unit Deferred.t) list ref = ref []

let test f = tests := f :: !tests

let () =
  test (fun () ->
    assert (Thread_safe.am_holding_async_lock ());
    In_thread.run (fun () ->
      assert (not (Thread_safe.am_holding_async_lock ()))))
;;

let () =
  test (fun () ->
    assert
      (try Thread_safe.run_in_async_exn (fun () -> assert false); false
       with _ -> true);
    In_thread.run (fun () ->
      Thread_safe.run_in_async_exn (fun () ->
        assert (Thread_safe.am_holding_async_lock ())));
  )
;;

let () =
  test (fun () ->
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
  test (fun () ->
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
  test (fun () ->
    In_thread.run (fun () -> Thread_safe.deferred ())
    >>= fun (d, put) ->
    assert (try put 13; false with _ -> true);
    whenever (In_thread.run (fun () -> put 13));
    d
    >>| fun i ->
    assert (i = 13);
  )
;;

let () =
  test (fun () ->
    In_thread.run (fun () -> Thread_safe.pipe ())
    >>= fun (pipe, put, close) ->
    assert (try put 13; false with _ -> true);
    let throttle = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1 in
    let run_in_thread f =
      whenever (Throttle.enqueue throttle (fun () -> In_thread.run f))
    in
    let num_elts = 100 in
    for i = 0 to num_elts - 1; do
      run_in_thread (fun () -> put i);
    done;
    run_in_thread (fun () -> close ());
    Pipe.to_list pipe
    >>| fun list ->
    assert (list = List.init num_elts ~f:Fn.id);
  )
;;

let tests =
  [ "Thread_safe_test",
    (fun () -> Deferred.List.iter !tests ~f:(fun f -> f ()))
  ]
;;

