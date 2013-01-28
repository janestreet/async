open Core.Std
open Async.Std

let tests =
  [ "Finalizer_test.test",
    fun () -> begin
      let finalizer_ran = ref false in
      return 13
      >>= fun x ->
      let l = [ x ] in
      Gc.add_finalizer_exn l (fun _ -> finalizer_ran := true);
      Gc.full_major ();
      after (sec 0.1)
      >>= fun () ->
      assert !finalizer_ran;
      Deferred.unit
    end
  ]
;;
