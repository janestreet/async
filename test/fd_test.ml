open Core.Std  let _ = _squelch_unused_module_warning_
open Async.Std

let clear_nonblock () =
  Unix.pipe (Info.of_string "clear_nonblock")
  >>= fun (`Reader rfd, `Writer wfd) ->
  let reader = Reader.create rfd in
  let writer = Writer.create wfd in
  let line = "hello" in
  Writer.write_line writer line;
  Reader.read_line reader
  >>= function
    | `Eof -> assert false
    | `Ok line' ->
      assert (line = line');
      Fd.clear_nonblock rfd;
      let all =
        In_thread.run (fun () ->
          let in_channel = Core.Std.Unix.in_channel_of_descr (Fd.file_descr_exn rfd) in
          let s = In_channel.input_all in_channel in
          In_channel.close in_channel;
          s)
      in
      after (sec 0.001) (* wait a bit to give [In_channel.input_all] a chance to fail. *)
      >>= fun () ->
      Writer.write writer line;
      Writer.close writer
      >>= fun () ->
      all
      >>= fun line' ->
      assert (line = line');
      Fd.close rfd ~should_close_file_descriptor:false;
;;

let syscall_in_thread_doesn't_raise () =
  Fd.syscall_in_thread ~name:"z" (Fd.stdout ())
    (fun _ -> raise (Unix.Unix_error (EINTR, "", "")))
  >>| function
  | `Error _ -> ()
  | `Already_closed | `Ok _ -> assert false
;;

let tests =
  [ "Fd.clear_nonblock", clear_nonblock;
    "Fd.syscall_in_thread", syscall_in_thread_doesn't_raise;
  ]
;;
