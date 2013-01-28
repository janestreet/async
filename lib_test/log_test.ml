open Core.Std
open Async.Std

let clear_file file =
  Sys.file_exists file
  >>= fun exists ->
  begin match exists with
  | `No      -> Deferred.unit
  | `Unknown -> failwithf "unable to determine if %s exists" file ()
  | `Yes     -> Unix.unlink file
  end
;;

let write_and_read (fmt : Log.Output.machine_readable_format) () =
  let file = "tmp_async_log_test.txt" in
  clear_file file
  >>= fun () ->
  let log  =
    Log.create ~level:`Debug
      ~output:[ Log.Output.file (fmt :> Log.Output.format) ~filename:file ]
  in
  let messages = [
    `Debug, "debugging message";
    `Info, "info message";
    `Info, "";
    `Error, "error message";
    `Error, " spaces at both ends ";
  ] in
  let start_time = Time.now () in
  List.iter messages ~f:(fun (level, msg) ->
    match level with
    | `Debug -> Log.debug log "%s" msg;
    | `Info  -> Log.info log "%s" msg;
    | `Error -> Log.error log "%s" msg);
  Log.flushed log
  >>= fun () ->
  Log.close log;
  let stop_time = Time.now () in
  Pipe.to_list (Log.Reader.pipe fmt file)
  >>= fun read ->
  assert (List.length read = List.length messages);
  List.iter2_exn messages read
    ~f:(fun (e_level, e_msg) msg ->
      assert (Some e_level = Log.Message.level msg);
      assert (e_msg = Log.Message.message msg);
      assert (Time.(start_time <= Log.Message.time msg && Log.Message.time msg <= stop_time)));
  Unix.unlink file
;;

let speed_test fmt max_time () =
  let file = "tmp_async_log_speed_test.txt" in
  clear_file file
  >>= fun () ->
  let log =
    Log.create ~level:`Info ~output:[ Log.Output.file fmt ~filename:file ]
  in
  let msg = lazy
    "the quick brown fox jumped over the lazy dog two or three times to make the \
    description longer"
  in
  let rec loop n =
    if n = 0
    then ()
    else begin
      Log.of_lazy log msg;
      loop (n - 1)
    end
  in
  let start_time = Time.now () in
  loop 1_000_000;
  Log.flushed log
  >>= fun () ->
  let stop_time  = Time.now () in
  let total_time = Time.diff stop_time start_time in
  if Time.Span.(>) total_time max_time then
    failwithf "regression: log write and flush took more than %s (%s)"
      (Time.Span.to_string max_time)
      (Time.Span.to_string total_time) ();
  Unix.unlink file
;;

let rotation_test () =
  let rotation =
    {Log.Rotation.
      messages = Some 100;
      size     = None;
      time     = None;
      keep     = `At_least 5;
    }
  in
  let rec loop n =
    if n <= 10 then begin
      let o = Log.Output.rotating_file `Sexp ~basename:"test" rotation in
      let log = Log.create ~level:`Info ~output:[o] in
      Log.info log "test";
      let flushed = Log.flushed log in
      Log.close log;
      flushed
      >>= fun () ->
      loop (n + 1)
    end else Deferred.unit
  in
  loop 0
  >>= fun () ->
  Sys.readdir "."
  >>= fun files ->
  let files = Array.to_list files in
  let logs =
    List.filter files ~f:(fun fn ->
      String.is_prefix fn ~prefix:"test."
      && String.is_suffix fn ~suffix:".log")
  in
  Deferred.List.iter logs ~f:(fun fn -> Unix.unlink fn)
  >>| fun () ->
  let num_logs = List.length logs in
  if num_logs = 6 then ()
  else failwith "more logs than expected after rotation"
;;

let tests = [
  "Log_test.write_and_read (sexp)", write_and_read `Sexp;
  "Log_test.write_and_read (bin-prot)", write_and_read `Bin_prot;
  "Log_test.speed_regression (sexp)", speed_test `Sexp (sec 8.);
  "Log_test.speed_regression (text)", speed_test `Text (sec 8.);
  "Log_test.speed_regression (bin-prot)", speed_test `Bin_prot (sec 5.);
  "Log_test.rotation", rotation_test;
]

