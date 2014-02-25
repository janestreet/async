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
  let start_times = Unix.times () in
  loop 1_000_000;
  Log.flushed log
  >>= fun () ->
  let stop_times = Unix.times () in
  let total_time = Unix.(
      (stop_times.tms_utime +. stop_times.tms_stime)
    -. (start_times.tms_utime +. start_times.tms_stime))
    |! Time.Span.of_sec
  in
  if Time.Span.(>) total_time max_time then
    failwithf "regression: log write and flush took more than %s (%s)"
      (Time.Span.to_string max_time)
      (Time.Span.to_string total_time) ();
  Unix.unlink file
;;

let rotation_test =
  let max_files = 5 in
  let expected_msgs_per_file = 1 in
  let rotation =
    Log.Rotation.create
      ~messages:expected_msgs_per_file
      ~keep:(`At_least max_files)
      ~naming_scheme:`Numbered
      ()
  in
  let rec loop n =
    if n > 0 then begin
      let o = Log.Output.rotating_file `Sexp ~basename:"test" rotation in
      (* rotation on start-up is different from rotation during a log life-time.  This
         case of a log containing multiple copies of the same output is weird but legal,
         and it shakes out some race conditions in the rotation code. *)
      let log = Log.create ~level:`Info ~output:[o; o; o; o; o] in
      Log.info log "test %d" n;
      Clock.after (Time.Span.of_sec 1.)
      >>= fun () ->
      Log.info log "test %d" n;
      let flushed = Log.flushed log in
      Log.close log;
      flushed
      >>= fun () ->
      loop (n - 1)
    end else Deferred.unit
  in
  let check_num_msgs_per_file fnames expected =
    Deferred.List.map fnames ~f:(fun fname ->
      Pipe.to_list (Log.Reader.pipe `Sexp fname)
      >>| fun msgs ->
      let observed = List.length msgs in
      if Int.(=) observed expected_msgs_per_file
      then Ok ()
      else Or_error.error "too many messages in log file"
             (observed, "instead of", expected) <:sexp_of< int * string * int >>)
  in
  let check_num_files logs expected =
    let num_logs = List.length logs in
    if Int.(=) num_logs expected
    then Ok ()
    else Or_error.error "too many log files after rotation"
           (num_logs, "instead of", expected) <:sexp_of< int * string * int >>
  in
  fun () ->
    loop (2 * max_files)
    >>= fun () ->
    Sys.readdir "."
    >>= fun files ->
    let logs = Array.fold files ~init:[] ~f:(fun acc fn ->
      if String.is_prefix fn ~prefix:"test." && String.is_suffix fn ~suffix:".log"
      then fn :: acc
      else acc)
    in
    check_num_msgs_per_file logs expected_msgs_per_file
    >>= fun too_many_msgs_errors ->
    (Or_error.ok_exn
      (Or_error.combine_errors_unit
         (check_num_files logs (max_files + 1) :: too_many_msgs_errors)));
    (* if the tests failed, leave the output around for debugging *)
    Deferred.List.iter logs ~f:(fun fn -> Unix.unlink fn)
;;

let rotation_types =
  let max_files = 4 in
  let keep = `At_least max_files in
  let prefix = "rotation_type_test" in
  let is_log fn = String.is_prefix fn ~prefix && String.is_suffix fn ~suffix:".log" in
  let time_stamps () =
    Sys.readdir "."
    >>= Deferred.Array.filter_map ~f:(fun fn ->
      if is_log fn
      then
        Unix.stat fn
        >>| Unix.Stats.mtime
        >>| Option.some
      else
        return None
    )
    >>| Time.Set.of_array
  in
  fun () -> Deferred.List.iter [`Numbered ; `Timestamped] ~f:(fun naming_scheme ->
    let rotation = Log.Rotation.create ~keep ~naming_scheme () in
    let rec loop n prev_ts =
      if n=0 then Deferred.unit
      else
        let output = [Log.Output.rotating_file `Sexp ~basename:prefix rotation] in
        let log = Log.create ~level:`Debug ~output in
        Log.info log "Some output %d" n;
        Log.flushed log
        >>= fun () ->
        time_stamps ()
        >>= fun cur_ts ->
        let removed = Set.diff prev_ts cur_ts in
        Set.iter cur_ts ~f:(fun c ->
          Set.iter removed ~f:(fun r ->
            assert (Time.(<) r c)
          )
        );
        Clock.after (Time.Span.of_ms 1.)
        >>= fun () -> loop (n-1) cur_ts
    in
    loop (3 * max_files) Time.Set.empty
    >>= fun () ->
    Sys.readdir "."
    >>= Deferred.Array.iter ~f:(fun fn ->
      if is_log fn
      then Unix.unlink fn
      else Deferred.unit
    )
  )


let tests = [
  "Log_test.write_and_read (sexp)", write_and_read `Sexp;
  "Log_test.write_and_read (bin-prot)", write_and_read `Bin_prot;
  "Log_test.speed_regression (sexp)", speed_test `Sexp (sec 8.);
  "Log_test.speed_regression (text)", speed_test `Text (sec 8.);
  "Log_test.speed_regression (bin-prot)", speed_test `Bin_prot (sec 5.);
  "Log_test.rotation", rotation_test;
  "Log_test.rotation (types)", rotation_types
]

