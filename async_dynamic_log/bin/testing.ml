open! Core
open! Async_kernel
open! Async_unix
open! Inotify
open! Async
open! Async_inotify

type levels =
  | Info
  | Debug
  | Error
[@@deriving sexp, bin_io]

let file_path =
  "/home/ubuntu/async-logging/async/async_dynamic_log/bin/lit.txt"
;;

let levels_to_string level =
  match level with Info -> "Info" | Debug -> "Debug" | Error -> "Error"
;;

let update_level level_ref =
  let%bind.Deferred () = Clock.after (Time_float_unix.Span.of_int_ms 50) in
  let%map.Deferred file_contents = Reader.file_contents file_path in
  match
    Or_error.try_with (fun () ->
        let x = file_contents |> Sexp.of_string |> levels_of_sexp in
        level_ref := x)
  with
  | Ok () -> ()
  | Error e -> print_s [%message (e : Error.t)]
;;

let test () =
  let level = ref Info in
  let%bind () = update_level level in
  let%bind _, _, inotify_pipe =
    Async_inotify.create ~events:[ Event.Selector.Modified ] file_path
  in
  Pipe.iter inotify_pipe ~f:(fun _ ->
      Core.print_endline "received";
      let%map () = update_level level in
      Core.print_endline ("Current level: " ^ levels_to_string !level))
;;

let command =
  Command.async
    ~summary:"log verbosity change proof of concept"
    [%map_open.Command
      let () = return () in
      fun () -> test ()]
;;
