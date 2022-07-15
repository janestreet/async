module Make_global_dynamic () : sig
  open! Async
  include Async_unix.Log.Global_intf

  val set_listener : string -> unit Deferred.t
end = struct
  include Async_unix.Log.Make_global ()
  open! Async_inotify
  open! Async
  open! Core

  let levels_to_string level =
    match level with
    | `Info -> "Info"
    | `Debug -> "Debug"
    | `Error -> "Error"
  ;;

  let string_to_levels str =
    match str with
    | "Info" -> `Info
    | "Debug" -> `Debug
    | "Error" -> `Error
    | _ -> failwith "invalid type"
  ;;

  let update_level file_path =
    let%bind.Deferred () = Clock.after (Time_float_unix.Span.of_int_ms 50) in
    let%map.Deferred file_contents = Reader.file_contents file_path in
    match
      Or_error.try_with (fun () ->
          let x = string_to_levels file_contents in
          set_level x)
    with
    | Ok () ->
      Core.print_endline ("Current level: " ^ levels_to_string (level ()))
    | Error e -> print_s [%message (e : Error.t)]
  ;;

  let set_listener file_path =
    let%bind _, _, inotify_pipe =
      Async_inotify.create ~events:[ Event.Selector.Modified ] file_path
    in
    let%bind () = update_level file_path in
    Pipe.iter inotify_pipe ~f:(fun _ -> update_level file_path)
  ;;
end

module Global_dynamic = Make_global_dynamic ()
