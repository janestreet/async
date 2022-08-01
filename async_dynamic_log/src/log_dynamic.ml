include Async_unix.Log
open! Async_inotify
open! Async
open! Core

type t =
  { log : Log.t
  ; map : Position.t Position.Map.t
  ; file : string option
  }

let levels_to_string level =
  match level with `Info -> "Info" | `Debug -> "Debug" | `Error -> "Error"
;;

let string_to_levels str =
  match str with
  | "Info" -> `Info
  | "Debug" -> `Debug
  | "Error" -> `Error
  | _ -> failwith "invalid type"
;;

let master = Hashtbl.create (module Position)
let file = "/home/ubuntu/async-logging/async/async_dynamic_log/src/gen.txt"

let add_to_hashtbl file_path (lexing : Lexing.position) =
  let key = Position.create_t lexing in
  (match Hashtbl.add master ~key ~data:true with _ -> ());
  let%bind file_contents = Reader.file_contents file_path in
  let%map w = Writer.open_file ~append:true file_path in
  let data = Hashtbl.find_exn master key in
  let filename_line = key.file ^ " : " ^ Int.to_string key.line ^ " | " in
  if not (String.is_substring file_contents ~substring:filename_line)
  then (
    let str_to_write =
      key.file
      ^ " : "
      ^ Int.to_string key.line
      ^ " | "
      ^ Bool.to_string data
      ^ "\n"
    in
    Writer.write w str_to_write)
;;

let error_s ?time ?tags (lexing : Lexing.position) t the_sexp =
  (match Hashtbl.find master (Position.create_t lexing) with
  | None -> sexp ~level:`Error ?time ?tags t.log the_sexp
  | Some print -> if print then sexp ~level:`Error ?time ?tags t.log the_sexp);
  don't_wait_for (add_to_hashtbl file lexing)
;;

let debug_s ?time ?tags (lexing : Lexing.position) t the_sexp =
  (match Hashtbl.find master (Position.create_t lexing) with
  | None -> sexp ~level:`Debug ?time ?tags t.log the_sexp
  | Some print -> if print then sexp ~level:`Debug ?time ?tags t.log the_sexp);
  don't_wait_for (add_to_hashtbl file lexing)
;;

let info_s ?time ?tags (lexing : Lexing.position) t the_sexp =
  (match Hashtbl.find master (Position.create_t lexing) with
  | None -> sexp ~level:`Info ?time ?tags t.log the_sexp
  | Some print -> if print then sexp ~level:`Info ?time ?tags t.log the_sexp);
  don't_wait_for (add_to_hashtbl file lexing)
;;

let update_states file_path () =
  let%map file_contents = Reader.file_contents file_path in
  let lines = Core.String.split file_contents ~on:'\n' in
  let rec loop list =
    match list with
    | [] -> ()
    | line :: tail ->
      let colon = String.index_exn line ':' in
      let pipe_line = String.index_exn line '|' in
      let file = String.slice line 0 (colon - 1) in
      let line_num =
        Int.of_string (String.slice line (colon + 2) (pipe_line - 1))
      in
      let state =
        Bool.of_string
          (String.slice line (pipe_line + 2) (String.length line))
      in
      Hashtbl.set master ~key:(Position.create file line_num) ~data:state;
      loop tail
  in
  loop (List.slice lines 0 (List.length lines - 1))
;;

let set_control_file file_path =
  (* Clears file and starts pipe in background for reading data*)
  let%bind w = Writer.open_file file_path in
  Writer.write w "";
  let%map _, _, inotify_pipe =
    Async_inotify.create ~events:[ Event.Selector.Modified ] file_path
  in
  don't_wait_for
    (Pipe.iter inotify_pipe ~f:(fun _ -> update_states file_path ()))
;;

let update_level file_path log =
  let%bind.Deferred () = Clock.after (Time_float_unix.Span.of_int_ms 50) in
  let%map.Deferred file_contents = Reader.file_contents file_path in
  match
    Or_error.try_with (fun () ->
        let x = string_to_levels file_contents in
        set_level log x)
  with
  | Ok () ->
    Core.print_endline ("Current level: " ^ levels_to_string (level log))
  | Error e -> print_s [%message (e : Error.t)]
;;

let set_listener file_path log =
  let%bind _, _, inotify_pipe =
    Async_inotify.create ~events:[ Event.Selector.Modified ] file_path
  in
  let%bind () = update_level file_path log in
  Pipe.iter inotify_pipe ~f:(fun _ -> update_level file_path log)
;;

let create
    ~level
    ~output
    ~on_error
    ?time_source
    ?transform
    ?listener_file_path
    ()
  =
  match listener_file_path with
  | None ->
    { log = Log.create ~level ~output ~on_error ?time_source ?transform ()
    ; map = Position.Map.empty
    ; file = None
    }
  | Some n ->
    let l = Log.create ~level ~output ~on_error ?time_source ?transform () in
    upon (set_listener n l) (fun () -> ());
    { log = l; map = Position.Map.empty; file = Some n }
;;

module Make_global_dynamic () : sig
  include Async_unix.Log.Global_intf

  val set_listener : string -> unit Deferred.t
end = struct
  include Async_unix.Log.Make_global ()

  let set_listener file_path = set_listener file_path (Lazy.force log)
end

module Global_dynamic_log = Make_global_dynamic ()
