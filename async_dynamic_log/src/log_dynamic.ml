include Async_unix.Log
open! Async_inotify
open! Async
open! Core

type t =
  { log : Log.t
  ; hashtable : (Position.t, bool) Base.Hashtbl.t
  ; file_path : string
  }

let add_to_hashtbl t (lexing : Lexing.position) =
  let key = Position.create_t lexing in
  (match Hashtbl.add t.hashtable ~key ~data:true with _ -> ());
  let%bind file_contents = Reader.file_contents t.file_path in
  let%map w = Writer.open_file ~append:true t.file_path in
  let data = Hashtbl.find_exn t.hashtable key in
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
  (match Hashtbl.find t.hashtable (Position.create_t lexing) with
  | None -> sexp ~level:`Error ?time ?tags t.log the_sexp
  | Some print -> if print then sexp ~level:`Error ?time ?tags t.log the_sexp);
  don't_wait_for (add_to_hashtbl t lexing)
;;

let debug_s ?time ?tags (lexing : Lexing.position) t the_sexp =
  (match Hashtbl.find t.hashtable (Position.create_t lexing) with
  | None -> sexp ~level:`Debug ?time ?tags t.log the_sexp
  | Some print -> if print then sexp ~level:`Debug ?time ?tags t.log the_sexp);
  don't_wait_for (add_to_hashtbl t lexing)
;;

let info_s ?time ?tags (lexing : Lexing.position) t the_sexp =
  (match Hashtbl.find t.hashtable (Position.create_t lexing) with
  | None -> sexp ~level:`Info ?time ?tags t.log the_sexp
  | Some print -> if print then sexp ~level:`Info ?time ?tags t.log the_sexp);
  don't_wait_for (add_to_hashtbl t lexing)
;;

let update_states t () =
  let%map file_contents = Reader.file_contents t.file_path in
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
      Hashtbl.set
        t.hashtable
        ~key:(Position.create file line_num)
        ~data:state;
      loop tail
  in
  loop (List.slice lines 1 (List.length lines - 1));
  let line = List.nth_exn lines 0 in
  let x = Log.Level.of_string line in
  set_level t.log x;
  Core.print_endline ("Current level: " ^ Log.Level.to_string (level t.log))
;;

let set_control_file t =
  (* Clears file, writes level, and starts pipe in background for reading
     data*)
  let%bind w = Writer.open_file t.file_path in
  Writer.write w (Log.Level.to_string (level t.log) ^ "\n");
  let%map _, _, inotify_pipe =
    Async_inotify.create ~events:[ Event.Selector.Modified ] t.file_path
  in
  don't_wait_for (Pipe.iter inotify_pipe ~f:(fun _ -> update_states t ()))
;;

let create
    ~level
    ~output
    ~on_error
    ~listener_file_path
    ?time_source
    ?transform
    ()
  =
  let l = Log.create ~level ~output ~on_error ?time_source ?transform () in
  let t =
    { log = l
    ; hashtable = Hashtbl.create (module Position)
    ; file_path = listener_file_path
    }
  in
  let%map () = set_control_file t in
  t
;;

module Make_global_dynamic () : sig
  include Async_unix.Log.Global_intf
end = struct
  include Async_unix.Log.Make_global ()
end

module Global_dynamic_log = Make_global_dynamic ()
