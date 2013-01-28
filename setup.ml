(* OASIS_START *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
(* OASIS_STOP *)


let protectx x ~f ~finally =
  let r = try f x with exn -> finally x; raise exn in
  finally x; r

let rm_rf dir =
  ignore (Printf.ksprintf Sys.command "/bin/rm -rf %S" dir : int)

let temp_dir ?(in_dir = Filename.temp_dir_name) prefix suffix =
  let base = Filename.concat in_dir prefix in
  let rec loop i =
     let dir = base ^ string_of_int i ^ suffix in
     let ret = Printf.ksprintf Sys.command "/bin/mkdir %S 2>/dev/null" dir in
     if ret = 0 then dir
     else if Sys.file_exists dir then loop (i + 1)
     else failwith ("mkdir failed on " ^ dir)
   in loop 0

let read_lines ic =
  let rec loop acc =
    match try Some (input_line ic) with End_of_file -> None with
    | Some line -> loop (line :: acc)
    | None -> List.rev acc
  in loop []

let test cmd =
  match Sys.command cmd with
  | 0 -> true
  | 1 -> false
  | _ -> failwith ("command ^cmd^ failed.")

let sh_lines cmd =
  protectx (Filename.temp_file "ocamlbuild_cmd" ".txt")
    ~f:(fun fn ->
      ignore (Sys.command ("(" ^ cmd ^ ") >" ^ fn) : int);
      protectx (open_in fn) ~f:read_lines ~finally:close_in)
    ~finally:Sys.remove

let getconf var =
  let cmd = Printf.sprintf "getconf %S" var in
  match sh_lines cmd with
  | []  -> None
  | [x] -> Some x
  | _   -> failwith ("`"^cmd^"` returned multiple lines")

let endswith x s =
  let len_x = String.length x and len_s = String.length s in
  (len_x <= len_s) && x = String.sub s (len_s - len_x) len_x

let select_files dir ext =
  List.map (Filename.concat dir)
    (List.filter (endswith ext)
      (Array.to_list (Sys.readdir dir)))
;;



(* dirty hack: ocamlbuild fails to build async.cmx when using async.cmi
   ocamlbuild tells ocamlopt to use async.cmi by touching async.mli *)

let () =
  try  ignore (Sys.getenv "ASYNC_BUILD_HACK" : string)
  with Not_found ->
    let ret =
      protectx (temp_dir "stop." ".hammertime") ~finally:rm_rf ~f:(fun dir ->
        let touch = Filename.concat dir "touch" in
        protectx (open_out touch) ~finally:close_out_noerr ~f:(fun oc ->
          output_string oc "\
#!/bin/bash
if [[ \"$(basename \"$1\")\" == async.mli ]]; then
  exit 0
else
  exec /bin/touch \"$@\"
fi
");

          if 0 <> Printf.ksprintf Sys.command "/bin/chmod +x %S" touch then
            failwith ("can't chmod " ^ touch ^ "to be executable");

          match Array.to_list Sys.argv with
          | "setup.ml" :: args ->
            Sys.command
              (String.concat " "
                 ("ASYNC_BUILD_HACK=cant_touch_this"
                  :: ("PATH="^dir^":$PATH")
                  :: "ocaml" :: "setup.ml" :: List.map (Printf.sprintf "%S") args))
          | _ ->
            Sys.command
              (String.concat " "
                 ("ASYNC_BUILD_HACK=cant_touch_this"
                  :: ("PATH="^dir^":$PATH")
                  :: (Array.to_list (Array.map (Printf.sprintf "%S") Sys.argv)))))
    in
    exit ret
;;


let () = setup ()
