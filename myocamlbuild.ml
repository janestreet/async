(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules ->
    (* ocamlbuild rule for native packs seems to be broken, it tries to touch
       lib/async.mli. The workaroung is to remove "touch" at the beginning of commands
       executed by ocamlbuild. *)
    let module U = Ocamlbuild_pack.My_unix in
    let orig_execute_many = U.implem.U.execute_many in
    let execute_many ?max_jobs ?ticker ?period ?display commands =
      let commands =
        List.map
          (List.map (fun task () ->
            let cmd = task () in
            if String.is_prefix "touch " cmd then begin
              Ocamlbuild_pack.Log.eprintf
                "removing 'touch' inserted by broken ocamlbuild rule for packed modules.";
              let idx = String.index cmd ';' in
              String.after cmd (idx + 1)
            end else
              cmd))
          commands
      in
      orig_execute_many ?max_jobs ?ticker ?period ?display commands
    in
    U.implem.U.execute_many <- execute_many
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
