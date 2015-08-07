open Core.Std
open Async.Std

let command =
  Command.async' ~summary:"test command"
    Command.Param.(
      anon ("STRING" %: string)
      @> flag "some-flag" (required string) ~doc:""
      @> nil)
    (fun anon flag () ->
       print_string (String.concat [ "\
anon: ";anon;"
flag: ";flag;"
"
                      ]);
       after (sec 0.01)
       >>= fun () ->
       printf "later\n";
       return ())
;;

let () = Command.run command
