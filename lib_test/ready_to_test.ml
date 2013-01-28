open Core.Std
open Async.Std

let already_closed () =
  Unix.pipe (Info.of_string "Ready_to_test.already_closed")
  >>= fun (`Reader fd, `Writer write_fd) ->
  let ready1 = Fd.ready_to fd `Read in
  (* At this point, the fd should be Open, Watching for read. *)
  let close = Fd.close fd in
  (* Now that we've requested a closed, the fd should be Close_requested,
     Stop_requested for read.  So, another call to [ready_to] should notice the close. *)
  let ready2 = Fd.ready_to fd `Read in
  close
  >>= fun () ->
  ready1
  >>= function
  | `Bad_fd | `Ready -> assert false
  | `Closed ->
    ready2
    >>= function
    | `Bad_fd | `Ready -> assert false
    | `Closed -> Fd.close write_fd
;;

let tests =
  [ "Ready_to_test.already_closed", already_closed;
  ]
;;
