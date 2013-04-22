open Core.Std
open Async.Std

  (*
module Sound = Async_extended.Std.Sound (* so omake isn't confused about a circular dependency *)

let () = upon (Sound.play Sound.default) (fun () -> shutdown 0)

let () = never_returns (Scheduler.go ())
  *)

let () =
  don't_wait_for begin
    let module Player = Async_jane.Std.Sound_player in
    let errors_reader, errors_writer = Pipe.create () in
    don't_wait_for (Pipe.iter_without_pushback errors_reader ~f:(fun error ->
      Printf.printf "ERROR: %s" (Error.to_string_hum error))
    );
    let player = Player.create ~error_pipe:errors_writer Player.Local in
    Deferred.List.iter (List.init 100 ~f:ignore) ~f:(fun () ->
      Player.play player ~don't_play_same_for:(sec 0.)
        ~filename:"/j/office/sounds/chime.wav";
      Clock.after (sec 0.1)
    )
  end;
  never_returns (Scheduler.go ())
