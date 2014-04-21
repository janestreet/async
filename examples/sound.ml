open Core.Std
open Async.Std

  (*
module Sound = Async_extended.Std.Sound (* so omake isn't confused about a circular dependency *)

let () = upon (Sound.play Sound.default) (fun () -> shutdown 0)

let () = never_returns (Scheduler.go ())
  *)

let () =
  Command.async_basic ~summary:"testing jane sound player"
    Command.Spec.(
      empty
      +> anon ("SOUND-FILE" %: file)
      +> anon ("TIMES" %: int)
      +> anon ("INTERVAL" %: Arg_type.create Time.Span.of_string)
    )
    (fun filename times interval () ->
       let module Player = Async_jane.Std.Sound_player in
       let errors_reader, errors_writer = Pipe.create () in
       don't_wait_for (Pipe.iter_without_pushback errors_reader ~f:(fun error ->
         Log.Global.error "%s" (Error.to_string_hum error))
       );
       let player =
         Player.create ~error_pipe:errors_writer Player.Playback_location.Local
       in
       Deferred.List.iter (List.init times ~f:ignore) ~f:(fun () ->
         Player.play player ~don't_play_same_for:(sec 0.) ~filename;
         Clock.after interval
       )
       >>= never
    )
  |> Command.run
