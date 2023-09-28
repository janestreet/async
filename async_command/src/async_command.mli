(** [Async.Command] is {{!Core.Command}[Core.Command]} with additional Async functions. *)

open! Core
open! Import

(** @open *)
include module type of struct
  include Core.Command
end

type 'a with_options = ?behave_nicely_in_pipeline:bool -> ?extract_exn:bool -> 'a

(** [async] is like [Core.Command.basic], except that the main function it expects returns
    [unit Deferred.t], instead of [unit].  [async] will also start the Async scheduler
    before main is run, and will stop the scheduler when main returns.

    [async] also handles top-level exceptions by wrapping the user-supplied function in a
    [Monitor.try_with]. If an exception is raised, it will print it to stderr and call
    [shutdown 1]. The [extract_exn] argument is passed along to [Monitor.try_with]; by
    default it is [false].

    If [behave_nicely_in_pipeline] is true, then [Writer.behave_nicely_in_pipeline ()]
    is called when the command starts. Its default is [true]. *)
val async : unit Deferred.t basic_command with_options

val async_spec : ('a, unit Deferred.t) basic_spec_command with_options

(** [async_or_error] is like [async], except that the main function it expects may
    return an error, in which case it prints out the error message and shuts down with
    exit code 1. *)
val async_or_error : unit Deferred.Or_error.t basic_command with_options

val async_spec_or_error : ('a, unit Deferred.Or_error.t) basic_spec_command with_options

(** Staged functions allow the main function to be separated into two stages.  The first
    part is guaranteed to run before the Async scheduler is started, and the second part
    will run after the scheduler is started.  This is useful if the main function runs
    code that relies on the fact that threads have not been created yet
    (e.g., [Daemon.daemonize]).

    As an example:
    {[
      let main () =
        assert (not (Scheduler.is_running ()));
        stage (fun `Scheduler_started ->
          assert (Scheduler.is_running ());
          Deferred.unit
        )
    ]}
*)

type 'r staged = ([ `Scheduler_started ] -> 'r) Staged.t

module Staged : sig
  val async : unit Deferred.t staged basic_command with_options
  val async_spec : ('a, unit Deferred.t staged) basic_spec_command with_options
  val async_or_error : unit Deferred.Or_error.t staged basic_command with_options

  val async_spec_or_error
    : ('a, unit Deferred.Or_error.t staged) basic_spec_command with_options
end

(** To create an [Arg_type.t] that uses auto-completion and uses Async to compute the
    possible completions, one should use

    {[
      Arg_type.create ~complete of_string
    ]}

    where [complete] wraps its Async operations in [Thread_safe.block_on_async].  With
    this, the [complete] function is only called when the executable is auto-completing,
    not for ordinary execution.  This improves performance, and also means that the Async
    scheduler isn't started for ordinary execution of the command, which makes it possible
    for the command to daemonize (which requires the scheduler to not have been started).
*)

module For_testing : sig
  (** Invoke a [Command.t] that possibly is one of the above async choices from within a
      test already running async.

      Note that for the [Staged] variants, we can't have them run before Async starts
      (since that ship has already sailed!) So anything that actually relies on that is
      unlikely to work. *)
  val run_from_within_async : argv:string list -> t -> unit Or_error.t Deferred.t
end
