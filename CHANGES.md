## Release v0.17.0

- Changes in `Async`:
  * Fix a segfault that occasionally manifests in the pingpong benchmark

- Changes in `Async_rpc`:
  * Add a new version of the `Async_rpc` wire protocol:
    * Clients and servers now exchange `Versioned_rpc` menus up-front instead of needing
      to request it via a special RPC, and bin shape digests are included in the menu to
      allow for clients and servers to check compatibility at runtime.
    * Clients and servers can exchange a user-provided identification string
    * `Authorization_failure` was added as an additional structured RPC error type, as
      well as `Unknown` to cover future cases
  * Add an argument to rpc creator functions to specify how `Async_rpc` should determine
    whether the result of the RPC was a success or error. This information is exposed in
    `Tracing_event.t`'s via `Async_rpc_kernel.Async_rpc_kernel_private.Connection.events`,
    which is intended to be used by observability libraries to generate metrics and traces
    from RPC activity.
  * Add a note in `Rpc_transport` pointing out that reducing reader/writer buffer sizes
    can have unexpected effects due to differences in how the the buffers get allocated if
    they're under or over the threshold to use mmap.
  * Add `Connection.serve_unix` for serving specifically on unix domain sockets which
    supports extracting the peer's credentials for authentication.
  * Add the ability to enable dumping RPC message buffers on deserialization errors. This
    functionality is intended to help debugging cases where invalid messages are being
    received on an RPC connection and where it's unclear where the corruption is
    happening.
  * Mention the `ASYNC_RPC_MAX_MESSAGE_SIZE` env var in the error messages for overly
    large messages.
  * Fix a bug where an exception getting raised from trying to write an response would get
    caught and attempted to be written again as an error instead of allowing it to
    propagate to close the connection.

- Changes in `Async_quickcheck`:
  * Added `Async_quickcheck.async_test_or_error, for getting a `Result.t` with the failing
    input and the error when the test fails.

- Changes in `Persistent_connection`:
  * Take `Event` out of the functor and parameterize it so that it's easier to write
    generic functions that operate on events.
  * Add `Make'` which allows for using a custom error type instead of `Error.t`.

- Changes in `Unpack_sequence`:
  * Allow specifying a `size_budget` for the pipe when calling
    `Unpack_sequence.unpack_into_pipe`.

## Release v0.16.0

- Update `Async_command.with_options`:
  * Add optional `behave_nicely_in_pipeline` parameter
  * If true, calls `Writer.behave_nicely_in_pipeline()` when the command starts
  * Default value is true

- Update `Async_quickcheck.async_test` function:
  * Add optional `sizes` parameter to specify a sequence of sizes for generated test data
  * Add optional `shrinker` parameter to provide a custom shrinker for the test data
  * Add optional `shrink_attempts` parameter to control the number of attempts at shrinking test data

- Changes in `Async_rpc.Rpc`:
 * Change `handshake_timeout` parameter type from `Time.Span.t` to `Time_float.Span.t`.
 * Remove `on_handshake_error` type from `Rpc.Connection` to simplify the interface

- Changes in `Async_rpc.Rpc_transport`:
  * Add `writer_buffer_size` parameter to `Rpc_transport.of_fd`

- Add new function to `Rpc_transport_low_latency.Reader.With_internal_reader`:
  * `read_one_message_bin_prot_without_buffering`
    - Similar to `Rpc_kernel.Transport.Reader.read_one_message_bin_prot`
    - Reads only the necessary bytes for a single bin prot message from the underlying file descriptor

- Changes in `Lock_file_async.Flock`:
  * `Flock.lock_exn` and `Flock.lock` now have optional `exclusive` and `close_on_exec` parameters
    - `exclusive` controls exclusive access to the lock
    - `close_on_exec` specifies whether to close the lock when executing a new program

- Update functions in `Flock` module in `Lock_file_async`:
  * Added an optional `exclusive` parameter, enabling shared locks
    when set to false (previously exclusive locks only)
  * Add optional `close_on_exec` parameter to control if the lock is closed on exec

- Changes in `Persistent_connection`:
  * `retry_delay` and `handshake_timeout` changed from `Time.Span.t` to `Time_float.Span.t`

- Changes in `Unpack_sequence`:
  * Added a new function: `Unpack_sequence.unpack_iter_with_pushback`
    - Similar to `unpack_iter`, but waits for `pushback` after unpacking every chunk

## Old pre-v0.15 changelogs (very likely stale and incomplete)

## git version

- Added a new (single-module) library `async.log_extended`, extending
  `Async.Log` with `Console` and `Syslog` submodules.

- Improved Async scheduler fairness by calling Thread.yield every cycle, which
  releases the OCaml lock if any other OCaml threads are waiting to acquire it.

- Added a new (single-module) library `async.unpack_sequence`, for efficiently
  unpacking a sequence of packed values coming from a `string Pipe.Reader.t` or
  a `Reader.t`.

- Increased Unix.listen's default backlog from 10 to 64, to reduce occurrences
  of TCP clients getting Unexpected EOF errors when the listening server is
  busy.

- Added an optional argument to Pipe functions fold and iter so they can
  consider a value flushed when it is processed by the supplied ~f rather than
  when it is read out of the pipe.

- `Weak_hashtbl` was moved into its own library `Weak_hashtbl_async`, which is
  released as part of the async package.

- Added function `Tcp.Server.close_finished_and_handlers_determined`.

## v0.10

- Moved `Persistent_connection` to `Async_kernel`, so that it can be used in
  javascript.

- Improved `Log.Output` to write, flush, rotate, or close all `Log.Output.t`s,
  even if one raises

- Added `Async.print_s` for pretty printing a sexp to stdout.

- Removed a per-cycle allocation from the scheduler.

- Fixed `Reader.load_bin_prot` to always return `Error` when there is an error,
  rather than return `Error` in some situations and raise in others.
- Improved the behavior of shutdown when there are errors.

- Added `Scheduler.may_sleep_for_thread_fairness`, an opt-in configuration that
  improves thread fairness.

- Added to `Sys.file_exists` function a `follow_symlinks` optional argument.

- Fixed the Async scheduler so that for top-level unhandled exceptions, it runs
  `at_exit` functions once, not twice.

- For `Writer.open_file`, exposed the syscall optional argument.

- Exposed `Async.ifprintf`, which had previously been mistakenly shadowed even
  though it doesn't block.

- Unified `Synchronous_time_source.t` and `Time_source.t`, into a single data
  structure, allowing one to convert between them as needed. This involved
  substantive changes to Async's clock.

- Added function `Time_source.timing_wheel_now`.

- Added stable types for `Log.Level` and `Log.Output.format`.

- Improved shutdown function so that when shutdown is forced (i.e. `at_shutdown`
  handlers time out), it calls `Pervasives.exit`, which runs `at_exit` handlers.
  This improves `Command.async` in this situation, due to its use of an
  `at_exit` handler to print errors.

- Improved `Process.run`'s error message when `working_dir` is missing.

- Fixed `Rpc.Connection.create` so that it doesn't raise on a failed handshake.

- Significantly improved the performance of `Log.printf` when the log message
  won't be printed, by using `ifprintf` to avoid constructing the message.

- Added `Socket.Address` functions `Inet.to_sockaddr` and `Unix.to_sockaddr`,
  the type specialized versions of `Socket.Address.to_sockaddr`.

- Added `Socket.bind_inet`, which is like bind, but restricted to Inet addresses
  and does not return a `Deferred.t`. Changed `Udp.bind` and `bind_any` to not
  return a `Deferred.t`.

- Added to `File_tail.create` an optional `?throttle` argument so that one can
  use a custom throttle and `max_concurrent_jobs` rather than a global throttle
  with a fixed `max_concurrent_jobs`.

- Renamed `Tcp` and `Rpc`'s `via_local_interface` and `via_local_port` arguments
  as `bind_to_address` and `bind_to_port`.

- Made `Tcp.Server.create` and `create_sock`'s `~on_handler_error` argument
  mandatory.

- In `Tcp.Server`, stopped calling `on_handler_error` for `Writer` error from
  `inner_monitor`, which only indicated that the client likely closed the
  connection before all the bytes could be written.

- Renamed `Command.async` as `async_spec` and `Command.async'` as `async`.  We
  want to encourage the use of `Command.Param` and discourage the use of
  `Command.Spec`.

- Changed `Async` so that in tests it uses synchronous output.

- Changed `Async`'s default max number of open file descriptors from `8_192` to
  the minimum of `32_768` and `ulimit -n -H`.

- In the Async scheduler's main loop, avoided calling `Time_ns.now` and
  `Linux_ext.Timerfd.set_after` unless they are needed. This saves about 50ns
  per cycle.

- Moved `Tcp` functions for specifying where to connect and where to listen into
  submodules: `Where_to_connect` and `Where_to_listen`

- Changed `Tcp.to_host_and_port` from taking a string and int to
  `Tcp.Where_to_connect.of_host_and_port`, taking a `Host_and_port.t`

- Changed `Rpc.Connection.client` to take a `Tcp.Where_to_connect.t` instead of
  `~host ~port`.

- Changed `Synchronous_time_source.Event.abort` to return a variant type, in the
  same style as `Time_source.Event.abort`. Added `abort_exn` and
  `abort_if_possible`, also in the same style as `Time_source.Event`.

- Added function `Scheduler.long_cycles`, which returns the stream of cycles
  whose duration exceeds a user-supplied time span. This is more efficient than
  `cycle_times`, because it only allocates a stream element when there is a long
  cycle, rather than on every cycle.

- Made internal libraries stdless: `Async_unix`, `Async_extra`.

- Changed `Udp.recvfrom_loop` and `read_loop` functions to return a variant
  `Closed | Stopped` rather than `unit`.

- Extended the `Unix.Inet_addr` module's interface to include
  `Core.Unix.Inet_addr`'s interface.

## v0.9

## 113.43.00

- Added some expect tests of `Monitor`, in particular
  `Monitor.handle_errors`.

- Added a benchmark of `Monitor.try_with`.

## 113.33.00

Keep up to date with interface changes in `Async_kernel`, `Async_extra` and
`Async_unix`.

## 113.24.00

Keep up to date with interface changes in `Async_kernel`, `Async_extra` and
`Async_unix`.

## 113.00.00

- Added `Async.Std.Printf` module so that one doesn't unintentionally use
  blocking `Core.Std.Printf` functions in an Async program.

    There was much pre-existing code that did this via:

    : open Core.Std
    : open Async.Std

    `Async.Std.Printf` defines blocking functions (e.g `printf`,
    `eprintf`) to cause a type error, but leaves Async-friendly functions
    (e.g. `sprintf`, `ksprintf`) untouched.

    Replaced uses of `Printf.*`, with `Core.Std.Printf.*` where needed.

## 112.35.00

- Include some previously-omitted benchmarks

## 112.24.00

Keep up to date with interface changes in `Async_kernel`, `Async_extra` and
`Async_unix`.

## 112.17.00

Added tests and updated examples

## 112.01.00

- update tests

## 111.25.00

- add a dns example

## 111.11.00

- Updated the sound.ml example

## 109.53.00

- Bump version number

## 109.14.00

- Added function `Monitor.kill`, which kills a monitor and all its
  descendants.

    This prevents any jobs from ever running in the monitor again.

## 109.09.00

- Switched `Async.Std`'s toplevel bindings for `Deferred.Or_error`'s `bind` and `map` to use
  `Deferred.Result`.
  This allows them to be used with any `'error` type, rather than just `Error.t`.

## 109.05.00

- Added `val _squelch_unused_module_warning_` to `Async.Std`.
