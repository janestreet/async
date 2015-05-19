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

