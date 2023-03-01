open! Core
open! Async_kernel
open! Async_unix
module IO = Async_kernel.Deferred.Or_error

let run f = Thread_safe.block_on_async_exn f |> Or_error.ok_exn
let sanitize s = s
let upon_unreleasable_issue = Expect_test_config.upon_unreleasable_issue
