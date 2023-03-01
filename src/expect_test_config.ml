open! Async_kernel
open! Async_unix
module IO = Deferred
module Expect_test_config = Core.Expect_test_config

let run f = Thread_safe.block_on_async_exn f
let sanitize s = s
let upon_unreleasable_issue = Expect_test_config.upon_unreleasable_issue
