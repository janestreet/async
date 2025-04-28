(** An alternative to [Async.Deferred_or_error_expect_test_config] in which [%expect] has
    type [unit] rather than [unit Deferred.Or_error.t]. This lets one write:

    {[
      [%expect {| |};
    ]}

    rather than:

    {[
      let%bind () = [%expect {| |}] in
    ]}

    It also means that [let%expect] cannot terminate with [%expect], and must instead
    terminate with [return ()]. *)

open! Async_kernel
include Expect_test_config_types.S with type 'a IO.t = 'a Deferred.Or_error.t
