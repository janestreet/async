open! Core
open! Async_kernel

module type Quickcheck_async_configured = sig
  include Quickcheck.Quickcheck_configured

  (** Like [test], but for asynchronous tests. *)
  val async_test
    :  ?seed:Quickcheck.seed
    -> ?sizes:int Sequence.t
    -> ?trials:int
    -> ?shrinker:'a Quickcheck.Shrinker.t
    -> ?shrink_attempts:Quickcheck.shrink_attempts
    -> ?sexp_of:('a -> Sexp.t)
    -> ?examples:'a list
    -> 'a Quickcheck.Generator.t
    -> f:('a -> unit Deferred.t)
    -> unit Deferred.t

  (** [async_test_or_error] is like [test], except failure is determined using
      [Or_error.t]. Any exceptions raised by [f] are also treated as failures. The failing
      input and the error are returned in a [Result.t]. *)
  val async_test_or_error
    :  ?seed:Quickcheck.seed
    -> ?sizes:int Sequence.t
    -> ?trials:int
    -> ?shrinker:'a Quickcheck.Shrinker.t
    -> ?shrink_attempts:Quickcheck.shrink_attempts
    -> ?examples:'a list
    -> 'a Quickcheck.Generator.t
    -> f:('a -> unit Or_error.t Deferred.t)
    -> (unit, 'a * Error.t) Result.t Deferred.t
end
