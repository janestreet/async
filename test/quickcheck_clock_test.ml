open Core.Std
open Async.Std
open Quickcheck

let span_gen =
  let open Generator in
  float_between
    ~nan:Without
    ~lower_bound:(Incl 1.)
    ~upper_bound:(Incl 10_000.)
  >>| Time.Span.of_us

let after_test () =
  async_test (Generator.list span_gen)
    ~sexp_of:<:sexp_of< Time.Span.t list >>
    ~f:(fun spans ->
      Deferred.List.fold spans ~init:(Time.now ()) ~f:(fun start span ->
        Clock.after span
        >>| fun () ->
        let finish = Time.now () in
        if Time.(<) finish (Time.add start span)
        then failwiths "Clock.after finished too soon" (start, span, finish)
               <:sexp_of< Time.t * Time.Span.t * Time.t >>;
        finish)
      >>| ignore)

let tests =
  [ "Clock.after", after_test
  ]
