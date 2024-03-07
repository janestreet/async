open! Core
open! Async_kernel
open Deferred.Infix
module Generator = Quickcheck.Generator
module Observer = Quickcheck.Observer
module Shrinker = Quickcheck.Shrinker

module Configure (Config : Quickcheck.Quickcheck_config) = struct
  include Quickcheck.Configure (Config)

  let shrink_error ~shrinker ~shrink_count ~f input error =
    let rec loop ~shrink_count ~alternates input error =
      match shrink_count with
      | 0 -> return (input, error)
      | _ ->
        let shrink_count = shrink_count - 1 in
        (match Sequence.next alternates with
         | None -> return (input, error)
         | Some (alternate, alternates) ->
           (match%bind f alternate with
            | Ok () -> loop ~shrink_count ~alternates input error
            | Error error ->
              let alternates = Shrinker.shrink shrinker alternate in
              loop ~shrink_count ~alternates alternate error))
    in
    let alternates = Shrinker.shrink shrinker input in
    loop ~shrink_count ~alternates input error
  ;;

  let test_cases ?seed ?sizes ~examples ~trials quickcheck_generator =
    Sequence.append
      (Sequence.of_list examples)
      (Sequence.take (random_sequence ?seed ?sizes quickcheck_generator) trials)
  ;;

  let failing_case ~test_cases ~f =
    Sequence.delayed_fold
      test_cases
      ~init:()
      ~f:(fun () x ~k ->
        match%bind f x with
        | Error error -> return (Some (x, error))
        | Ok () -> k ())
      ~finish:(fun () -> return None)
  ;;

  let shrunken_case ~shrinker ~failing_case ~shrink_attempts ~f =
    match shrinker with
    | None -> return failing_case
    | Some shrinker ->
      let shrink_count =
        match shrink_attempts with
        | `Limit n -> n
        | `Exhaustive -> Int.max_value
      in
      (match failing_case with
       | Some (input, error) ->
         shrink_error ~shrinker ~shrink_count ~f input error >>| Option.some
       | None -> return None)
  ;;

  let async_test
    ?seed
    ?sizes
    ?(trials = default_trial_count)
    ?shrinker
    ?(shrink_attempts = default_shrink_attempts)
    ?sexp_of
    ?(examples = [])
    quickcheck_generator
    ~f
    =
    let f x =
      Deferred.Or_error.try_with ~run:`Now ~rest:`Raise ~extract_exn:true (fun () -> f x)
    in
    let test_cases = test_cases ?seed ?sizes ~examples ~trials quickcheck_generator in
    let%bind failing_case = failing_case ~test_cases ~f in
    let%map shrunken_case = shrunken_case ~shrinker ~failing_case ~shrink_attempts ~f in
    match shrunken_case with
    | None -> ()
    | Some (input, error) ->
      let tagged_error =
        match sexp_of with
        | None -> error
        | Some sexp_of_arg -> Error.tag_arg error "random input" input sexp_of_arg
      in
      Error.raise tagged_error
  ;;

  let async_test_or_error
    ?seed
    ?sizes
    ?(trials = default_trial_count)
    ?shrinker
    ?(shrink_attempts = default_shrink_attempts)
    ?(examples = [])
    quickcheck_generator
    ~f
    =
    let f x =
      Deferred.Or_error.try_with_join ~run:`Now ~rest:`Raise ~extract_exn:true (fun () ->
        f x)
    in
    let test_cases = test_cases ?seed ?sizes ~examples ~trials quickcheck_generator in
    let%bind failing_case = failing_case ~test_cases ~f in
    let%map shrunken_case = shrunken_case ~shrinker ~failing_case ~shrink_attempts ~f in
    match shrunken_case with
    | None -> Ok ()
    | Some (input, error) -> Error (input, error)
  ;;
end

include Configure (Quickcheck)
