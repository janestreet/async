open! Core
open! Async

let trials = 5
let examples = [ "Example 1"; "Example 2"; "Example 3" ]
let generator = Quickcheck.Generator.return "Generated value"

let%expect_test "[~examples] are tested first" =
  let%bind () =
    Async_quickcheck.async_test ~trials ~examples generator ~f:(fun string ->
      print_endline string;
      return ())
  in
  [%expect
    {|
    Example 1
    Example 2
    Example 3
    Generated value
    Generated value
    Generated value
    Generated value
    Generated value
    |}];
  let%bind result =
    Async_quickcheck.async_test_or_error ~trials ~examples generator ~f:(fun string ->
      print_endline string;
      return (Ok ()))
  in
  let () = Result.ok result |> Option.value_exn in
  [%expect
    {|
    Example 1
    Example 2
    Example 3
    Generated value
    Generated value
    Generated value
    Generated value
    Generated value
    |}];
  return ()
;;

let%expect_test "[test], [async_test], and [async_result] handle [~examples] the same" =
  let%bind () =
    Async_quickcheck.async_test ~trials ~examples generator ~f:(fun string ->
      print_endline string;
      return ())
  in
  let async_test_output = Expect_test_helpers_base.expect_test_output () in
  let%bind result =
    Async_quickcheck.async_test_or_error ~trials ~examples generator ~f:(fun string ->
      print_endline string;
      return (Ok ()))
  in
  let () = Result.ok result |> Option.value_exn in
  let async_test_or_error_output = Expect_test_helpers_base.expect_test_output () in
  Async_quickcheck.test ~trials ~examples generator ~f:print_endline;
  let test_output = Expect_test_helpers_base.expect_test_output () in
  Expect_test_helpers_base.require_equal (module String) async_test_output test_output;
  Expect_test_helpers_base.require_equal
    (module String)
    async_test_or_error_output
    test_output;
  return ()
;;

let%expect_test "shrinkers are applied" =
  let shrinker =
    Quickcheck.Shrinker.create (function
      | "SHRUNKEN" -> Sequence.empty
      | _ -> Sequence.singleton "SHRUNKEN")
  in
  let%bind () =
    Expect_test_helpers_async.require_does_raise_async (fun () ->
      Async_quickcheck.async_test
        ~sexp_of:String.sexp_of_t
        ~shrinker
        ~trials
        ~examples
        generator
        ~f:(fun (_ : _) -> assert false))
  in
  [%expect
    {| ("random input" SHRUNKEN "Assert_failure test_async_quickcheck.ml:83:27") |}];
  let%map () =
    Expect_test_helpers_async.require_does_raise_async (fun () ->
      let%map result =
        Async_quickcheck.async_test_or_error
          ~shrinker
          ~trials
          ~examples
          generator
          ~f:(fun (_ : _) -> assert false)
      in
      match result with
      | Ok () -> return ()
      | Error (input, error) ->
        Error.tag_arg error "random input" input String.sexp_of_t |> Error.raise)
  in
  [%expect
    {| ("random input" SHRUNKEN "Assert_failure test_async_quickcheck.ml:95:29") |}]
;;
