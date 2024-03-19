open! Core
open! Async

let with_conn implementations ~f =
  let implementations =
    Rpc.Implementations.create_exn ~implementations ~on_unknown_rpc:`Raise
  in
  let%bind server =
    Rpc.Connection.serve
      ~initial_connection_state:(fun _ _ -> ())
      ~implementations
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ()
  in
  let port = Tcp.Server.listening_on server in
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let%bind conn = Rpc.Connection.client where_to_connect >>| Result.ok_exn in
  f conn
;;

let to_sexp rpc_result sexp_of_content =
  match rpc_result with
  | Ok v -> [%sexp Ok, (sexp_of_content v : Sexp.t)]
  | Error (Async_rpc_kernel.Rpc_error.Authorization_failure exn) ->
    [%sexp Error, (Uncaught_exn, (exn : Sexp.t))]
  (* Other errors should not happen *)
  | Error _ -> failwith "Unexpected error"
;;

let%test_module "RPC" =
  (module struct
    let rpc_with_name ~name =
      Rpc.Rpc.create
        ~name
        ~version:1
        ~bin_query:[%bin_type_class: int]
        ~bin_response:[%bin_type_class: int]
        ~include_in_error_count:Only_on_exn
    ;;

    let rpc = rpc_with_name ~name:"test-rpc-with-auth"
    let rpc' = rpc_with_name ~name:"test-rpc-with-auth-sync"

    let implementation =
      let open Async_rpc_kernel in
      [ Rpc.Rpc.implement_with_auth rpc (fun () query ->
          (if query < 0
           then Or_not_authorized.Not_authorized (Error.of_string "query negative")
           else Authorized query)
          |> Deferred.return)
      ; Rpc.Rpc.implement_with_auth' rpc' (fun () query ->
          if query < 0
          then Or_not_authorized.Not_authorized (Error.of_string "query negative")
          else Authorized query)
      ]
    ;;

    let%expect_test "rpc-with-auth" =
      with_conn implementation ~f:(fun conn ->
        let%bind response = Rpc.Rpc.dispatch' rpc conn 123 in
        print_s (to_sexp response [%sexp_of: int]);
        [%expect {| (Ok 123) |}];
        let%bind response = Rpc.Rpc.dispatch' rpc conn (-124) in
        print_s (to_sexp response [%sexp_of: int]);
        [%expect
          {|
          (Error
           (Uncaught_exn
            ((location "server-side rpc authorization") (exn "query negative"))))
          |}];
        let%bind response = Rpc.Rpc.dispatch' rpc' conn 125 in
        print_s (to_sexp response [%sexp_of: int]);
        [%expect {| (Ok 125) |}];
        let%bind response = Rpc.Rpc.dispatch' rpc' conn (-126) in
        print_s (to_sexp response [%sexp_of: int]);
        [%expect
          {|
          (Error
           (Uncaught_exn
            ((location "server-side rpc authorization") (exn "query negative"))))
          |}];
        Deferred.unit)
    ;;
  end)
;;

let%test_module "pipe-rpc" =
  (module struct
    let rpc_with_name ~name =
      Rpc.Pipe_rpc.create
        ~name
        ~version:1
        ~bin_query:[%bin_type_class: int]
        ~bin_response:[%bin_type_class: int]
        ~bin_error:[%bin_type_class: Error.t]
        ()
    ;;

    let rpc = rpc_with_name ~name:"test-pipe-rpc-with-auth"
    let rpc_direct = rpc_with_name ~name:"test-pipe-rpc-with-auth_direct"

    let implementation =
      let open Async_rpc_kernel in
      [ Rpc.Pipe_rpc.implement_with_auth rpc (fun () query ->
          Deferred.return
            (if query < 0
             then Or_not_authorized.Not_authorized (Error.of_string "query negative")
             else (
               let pipe =
                 Pipe.create_reader ~close_on_exception:true (Fn.flip Pipe.write query)
               in
               Authorized (Ok pipe))))
      ; Rpc.Pipe_rpc.implement_direct_with_auth rpc_direct (fun () query writer ->
          if query < 0
          then
            Or_not_authorized.Not_authorized (Error.of_string "query negative")
            |> Deferred.return
          else (
            match Rpc.Pipe_rpc.Direct_stream_writer.write writer query with
            | `Flushed x ->
              let%map () = x in
              Or_not_authorized.Authorized (Ok ())
            | `Closed -> assert false))
      ]
    ;;

    let%expect_test "pipe-rpc-with-auth" =
      with_conn implementation ~f:(fun conn ->
        let sexp_first_update result =
          let pipe, _ = result |> Result.ok |> Option.value_exn in
          Pipe.read_now_exn pipe |> [%sexp_of: int]
        in
        let%bind response = Rpc.Pipe_rpc.dispatch' rpc conn 127 in
        print_s (to_sexp response sexp_first_update);
        [%expect {| (Ok 127) |}];
        let%bind response = Rpc.Pipe_rpc.dispatch' rpc conn (-128) in
        print_s (to_sexp response sexp_first_update);
        [%expect
          {|
          (Error
           (Uncaught_exn
            ((location "server-side pipe_rpc authorization") (exn "query negative"))))
          |}];
        let%bind response = Rpc.Pipe_rpc.dispatch' rpc_direct conn 129 in
        print_s (to_sexp response sexp_first_update);
        [%expect {| (Ok 129) |}];
        let%bind response = Rpc.Pipe_rpc.dispatch' rpc_direct conn (-130) in
        print_s (to_sexp response sexp_first_update);
        [%expect
          {|
          (Error
           (Uncaught_exn
            ((location "server-side pipe_rpc authorization") (exn "query negative"))))
          |}];
        Deferred.unit)
    ;;
  end)
;;

let%test_module "state-rpc" =
  (module struct
    let rpc_with_name ~name =
      Rpc.State_rpc.create
        ~name
        ~version:1
        ~bin_query:[%bin_type_class: int]
        ~bin_state:[%bin_type_class: int]
        ~bin_update:[%bin_type_class: int]
        ~bin_error:[%bin_type_class: Error.t]
        ()
    ;;

    let rpc = rpc_with_name ~name:"test-state-rpc-with-auth"
    let rpc_direct = rpc_with_name ~name:"test-state-rpc-with-auth_direct"

    let implementation =
      let open Async_rpc_kernel in
      [ Rpc.State_rpc.implement_with_auth rpc (fun () query ->
          Deferred.return
            (if query < 0
             then Or_not_authorized.Not_authorized (Error.of_string "query negative")
             else (
               let pipe =
                 Pipe.create_reader ~close_on_exception:true (Fn.flip Pipe.write query)
               in
               Authorized (Ok (query, pipe)))))
      ; Rpc.State_rpc.implement_direct_with_auth rpc_direct (fun () query writer ->
          if query < 0
          then
            Or_not_authorized.Not_authorized (Error.of_string "query negative")
            |> Deferred.return
          else (
            match Rpc.Pipe_rpc.Direct_stream_writer.write writer query with
            | `Flushed x ->
              let%map () = x in
              Or_not_authorized.Authorized (Ok query)
            | `Closed -> assert false))
      ]
    ;;

    let%expect_test "state-rpc-with-auth" =
      with_conn implementation ~f:(fun conn ->
        let sexp_state_and_first_update result =
          let state, pipe, _ = result |> Result.ok |> Option.value_exn in
          [%message (state : int) (Pipe.read_now_exn pipe : int)]
        in
        let%bind response = Rpc.State_rpc.dispatch' rpc conn 131 in
        print_s (to_sexp response sexp_state_and_first_update);
        [%expect {| (Ok ((state 131) ("Pipe.read_now_exn pipe" 131))) |}];
        let%bind response = Rpc.State_rpc.dispatch' rpc conn (-132) in
        print_s (to_sexp response sexp_state_and_first_update);
        [%expect
          {|
          (Error
           (Uncaught_exn
            ((location "server-side pipe_rpc authorization") (exn "query negative"))))
          |}];
        let%bind response = Rpc.State_rpc.dispatch' rpc_direct conn 133 in
        print_s (to_sexp response sexp_state_and_first_update);
        [%expect {| (Ok ((state 133) ("Pipe.read_now_exn pipe" 133))) |}];
        let%bind response = Rpc.State_rpc.dispatch' rpc_direct conn (-134) in
        print_s (to_sexp response sexp_state_and_first_update);
        [%expect
          {|
          (Error
           (Uncaught_exn
            ((location "server-side pipe_rpc authorization") (exn "query negative"))))
          |}];
        Deferred.unit)
    ;;
  end)
;;
