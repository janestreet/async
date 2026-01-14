open Core
open Async

module Hello = struct
  module Model = struct
    let name = "hello"

    type query = unit
    type response = unit
  end

  include Model
  include Versioned_rpc.Caller_converts.Rpc.Make (Model)

  module V1 = Register (struct
      let version = 1

      type query = unit [@@deriving bin_io]
      type response = unit [@@deriving bin_io]

      let query_of_model = Fn.id
      let model_of_response = Fn.id
    end)
end

let%expect_test _ =
  let implementations =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:
        [ Rpc.Rpc.implement Hello.V1.rpc (fun () () ->
            printf "server says hi\n";
            return ())
        ]
      ~on_exception:Log_on_background_exn
  in
  let%bind server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr _conn -> ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ()
  in
  let host_and_port =
    Host_and_port.create ~host:"localhost" ~port:(Tcp.Server.listening_on server)
  in
  (* test Persistent_connection.Rpc *)
  let on_unversioned_event
    : Host_and_port.t Persistent_connection.Rpc.Event.t -> unit Deferred.t
    = function
    | Obtained_address _ ->
      printf "(Obtained_address <elided>)\n";
      return ()
    | event ->
      print_s [%sexp (event : Host_and_port.t Persistent_connection.Rpc.Event.t)];
      return ()
  in
  let unversioned_conn =
    Persistent_connection.Rpc.create'
      ~on_event:on_unversioned_event
      ~server_name:"unversioned rpc"
      (fun () -> return (Ok host_and_port))
  in
  [%expect {| Attempting_to_connect |}];
  let%bind this_conn = Persistent_connection.Rpc.connected unversioned_conn in
  [%expect
    {|
    (Obtained_address <elided>)
    (Connected <opaque>)
    |}];
  let%bind () = Rpc.Rpc.dispatch_exn Hello.V1.rpc this_conn () in
  [%expect {| server says hi |}];
  let%bind () = Persistent_connection.Rpc.close unversioned_conn in
  [%expect {| Disconnected |}];
  (* test Persistent_connection.Versioned_rpc *)
  let on_versioned_event
    : Host_and_port.t Persistent_connection.Versioned_rpc.Event.t -> unit Deferred.t
    = function
    | Obtained_address _ ->
      printf "(Obtained_address <elided>)\n";
      return ()
    | event ->
      print_s
        [%sexp (event : Host_and_port.t Persistent_connection.Versioned_rpc.Event.t)];
      return ()
  in
  let versioned_conn =
    Persistent_connection.Versioned_rpc.create'
      ~on_event:on_versioned_event
      ~server_name:"versioned rpc"
      (fun () -> return (Ok host_and_port))
  in
  [%expect {| Attempting_to_connect |}];
  let%bind this_conn = Persistent_connection.Versioned_rpc.connected versioned_conn in
  [%expect
    {|
    (Obtained_address <elided>)
    (Connected <opaque>)
    |}];
  let%bind () = Hello.dispatch_multi this_conn () |> Deferred.Or_error.ok_exn in
  [%expect {| server says hi |}];
  let%bind () = Persistent_connection.Versioned_rpc.close versioned_conn in
  [%expect {| Disconnected |}];
  return ()
;;

let%expect_test "event logging" =
  let open Expect_test_helpers_base in
  let implementations =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:
        [ Rpc.Rpc.implement Hello.V1.rpc (fun () () ->
            printf "server says hi\n";
            return ())
        ]
      ~on_exception:Log_on_background_exn
  in
  let port = 1234 in
  let%bind _server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr _conn -> ())
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  let host_and_port = Host_and_port.create ~host:"localhost" ~port in
  let address_lookup_fail = ref true in
  let print_source_code_positions_once = ref true in
  let persistent_conn =
    Persistent_connection.Rpc.create'
      ~server_name:"rpc test"
      ~log:
        (Log.create
           ~level:`Debug
           ~output:
             [ Log.Output.create_unbuffered
                 ~flush:(fun () -> return ())
                 (fun e -> print_s [%sexp (e : Log.Message_event.Unstable.t)])
             ]
           ~on_error:`Raise
           ~transform:(fun message ->
             if !print_source_code_positions_once
             then (
               print_source_code_positions_once := false;
               let logged_at = Log.Message_event.source message in
               print_s [%sexp (logged_at : Ppx_log_types.Message_source.t)]);
             message)
           ())
      (fun () ->
        (* fail once so we can see the error event *)
        if !address_lookup_fail
        then (
          address_lookup_fail := false;
          return (Or_error.error_string "failed to get host and port"))
        else return (Ok host_and_port))
  in
  let%bind rpc_conn = Persistent_connection.Rpc.connected persistent_conn in
  [%expect
    {|
    (Code (
      (pos_fname lib/persistent_connection/src/persistent_connection.ml)
      (pos_lnum     35)
      (library_name Persistent_connection)))
    ((time  "1970-01-01 00:00:00Z")
     (level Info)
     (raw_message (
       Structured (
         (label (String Attempting_to_connect))
         (tags (
           ((name event) (data (Sexp Attempting_to_connect)))
           ((name persistent_connection_to) (data (String "rpc test")))
           ((name created_at) (data (Sexp <hidden_in_test>))))))))
     (source <hidden_in_test>))
    ((time  "1970-01-01 00:00:00Z")
     (level Error)
     (raw_message (
       Structured (
         (label (String Failed_to_connect))
         (tags (
           ((name event)
            (data (Sexp (Failed_to_connect "failed to get host and port"))))
           ((name persistent_connection_to) (data (String "rpc test")))
           ((name created_at) (data (Sexp <hidden_in_test>))))))))
     (source <hidden_in_test>))
    ((time  "1970-01-01 00:00:00Z")
     (level Info)
     (raw_message (
       Structured (
         (label (String Obtained_address))
         (tags (
           ((name event) (data (Sexp (Obtained_address (localhost 1234)))))
           ((name persistent_connection_to) (data (String "rpc test")))
           ((name created_at) (data (Sexp <hidden_in_test>))))))))
     (source <hidden_in_test>))
    ((time  "1970-01-01 00:00:00Z")
     (level Info)
     (raw_message (
       Structured (
         (label (String Connected))
         (tags (
           ((name event) (data (Sexp (Connected <opaque>))))
           ((name persistent_connection_to) (data (String "rpc test")))
           ((name created_at) (data (Sexp <hidden_in_test>))))))))
     (source <hidden_in_test>))
    |}];
  let%bind () = Rpc.Rpc.dispatch_exn Hello.V1.rpc rpc_conn () in
  [%expect {| server says hi |}];
  let%bind () = Persistent_connection.Rpc.close persistent_conn in
  [%expect
    {|
    ((time  "1970-01-01 00:00:00Z")
     (level Info)
     (raw_message (
       Structured (
         (label (String Disconnected))
         (tags (
           ((name event)                    (data (Sexp   Disconnected)))
           ((name persistent_connection_to) (data (String "rpc test")))
           ((name created_at) (data (Sexp <hidden_in_test>))))))))
     (source <hidden_in_test>))
    |}];
  return ()
;;
