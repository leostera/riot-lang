open Std

module Component = Suri.Component
module Accepts = Suri.Middleware.Accepts
module Basic_auth = Suri.Middleware.Basic_auth
module Body_parser = Suri.Middleware.Body_parser
module Config = Suri.Config
module Conn = Suri.Middleware.Conn
module Cors = Suri.Middleware.Cors
module Csrf = Suri.Middleware.Csrf
module Logger = Suri.Middleware.Logger
module Remote_ip = Suri.Middleware.Remote_ip
module Request_id = Suri.Middleware.Request_id
module Router = Suri.Middleware.Router
module Session = Suri.Middleware.Session
module Static = Suri.Middleware.Static
module Response = Suri.Response
module Connection = Suri.Testing.Internal.Connection
module Handler = Suri.Testing.Internal.Handler
module LiveViewSession = Suri.Testing.Internal.LiveViewSession
module LiveViewProtocol = Suri.Testing.Internal.LiveViewProtocol
module Channel = Suri.Testing.Internal.Channel
module Http1 = Suri.Testing.Internal.Http1

let valid_websocket_key = "dGhlIHNhbXBsZSBub25jZQ=="

let websocket_request = fun
  ?(method_ = Net.Http.Method.Get)
  ?(version = Net.Http.Version.Http11)
  ?(headers = [("upgrade", "websocket"); ("connection", "keep-alive, Upgrade"); ("sec-websocket-version", "13"); ("sec-websocket-key", valid_websocket_key);])
  () ->
  let uri =
    Net.Uri.from_string "/"
    |> Result.unwrap
  in
  let http_req =
    Net.Http.Request.create method_ uri
    |> fun req ->
      Net.Http.Request.with_version req version
      |> fun req ->
        List.fold_left
          headers
          ~init:req
          ~fn:(fun req (name, value) ->
            Net.Http.Request.with_header req name value)
  in
  Suri.Request.from_http ~body:"" http_req

let http_request = fun
  ?(method_ = Net.Http.Method.Get) ?(version = Net.Http.Version.Http11) ?(headers = []) () ->
  let uri =
    Net.Uri.from_string "/"
    |> Result.unwrap
  in
  Net.Http.Request.create method_ uri
  |> fun req ->
    Net.Http.Request.with_version req version
    |> fun req ->
      List.fold_left
        headers
        ~init:req
        ~fn:(fun req (name, value) ->
          Net.Http.Request.add_header req name value)

let config_for_test = fun
  ?(env = Config.default.env)
  ?(host = Config.default.host)
  ?(port = Config.default.port)
  ?(acceptors = Config.default.acceptors)
  ?(max_request_line_length = Config.default.max_request_line_length)
  ?(max_header_count = Config.default.max_header_count)
  ?(max_header_length = Config.default.max_header_length)
  ?(max_body_size = Config.default.max_body_size)
  ?(max_keep_alive_requests = Config.default.max_keep_alive_requests)
  ?(max_websocket_frame_size = Config.default.max_websocket_frame_size)
  ?(max_websocket_message_size = Config.default.max_websocket_message_size)
  ?(read_header_timeout_ms = Config.default.read_header_timeout_ms)
  ?(read_body_timeout_ms = Config.default.read_body_timeout_ms)
  ?(idle_timeout_ms = Config.default.idle_timeout_ms)
  ?(write_timeout_ms = Config.default.write_timeout_ms)
  ?(buffer_size = Config.default.buffer_size)
  ?(liveview_secret = Config.default.liveview_secret)
  () ->
  Config.{
    env;
    host;
    port;
    acceptors;
    max_request_line_length;
    max_header_count;
    max_header_length;
    max_body_size;
    max_keep_alive_requests;
    max_websocket_frame_size;
    max_websocket_message_size;
    read_header_timeout_ms;
    read_body_timeout_ms;
    idle_timeout_ms;
    write_timeout_ms;
    buffer_size;
    liveview_secret;
  }

let tamper_last_char = fun value ->
  let len = String.length value in
  let prefix = String.sub value ~offset:0 ~len:(len - 1) in
  let last = String.get_unchecked value ~at:(len - 1) in
  let replacement =
    if last = 'A' then
      "B"
    else
      "A"
  in
  prefix ^ replacement

let test_liveview_session_signing_uses_hmac = fun _ctx ->
  let secret = "0123456789abcdef0123456789abcdef" in
  let signature = LiveViewSession.sign ~secret ~data:"payload" in
  Test.assert_true (LiveViewSession.verify ~secret ~data:"payload" ~signature);
  Test.assert_false (LiveViewSession.verify ~secret ~data:"tampered" ~signature);
  Ok ()

let test_liveview_session_token_rejects_tampering = fun _ctx ->
  let secret = "0123456789abcdef0123456789abcdef" in
  let token =
    LiveViewSession.encode ~secret ~json:(Data.Json.obj [ ("id", Data.Json.string "counter"); ])
  in
  match LiveViewSession.decode ~secret ~token with
  | Error err -> Error (LiveViewSession.decode_error_to_string err)
  | Ok json ->
      let actual =
        match Data.Json.get_field "id" json with
        | Some value -> Data.Json.get_string value
        | None -> None
      in
      Test.assert_equal ~expected:(Some "counter") ~actual;
      match LiveViewSession.decode ~secret ~token:(tamper_last_char token) with
      | Error _ -> Ok ()
      | Ok _ -> Error "expected tampered liveview token to fail verification"

let test_liveview_session_token_returns_structured_errors = fun _ctx ->
  let secret = "0123456789abcdef0123456789abcdef" in
  match LiveViewSession.decode ~secret ~token:"not-a-token" with
  | Error LiveViewSession.InvalidTokenFormat -> Ok ()
  | Ok _ -> Error "expected invalid LiveView token format to fail"
  | Error error -> Error (LiveViewSession.decode_error_to_string error)

let test_liveview_protocol_decodes_event_messages = fun _ctx ->
  let payload =
    Data.Json.obj
      [ ("Event", Data.Json.array [ Data.Json.string "handler-1"; Data.Json.string "clicked"; ]); ]
    |> Data.Json.to_string
  in
  match LiveViewProtocol.deserialize_client_msg payload with
  | Ok (LiveViewProtocol.Event { handler_id; event_data }) ->
      Test.assert_equal ~expected:"handler-1" ~actual:handler_id;
      Test.assert_equal ~expected:"clicked" ~actual:event_data;
      Ok ()
  | Ok LiveViewProtocol.Mount -> Error "expected LiveView event message"
  | Error error -> Error (LiveViewProtocol.client_msg_error_to_string error)

let test_liveview_protocol_returns_structured_json_errors = fun _ctx ->
  match LiveViewProtocol.deserialize_client_msg "{" with
  | Error (LiveViewProtocol.InvalidJson _) -> Ok ()
  | Ok _ -> Error "expected invalid LiveView JSON to fail"
  | Error error -> Error (LiveViewProtocol.client_msg_error_to_string error)

let test_liveview_protocol_returns_structured_message_errors = fun _ctx ->
  let payload = Data.Json.obj [ ("Unknown", Data.Json.string "message"); ] in
  match LiveViewProtocol.deserialize_client_msg (Data.Json.to_string payload) with
  | Error (LiveViewProtocol.UnknownMessageFormat json) ->
      Test.assert_equal ~expected:(Data.Json.to_string payload) ~actual:(Data.Json.to_string json);
      Ok ()
  | Ok _ -> Error "expected unknown LiveView message format to fail"
  | Error error -> Error (LiveViewProtocol.client_msg_error_to_string error)

let test_liveview_protocol_returns_structured_event_payload_errors = fun _ctx ->
  let payload = Data.Json.obj [ ("Event", Data.Json.array [ Data.Json.string "handler-1"; ]); ] in
  match LiveViewProtocol.deserialize_client_msg (Data.Json.to_string payload) with
  | Error (LiveViewProtocol.InvalidEventPayload json) ->
      Test.assert_equal
        ~expected:(Data.Json.to_string (Data.Json.array [ Data.Json.string "handler-1"; ]))
        ~actual:(Data.Json.to_string json);
      Ok ()
  | Ok _ -> Error "expected invalid LiveView event payload to fail"
  | Error error -> Error (LiveViewProtocol.client_msg_error_to_string error)

let test_liveview_protocol_serializes_server_errors_structurally = fun _ctx ->
  let message =
    LiveViewProtocol.Error (LiveViewProtocol.ClientMessageDecodeFailed (LiveViewProtocol.InvalidJson (Data.Json.Unexpected_end_of_input {
      expected = "value";
    })))
    |> LiveViewProtocol.serialize_server_msg
  in
  match Data.Json.from_string message with
  | Error error -> Error (Data.Json.error_to_string error)
  | Ok json ->
      let error_type =
        Data.Json.get_field "Error" json
        |> Option.and_then ~fn:(Data.Json.get_field "type")
        |> Option.and_then ~fn:Data.Json.get_string
      in
      let client_error_type =
        Data.Json.get_field "Error" json
        |> Option.and_then ~fn:(Data.Json.get_field "error")
        |> Option.and_then ~fn:(Data.Json.get_field "type")
        |> Option.and_then ~fn:Data.Json.get_string
      in
      let json_error_type =
        Data.Json.get_field "Error" json
        |> Option.and_then ~fn:(Data.Json.get_field "error")
        |> Option.and_then ~fn:(Data.Json.get_field "error")
        |> Option.and_then ~fn:(Data.Json.get_field "type")
        |> Option.and_then ~fn:Data.Json.get_string
      in
      Test.assert_equal ~expected:(Some "ClientMessageDecodeFailed") ~actual:error_type;
      Test.assert_equal ~expected:(Some "InvalidJson") ~actual:client_error_type;
      Test.assert_equal ~expected:(Some "UnexpectedEndOfInput") ~actual:json_error_type;
      Ok ()

let test_liveview_protocol_serializes_event_payload_errors = fun _ctx ->
  let payload = Data.Json.obj [ ("Event", Data.Json.array [ Data.Json.string "handler-1"; ]); ] in
  let message =
    LiveViewProtocol.Error (LiveViewProtocol.ClientMessageDecodeFailed (LiveViewProtocol.InvalidEventPayload payload))
    |> LiveViewProtocol.serialize_server_msg
  in
  match Data.Json.from_string message with
  | Error error -> Error (Data.Json.error_to_string error)
  | Ok json ->
      let client_error_type =
        Data.Json.get_field "Error" json
        |> Option.and_then ~fn:(Data.Json.get_field "error")
        |> Option.and_then ~fn:(Data.Json.get_field "type")
        |> Option.and_then ~fn:Data.Json.get_string
      in
      let serialized_payload =
        Data.Json.get_field "Error" json
        |> Option.and_then ~fn:(Data.Json.get_field "error")
        |> Option.and_then ~fn:(Data.Json.get_field "message")
      in
      Test.assert_equal ~expected:(Some "InvalidEventPayload") ~actual:client_error_type;
      Test.assert_equal
        ~expected:(Some (Data.Json.to_string payload))
        ~actual:(Option.map serialized_payload ~fn:Data.Json.to_string);
      Ok ()

module TestLiveViewComponent = struct
  let id = "test-liveview"

  type state = unit

  type msg = unit

  type args = unit

  let serialize_args () = Data.Json.Null

  let deserialize_args = fun __tmp1 ->
    match __tmp1 with
    | Data.Json.Null -> Ok ()
    | json -> Error json

  let init _conn () = ()

  let update _msg state = state

  let render ~state () = Component.text "ok"
end

let liveview_session_uri = fun json ->
  let token = LiveViewSession.encode ~secret:Config.default.liveview_secret ~json in
  "/?session=" ^ Net.Uri.form_encode token

let test_liveview_initializes_with_valid_session_token = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ~uri:(liveview_session_uri Data.Json.Null) ()
    |> Result.unwrap
  in
  let (_opts, handler) = Suri.LiveView.mount (module TestLiveViewComponent) conn in
  match Channel.initialize handler with
  | Channel.Continue _ -> Ok ()
  | Channel.Error reported -> Error (Channel.reported_error_to_string reported)
  | Channel.Push _ -> Error "expected LiveView valid session token to initialize"

let test_liveview_rejects_missing_session_tokens = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ~uri:"/" ()
    |> Result.unwrap
  in
  let (_opts, handler) = Suri.LiveView.mount (module TestLiveViewComponent) conn in
  match Channel.initialize handler with
  | Channel.Error reported ->
      match Channel.reported_error reported with
      | Channel.InitializationFailed _ ->
          Test.assert_true
            (String.contains
              (Channel.reported_error_to_string reported)
              "LiveView session token is required");
          Ok ()
      | Channel.UnknownOpcode _ -> Error "expected LiveView initialization failure"
  | Channel.Continue _ ->
      Error "expected missing LiveView session token to reject handler initialization"
  | Channel.Push _ ->
      Error "expected missing LiveView session token to reject handler initialization"

let test_liveview_rejects_invalid_session_tokens = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ~uri:"/?session=not-a-token" ()
    |> Result.unwrap
  in
  let (_opts, handler) = Suri.LiveView.mount (module TestLiveViewComponent) conn in
  match Channel.initialize handler with
  | Channel.Error reported ->
      match Channel.reported_error reported with
      | Channel.InitializationFailed _ ->
          Test.assert_true
            (String.contains
              (Channel.reported_error_to_string reported)
              "Invalid LiveView session token");
          Ok ()
      | Channel.UnknownOpcode _ -> Error "expected LiveView initialization failure"
  | Channel.Continue _ -> Error "expected invalid LiveView token to reject handler initialization"
  | Channel.Push _ -> Error "expected invalid LiveView token to reject handler initialization"

let tests =
  Test.[
    case "liveview session signing uses hmac" test_liveview_session_signing_uses_hmac;
    case "liveview session token rejects tampering" test_liveview_session_token_rejects_tampering;
    case
      "liveview session token returns structured errors"
      test_liveview_session_token_returns_structured_errors;
    case "liveview protocol decodes event messages" test_liveview_protocol_decodes_event_messages;
    case
      "liveview protocol returns structured json errors"
      test_liveview_protocol_returns_structured_json_errors;
    case
      "liveview protocol returns structured message errors"
      test_liveview_protocol_returns_structured_message_errors;
    case
      "liveview protocol returns structured event payload errors"
      test_liveview_protocol_returns_structured_event_payload_errors;
    case
      "liveview protocol serializes server errors structurally"
      test_liveview_protocol_serializes_server_errors_structurally;
    case
      "liveview protocol serializes event payload errors"
      test_liveview_protocol_serializes_event_payload_errors;
    case
      "liveview initializes with valid session token"
      test_liveview_initializes_with_valid_session_token;
    case "liveview rejects missing session tokens" test_liveview_rejects_missing_session_tokens;
    case "liveview rejects invalid session tokens" test_liveview_rejects_invalid_session_tokens;
  ]

let main ~args = Test.Cli.main ~name:"suri:liveview" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
