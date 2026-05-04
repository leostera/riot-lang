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
module Testing = Suri.Testing

let valid_websocket_key = "dGhlIHNhbXBsZSBub25jZQ=="

let websocket_request = fun
  ?(method_ = Net.Http.Method.Get)
  ?(version = Net.Http.Version.Http11)
  ?(headers = [("upgrade", "websocket"); ("connection", "keep-alive, Upgrade"); ("sec-websocket-version", "13"); ("sec-websocket-key", valid_websocket_key);])
  () ->
  let uri =
    Net.Uri.of_string "/"
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
    Net.Uri.of_string "/"
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

let test_request_id_accepts_valid_client_id = fun _ctx ->
  Test.assert_equal ~expected:(Ok ()) ~actual:(Request_id.validate_request_id "trace-123_ABC.~");
  Test.assert_true (Request_id.is_valid_request_id "trace-123_ABC.~");
  Test.assert_equal
    ~expected:"trace-123_ABC.~"
    ~actual:(Request_id.choose_request_id ~generate:(fun () -> "generated") (Some "trace-123_ABC.~"));
  Ok ()

let test_request_id_rejects_control_characters = fun _ctx ->
  match Request_id.validate_request_id "trace\r\nx-evil: yes" with
  | Error (Request_id.InvalidRequestIdCharacter { char = '\r'; index = 5 }) ->
      Test.assert_false (Request_id.is_valid_request_id "trace\r\nx-evil: yes");
      Test.assert_equal
        ~expected:"generated"
        ~actual:(Request_id.choose_request_id
          ~generate:(fun () -> "generated")
          (Some "trace\r\nx-evil: yes"));
      Ok ()
  | Ok () -> Error "expected control character to fail request id validation"
  | Error error -> Error (Request_id.validation_error_to_string error)

let test_request_id_rejects_whitespace = fun _ctx ->
  match Request_id.validate_request_id "trace id" with
  | Error (Request_id.InvalidRequestIdCharacter { char = ' '; index = 5 }) -> Ok ()
  | Ok () -> Error "expected whitespace to fail request id validation"
  | Error error -> Error (Request_id.validation_error_to_string error)

let test_request_id_rejects_empty_and_overlong_values = fun _ctx ->
  let too_long = String.make ~len:(Request_id.max_request_id_length + 1) ~char:'a' in
  match (Request_id.validate_request_id "", Request_id.validate_request_id too_long) with
  | (Error Request_id.EmptyRequestId, Error (Request_id.RequestIdTooLong { length; max_length })) ->
      Test.assert_equal ~expected:(Request_id.max_request_id_length + 1) ~actual:length;
      Test.assert_equal ~expected:Request_id.max_request_id_length ~actual:max_length;
      Test.assert_false (Request_id.is_valid_request_id "");
      Test.assert_false (Request_id.is_valid_request_id too_long);
      Test.assert_equal
        ~expected:"generated"
        ~actual:(Request_id.choose_request_id ~generate:(fun () -> "generated") None);
      Ok ()
  | _ -> Error "expected typed request id validation errors"

let test_request_id_regenerates_control_characters = fun _ctx ->
  Test.assert_false (Request_id.is_valid_request_id "trace\r\nx-evil: yes");
  Test.assert_equal
    ~expected:"generated"
    ~actual:(Request_id.choose_request_id
      ~generate:(fun () -> "generated")
      (Some "trace\r\nx-evil: yes"));
  Ok ()

let request_id_app = [
  Request_id.request_id;
  (
    fun ~conn ~next:_ ->
      let request_id =
        Conn.headers conn
        |> fun headers ->
          Net.Http.Header.get headers "x-request-id"
          |> Option.unwrap_or ~default:"missing"
      in
      conn
      |> Conn.respond ~status:Net.Http.Status.Ok ~body:request_id
      |> Conn.with_header "x-request-id" "handler-value"
      |> Conn.send
  );
]

let test_request_id_middleware_preserves_valid_client_id = fun _ctx ->
  match Testing.App.get request_id_app ~headers:[ ("x-request-id", "client-request-1"); ] "/" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_equal ~expected:"client-request-1" ~actual:response.body;
      Test.assert_equal
        ~expected:[ "client-request-1"; ]
        ~actual:(Net.Http.Header.get_all response.headers "x-request-id");
      Ok ()

let test_request_id_middleware_replaces_invalid_client_id_downstream = fun _ctx ->
  match Testing.App.get request_id_app ~headers:[ ("x-request-id", "bad\r\nx: yes"); ] "/" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_true (Request_id.is_valid_request_id response.body);
      Test.assert_false (String.equal response.body "bad\r\nx: yes");
      Test.assert_equal
        ~expected:[ response.body; ]
        ~actual:(Net.Http.Header.get_all response.headers "x-request-id");
      Ok ()

let test_request_id_middleware_generates_missing_id_downstream = fun _ctx ->
  match Testing.App.get request_id_app "/" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_true (Request_id.is_valid_request_id response.body);
      Test.assert_equal
        ~expected:[ response.body; ]
        ~actual:(Net.Http.Header.get_all response.headers "x-request-id");
      Ok ()

let tests =
  Test.[
    case "request id accepts valid client id" test_request_id_accepts_valid_client_id;
    case "request id rejects control characters" test_request_id_rejects_control_characters;
    case "request id rejects whitespace" test_request_id_rejects_whitespace;
    case
      "request id rejects empty and overlong values"
      test_request_id_rejects_empty_and_overlong_values;
    case "request id regenerates control characters" test_request_id_regenerates_control_characters;
    case
      "request id middleware preserves valid client id"
      test_request_id_middleware_preserves_valid_client_id;
    case
      "request id middleware replaces invalid client id downstream"
      test_request_id_middleware_replaces_invalid_client_id_downstream;
    case
      "request id middleware generates missing id downstream"
      test_request_id_middleware_generates_missing_id_downstream;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-request-id" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
