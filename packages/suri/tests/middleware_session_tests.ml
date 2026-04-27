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
module Connection = Suri.For_testing.Connection
module Handler = Suri.For_testing.Handler
module LiveViewSession = Suri.For_testing.LiveViewSession
module LiveViewProtocol = Suri.For_testing.LiveViewProtocol
module Channel = Suri.For_testing.Channel
module Http1 = Suri.For_testing.Http1

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
          ~fn:(fun req ((name, value)) ->
            Net.Http.Request.with_header req name value)
  in
  Suri.Request.of_http ~body:"" http_req

let http_request = fun
  ?(method_ = Net.Http.Method.Get)
  ?(version = Net.Http.Version.Http11)
  ?(headers = [])
  () ->
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
        ~fn:(fun req ((name, value)) ->
          Net.Http.Request.add_header req name value)

let config_for_test = fun
  ?(env = Config.default.env)
  ?(host = Config.default.host)
  ?(port = Config.default.port)
  ?(acceptors = Config.default.acceptors)
  ?(max_request_line_length = Config.default.max_request_line_length)
  ?(max_header_count = Config.default.max_header_count)
  ?(max_header_length = Config.default.max_header_length)
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

let test_session_middleware_installs_session = fun _ctx ->
  let conn = Conn.For_testing.make () in
  let found_session = ref false in
  let middleware = Session.middleware ~secret:"0123456789abcdef0123456789abcdef" () in
  let _conn' =
    middleware
      ~conn
      ~next:(fun conn ->
        found_session := Option.is_some (Session.find conn);
        conn)
  in
  Test.assert_true !found_session;
  Ok ()

let test_session_rejects_missing_secret = fun _ctx ->
  match Session.For_testing.validate_secret "   " with
  | Error Session.For_testing.Missing -> Ok ()
  | Ok () -> Error "expected missing session secret to fail"
  | Error error -> Error (Session.For_testing.secret_error_to_string error)

let test_session_rejects_short_secret = fun _ctx ->
  match Session.For_testing.validate_secret "short-secret" with
  | Error (Session.For_testing.TooShort 12) -> Ok ()
  | Ok () -> Error "expected short session secret to fail"
  | Error error -> Error (Session.For_testing.secret_error_to_string error)

let test_session_signing_uses_hmac = fun _ctx ->
  let secret = "0123456789abcdef0123456789abcdef" in
  let signature = Session.For_testing.sign ~secret "payload" in
  Test.assert_false (String.starts_with ~prefix:"0x" signature);
  Test.assert_true (Session.For_testing.verify ~secret "payload" signature);
  Test.assert_false (Session.For_testing.verify ~secret "tampered" signature);
  Ok ()

let test_session_cookie_roundtrips_and_rejects_tampering = fun _ctx ->
  let secret = "0123456789abcdef0123456789abcdef" in
  let session = Session.For_testing.create ~cookie_name:"_test" ~secret () in
  Session.put "user_id" "123" session;
  let cookie = Session.For_testing.to_cookie_value session in
  match Session.For_testing.from_cookie_value ~cookie_name:"_test" ~secret cookie with
  | Error err -> Error (Session.For_testing.decode_error_to_string err)
  | Ok decoded ->
      Test.assert_equal ~expected:(Some "123") ~actual:(Session.get_value "user_id" decoded);
      match Session.For_testing.from_cookie_value
        ~cookie_name:"_test"
        ~secret
        (tamper_last_char cookie) with
      | Error Session.For_testing.InvalidSignature -> Ok ()
      | Error err -> Error (Session.For_testing.decode_error_to_string err)
      | Ok _ -> Error "expected tampered session cookie to fail verification"

let test_session_cookie_decode_errors_are_structured = fun _ctx ->
  let secret = "0123456789abcdef0123456789abcdef" in
  let invalid_b64 = "not-base64!" in
  let invalid_b64_cookie = invalid_b64 ^ "." ^ Session.For_testing.sign ~secret invalid_b64 in
  let invalid_json_cookie = Session.For_testing.cookie_value_for_plaintext ~secret "{" in
  let invalid_session_data_cookie = Session.For_testing.cookie_value_for_plaintext ~secret "[]" in
  let checks = [
    (
      fun () ->
        match Session.For_testing.from_cookie_value ~cookie_name:"_test" ~secret "only-one-part" with
        | Error (Session.For_testing.InvalidCookieFormat { parts = 1 }) -> Ok ()
        | Ok _ -> Error "expected cookie format error"
        | Error err -> Error (Session.For_testing.decode_error_to_string err)
    );
    (
      fun () ->
        match Session.For_testing.from_cookie_value ~cookie_name:"_test" ~secret invalid_b64_cookie with
        | Error Session.For_testing.InvalidPayloadBase64 -> Ok ()
        | Ok _ -> Error "expected invalid base64 error"
        | Error err -> Error (Session.For_testing.decode_error_to_string err)
    );
    (
      fun () ->
        match Session.For_testing.from_cookie_value ~cookie_name:"_test" ~secret invalid_json_cookie with
        | Error (Session.For_testing.InvalidJson _) -> Ok ()
        | Ok _ -> Error "expected invalid JSON error"
        | Error err -> Error (Session.For_testing.decode_error_to_string err)
    );
    (
      fun () ->
        match Session.For_testing.from_cookie_value
          ~cookie_name:"_test"
          ~secret
          invalid_session_data_cookie with
        | Error (Session.For_testing.InvalidSessionData (Data.Json.Array [])) -> Ok ()
        | Ok _ -> Error "expected invalid session data error"
        | Error err -> Error (Session.For_testing.decode_error_to_string err)
    );
  ]
  in
  List.fold_left
    checks
    ~init:(Ok ())
    ~fn:(fun result check ->
      match result with
      | Error _ -> result
      | Ok () -> check ())

let tests =
  Test.[
    case "session middleware installs session" test_session_middleware_installs_session;
    case "session rejects missing secret" test_session_rejects_missing_secret;
    case "session rejects short secret" test_session_rejects_short_secret;
    case "session signing uses hmac" test_session_signing_uses_hmac;
    case
      "session cookie roundtrips and rejects tampering"
      test_session_cookie_roundtrips_and_rejects_tampering;
    case
      "session cookie decode errors are structured"
      test_session_cookie_decode_errors_are_structured;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-session" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
