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

let test_session_middleware_installs_session = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ()
    |> Result.unwrap
  in
  let found_session = ref false in
  match Session.middleware ~secret:"0123456789abcdef0123456789abcdef" () with
  | Error error -> Error (Session.setup_error_to_string error)
  | Ok middleware ->
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
  match Session.validate_secret "   " with
  | Error Session.Missing -> Ok ()
  | Ok () -> Error "expected missing session secret to fail"
  | Error error -> Error (Session.secret_error_to_string error)

let test_session_rejects_short_secret = fun _ctx ->
  match Session.validate_secret "short-secret" with
  | Error (Session.TooShort 12) -> Ok ()
  | Ok () -> Error "expected short session secret to fail"
  | Error error -> Error (Session.secret_error_to_string error)

let test_session_rejects_invalid_cookie_name = fun _ctx ->
  match Session.middleware ~cookie_name:"bad name" ~secret:"0123456789abcdef0123456789abcdef" () with
  | Error (Session.InvalidCookieName (
    Session.InvalidCookieNameChar { char = ' '; index = 3 }
  )) ->
      Ok ()
  | Ok _ -> Error "expected invalid session cookie name to fail"
  | Error error -> Error (Session.setup_error_to_string error)

let test_session_rejects_invalid_max_age = fun _ctx ->
  match Session.middleware ~max_age:0 ~secret:"0123456789abcdef0123456789abcdef" () with
  | Error (Session.InvalidMaxAge 0) -> Ok ()
  | Ok _ -> Error "expected invalid session max_age to fail"
  | Error error -> Error (Session.setup_error_to_string error)

let test_session_rejects_samesite_none_without_secure = fun _ctx ->
  match Session.middleware
    ~same_site:Http.Http1.Cookie.None
    ~secure:false
    ~secret:"0123456789abcdef0123456789abcdef"
    () with
  | Error Session.SameSiteNoneRequiresSecure -> Ok ()
  | Ok _ -> Error "expected SameSite=None without Secure to fail"
  | Error error -> Error (Session.setup_error_to_string error)

let test_session_signing_uses_hmac = fun _ctx ->
  let secret = "0123456789abcdef0123456789abcdef" in
  let signature = Session.sign ~secret "payload" in
  Test.assert_false (String.starts_with ~prefix:"0x" signature);
  Test.assert_true (Session.verify ~secret "payload" signature);
  Test.assert_false (Session.verify ~secret "tampered" signature);
  Ok ()

let test_session_cookie_roundtrips_and_rejects_tampering = fun _ctx ->
  let secret = "0123456789abcdef0123456789abcdef" in
  match Session.create ~cookie_name:"_test" ~secret () with
  | Error error -> Error (Session.setup_error_to_string error)
  | Ok session ->
      Session.put "user_id" "123" session;
      let cookie = Session.to_cookie_value session in
      match Session.from_cookie_value ~cookie_name:"_test" ~secret cookie with
      | Error err -> Error (Session.decode_error_to_string err)
      | Ok decoded ->
          Test.assert_equal ~expected:(Some "123") ~actual:(Session.get_value "user_id" decoded);
          match Session.from_cookie_value ~cookie_name:"_test" ~secret (tamper_last_char cookie) with
          | Error Session.InvalidSignature -> Ok ()
          | Error err -> Error (Session.decode_error_to_string err)
          | Ok _ -> Error "expected tampered session cookie to fail verification"

let test_session_cookie_payload_is_signed_plaintext_json = fun _ctx ->
  let secret = "0123456789abcdef0123456789abcdef" in
  match Session.create ~cookie_name:"_test" ~secret () with
  | Error error -> Error (Session.setup_error_to_string error)
  | Ok session ->
      Session.put "user_id" "123" session;
      let cookie = Session.to_cookie_value session in
      match String.split_on_char '.' cookie with
      | [ payload; signature ] -> (
          Test.assert_true (Session.verify ~secret payload signature);
          match Encoding.Base64.decode payload with
          | Ok json ->
              Test.assert_true (String.starts_with ~prefix:"{" json);
              Test.assert_true (String.contains json "\"values\"");
              Test.assert_true (String.contains json "\"user_id\"");
              let tampered_cookie = Encoding.Base64.encode "{}" ^ "." ^ signature in
              (
                match Session.from_cookie_value ~cookie_name:"_test" ~secret tampered_cookie with
                | Error Session.InvalidSignature -> Ok ()
                | Error err -> Error (Session.decode_error_to_string err)
                | Ok _ -> Error "expected tampered session payload to fail verification"
              )
          | Error _ -> Error "expected session cookie payload to be valid base64"
        )
      | parts ->
          Error ("expected cookie payload and signature, got " ^ Int.to_string (List.length parts))

let test_session_cookie_decode_errors_are_structured = fun _ctx ->
  let secret = "0123456789abcdef0123456789abcdef" in
  let invalid_b64 = "not-base64!" in
  let invalid_b64_cookie = invalid_b64 ^ "." ^ Session.sign ~secret invalid_b64 in
  let invalid_json_cookie = Session.cookie_value_for_plaintext ~secret "{" in
  let invalid_session_data_cookie = Session.cookie_value_for_plaintext ~secret "[]" in
  let checks = [
    (
      fun () ->
        match Session.from_cookie_value ~cookie_name:"_test" ~secret "only-one-part" with
        | Error (Session.InvalidCookieFormat { parts = 1 }) -> Ok ()
        | Ok _ -> Error "expected cookie format error"
        | Error err -> Error (Session.decode_error_to_string err)
    );
    (
      fun () ->
        match Session.from_cookie_value ~cookie_name:"_test" ~secret invalid_b64_cookie with
        | Error Session.InvalidPayloadBase64 -> Ok ()
        | Ok _ -> Error "expected invalid base64 error"
        | Error err -> Error (Session.decode_error_to_string err)
    );
    (
      fun () ->
        match Session.from_cookie_value ~cookie_name:"_test" ~secret invalid_json_cookie with
        | Error (Session.InvalidJson _) -> Ok ()
        | Ok _ -> Error "expected invalid JSON error"
        | Error err -> Error (Session.decode_error_to_string err)
    );
    (
      fun () ->
        match Session.from_cookie_value ~cookie_name:"_test" ~secret invalid_session_data_cookie with
        | Error (Session.InvalidSessionData (Data.Json.Array [])) -> Ok ()
        | Ok _ -> Error "expected invalid session data error"
        | Error err -> Error (Session.decode_error_to_string err)
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
    case "session rejects invalid cookie name" test_session_rejects_invalid_cookie_name;
    case "session rejects invalid max age" test_session_rejects_invalid_max_age;
    case
      "session rejects SameSite None without Secure"
      test_session_rejects_samesite_none_without_secure;
    case "session signing uses hmac" test_session_signing_uses_hmac;
    case
      "session cookie roundtrips and rejects tampering"
      test_session_cookie_roundtrips_and_rejects_tampering;
    case
      "session cookie payload is signed plaintext JSON"
      test_session_cookie_payload_is_signed_plaintext_json;
    case
      "session cookie decode errors are structured"
      test_session_cookie_decode_errors_are_structured;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-session" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
