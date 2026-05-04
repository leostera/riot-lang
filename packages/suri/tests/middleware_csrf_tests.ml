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

let test_csrf_generates_raw_hex_tokens = fun _ctx ->
  match Csrf.generate_token () with
  | Error error -> Error (Csrf.error_to_string error)
  | Ok token ->
      Test.assert_equal ~expected:64 ~actual:(String.length token);
      Test.assert_true (Csrf.is_raw_token token);
      Ok ()

let test_csrf_masking_roundtrips_and_uses_unique_masks = fun _ctx ->
  match Csrf.generate_token () with
  | Error error -> Error (Csrf.error_to_string error)
  | Ok token -> (
      match (Csrf.mask_token token, Csrf.mask_token token) with
      | (Ok masked1, Ok masked2) ->
          Test.assert_false (String.equal masked1 masked2);
          Test.assert_equal ~expected:(Ok token) ~actual:(Csrf.unmask_token masked1);
          Test.assert_equal ~expected:(Ok token) ~actual:(Csrf.unmask_token masked2);
          Ok ()
      | (Error error, _)
      | (_, Error error) -> Error (Csrf.error_to_string error)
    )

let test_csrf_rejects_malformed_masked_tokens = fun _ctx ->
  (
    match Csrf.unmask_token "not-base64" with
    | Error Csrf.InvalidMaskedTokenEncoding -> ()
    | Ok _ -> Test.assert_true false
    | Error _ -> Test.assert_true false
  );
  (
    match Csrf.unmask_token (Encoding.Base64.encode "too-short") with
    | Error Csrf.InvalidMaskedTokenLength { expected = 64; actual } ->
        Test.assert_equal ~expected:(String.length "too-short") ~actual
    | Ok _ -> Test.assert_true false
    | Error _ -> Test.assert_true false
  );
  Ok ()

let test_csrf_secure_equal_checks_full_token = fun _ctx ->
  match Csrf.generate_token () with
  | Error error -> Error (Csrf.error_to_string error)
  | Ok token ->
      let last = String.get_unchecked token ~at:(String.length token - 1) in
      let replacement =
        if last = '0' then
          "1"
        else
          "0"
      in
      Test.assert_true (Csrf.secure_equal token token);
      Test.assert_false (Csrf.secure_equal token (String.sub token ~offset:0 ~len:63));
      Test.assert_false (Csrf.secure_equal token (String.sub token ~offset:0 ~len:63 ^ replacement));
      Ok ()

let test_csrf_requires_session_middleware = fun _ctx ->
  match Csrf.generate_token () with
  | Error error -> Error (Csrf.error_to_string error)
  | Ok token ->
      let conn =
        Suri.Testing.Conn.make
          ~method_:Net.Http.Method.Post
          ~body_params:[ ("_csrf_token", token); ]
          ()
        |> Result.unwrap
      in
      let continued = ref false in
      let middleware = Csrf.middleware () in
      let conn' =
        middleware
          ~conn
          ~next:(fun conn ->
            continued := true;
            conn)
      in
      let response = Conn.to_response conn' in
      Test.assert_false !continued;
      Test.assert_equal ~expected:Net.Http.Status.InternalServerError ~actual:response.status;
      Test.assert_equal ~expected:Csrf.missing_session_body ~actual:response.body;
      Ok ()

let test_csrf_plain_apis_return_structured_errors = fun _ctx ->
  (
    match Csrf.generate_token () with
    | Ok token -> Test.assert_true (Csrf.is_raw_token token)
    | Error error -> Test.assert_false (String.equal "" (Csrf.error_to_string error))
  );
  (
    match Csrf.mask_token "not-a-raw-token" with
    | Ok token -> Test.assert_equal ~expected:"not-a-raw-token" ~actual:token
    | Error error -> Test.assert_false (String.equal "" (Csrf.error_to_string error))
  );
  let conn =
    Suri.Testing.Conn.make ()
    |> Result.unwrap
  in
  (
    match Csrf.get_token conn with
    | Error Csrf.MissingSession -> ()
    | Ok _ -> Test.assert_true false
    | Error error -> Test.assert_false (String.equal "" (Csrf.error_to_string error))
  );
  (
    match Csrf.hidden_field conn with
    | Error Csrf.MissingSession -> ()
    | Ok _ -> Test.assert_true false
    | Error error -> Test.assert_false (String.equal "" (Csrf.error_to_string error))
  );
  let session_check =
    match Session.create ~cookie_name:"_test" ~secret:"0123456789abcdef0123456789abcdef" () with
    | Error error -> Error (Session.setup_error_to_string error)
    | Ok session -> (
        match Csrf.get_or_create_token session with
        | Ok token ->
            Test.assert_true (Csrf.is_raw_token token);
            Test.assert_equal
              ~expected:(Some token)
              ~actual:(Session.get_value "_csrf_token" session);
            Ok ()
        | Error error -> Error (Csrf.error_to_string error)
      )
  in
  match session_check with
  | Error error -> Error error
  | Ok () ->
      let rendered =
        Csrf.error_to_string
          (Csrf.TokenGenerationFailed (Csrf.RandomByteFailed {
            index = 7;
            error = Random.InvalidIntBound { bound = 0 };
          }))
      in
      Test.assert_true (String.contains rendered "index 7");
      Test.assert_true (String.contains rendered "invalid int bound: 0");
      Ok ()

let test_csrf_verify_token_reports_structured_errors = fun _ctx ->
  let token = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" in
  let other_token = "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" in
  match Session.create ~cookie_name:"_test" ~secret:"0123456789abcdef0123456789abcdef" () with
  | Error error -> Error (Session.setup_error_to_string error)
  | Ok session -> (
      Session.put "_csrf_token" token session;
      Test.assert_equal ~expected:(Ok ()) ~actual:(Csrf.verify_token_result session token);
      Test.assert_true (Csrf.verify_token session token);
      Test.assert_equal
        ~expected:(Error Csrf.TokenMismatch)
        ~actual:(Csrf.verify_token_result session other_token);
      Test.assert_false (Csrf.verify_token session other_token);
      Test.assert_equal
        ~expected:(Error (Csrf.InvalidRequestToken Csrf.InvalidMaskedTokenEncoding))
        ~actual:(Csrf.verify_token_result session "not-base64");
      match Csrf.mask_token token with
      | Error error -> Error (Csrf.error_to_string error)
      | Ok masked -> (
          Test.assert_equal ~expected:(Ok ()) ~actual:(Csrf.verify_token_result session masked);
          match Session.create ~cookie_name:"_missing" ~secret:"0123456789abcdef0123456789abcdef" () with
          | Error error -> Error (Session.setup_error_to_string error)
          | Ok missing_session -> (
              Test.assert_equal
                ~expected:(Error Csrf.MissingStoredToken)
                ~actual:(Csrf.verify_token_result missing_session token);
              match Session.create
                ~cookie_name:"_invalid"
                ~secret:"0123456789abcdef0123456789abcdef"
                () with
              | Error error -> Error (Session.setup_error_to_string error)
              | Ok invalid_session ->
                  Session.put "_csrf_token" "not-a-raw-token" invalid_session;
                  Test.assert_equal
                    ~expected:(Error Csrf.InvalidStoredToken)
                    ~actual:(Csrf.verify_token_result invalid_session token);
                  Ok ()
            )
        )
    )

let tests =
  Test.[
    case "csrf generates raw hex tokens" test_csrf_generates_raw_hex_tokens;
    case
      "csrf masking roundtrips and uses unique masks"
      test_csrf_masking_roundtrips_and_uses_unique_masks;
    case "csrf rejects malformed masked tokens" test_csrf_rejects_malformed_masked_tokens;
    case "csrf secure equal checks full token" test_csrf_secure_equal_checks_full_token;
    case "csrf requires session middleware" test_csrf_requires_session_middleware;
    case "csrf plain apis return structured errors" test_csrf_plain_apis_return_structured_errors;
    case
      "csrf verify token reports structured errors"
      test_csrf_verify_token_reports_structured_errors;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-csrf" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
