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

let test_csrf_generates_raw_hex_tokens = fun _ctx ->
  let token = Csrf.For_testing.generate_token () in
  Test.assert_equal ~expected:64 ~actual:(String.length token);
  Test.assert_true (Csrf.For_testing.is_raw_token token);
  Ok ()

let test_csrf_masking_roundtrips_and_uses_unique_masks = fun _ctx ->
  let token = Csrf.For_testing.generate_token () in
  let masked1 = Csrf.For_testing.mask_token token in
  let masked2 = Csrf.For_testing.mask_token token in
  Test.assert_false (String.equal masked1 masked2);
  Test.assert_equal ~expected:(Some token) ~actual:(Csrf.For_testing.unmask_token masked1);
  Test.assert_equal ~expected:(Some token) ~actual:(Csrf.For_testing.unmask_token masked2);
  Ok ()

let test_csrf_rejects_malformed_masked_tokens = fun _ctx ->
  Test.assert_equal ~expected:None ~actual:(Csrf.For_testing.unmask_token "not-base64");
  Test.assert_equal
    ~expected:None
    ~actual:(Csrf.For_testing.unmask_token (Encoding.Base64.encode "too-short"));
  Ok ()

let test_csrf_secure_equal_checks_full_token = fun _ctx ->
  let token = Csrf.For_testing.generate_token () in
  let last = String.get_unchecked token ~at:(String.length token - 1) in
  let replacement =
    if last = '0' then
      "1"
    else
      "0"
  in
  Test.assert_true (Csrf.For_testing.secure_equal token token);
  Test.assert_false (Csrf.For_testing.secure_equal token (String.sub token ~offset:0 ~len:63));
  Test.assert_false
    (Csrf.For_testing.secure_equal token (String.sub token ~offset:0 ~len:63 ^ replacement));
  Ok ()

let test_csrf_requires_session_middleware = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make
      ~method_:Net.Http.Method.Post
      ~body_params:[ ("_csrf_token", Csrf.For_testing.generate_token ()); ]
      ()
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
  Test.assert_equal ~expected:Csrf.For_testing.missing_session_body ~actual:response.body;
  Ok ()

let test_csrf_result_apis_return_structured_errors = fun _ctx ->
  (
    match Csrf.For_testing.generate_token_result () with
    | Ok token -> Test.assert_true (Csrf.For_testing.is_raw_token token)
    | Error error -> Test.assert_false (String.equal "" (Csrf.For_testing.error_to_string error))
  );
  (
    match Csrf.For_testing.mask_token_result "not-a-raw-token" with
    | Ok token -> Test.assert_equal ~expected:"not-a-raw-token" ~actual:token
    | Error error -> Test.assert_false (String.equal "" (Csrf.For_testing.error_to_string error))
  );
  let conn = Suri.Testing.Conn.make () in
  (
    match Csrf.For_testing.get_token_result conn with
    | Error Csrf.For_testing.MissingSession -> ()
    | Ok _ -> Test.assert_true false
    | Error error -> Test.assert_false (String.equal "" (Csrf.For_testing.error_to_string error))
  );
  (
    match Csrf.hidden_field_result conn with
    | Error Csrf.MissingSession -> ()
    | Ok _ -> Test.assert_true false
    | Error error -> Test.assert_false (String.equal "" (Csrf.error_to_string error))
  );
  let session =
    Session.For_testing.create ~cookie_name:"_test" ~secret:"0123456789abcdef0123456789abcdef" ()
  in
  (
    match Csrf.For_testing.get_or_create_token_result session with
    | Ok token ->
        Test.assert_true (Csrf.For_testing.is_raw_token token);
        Test.assert_equal ~expected:(Some token) ~actual:(Session.get_value "_csrf_token" session)
    | Error error -> Test.assert_false (String.equal "" (Csrf.For_testing.error_to_string error))
  );
  let rendered =
    Csrf.For_testing.error_to_string
      (Csrf.For_testing.TokenGenerationFailed (Csrf.For_testing.RandomByteFailed {
        index = 7;
        error = Random.InvalidIntBound { bound = 0 };
      }))
  in
  Test.assert_true (String.contains rendered "index 7");
  Test.assert_true (String.contains rendered "invalid int bound: 0");
  Ok ()

let tests =
  Test.[
    case "csrf generates raw hex tokens" test_csrf_generates_raw_hex_tokens;
    case
      "csrf masking roundtrips and uses unique masks"
      test_csrf_masking_roundtrips_and_uses_unique_masks;
    case "csrf rejects malformed masked tokens" test_csrf_rejects_malformed_masked_tokens;
    case "csrf secure equal checks full token" test_csrf_secure_equal_checks_full_token;
    case "csrf requires session middleware" test_csrf_requires_session_middleware;
    case "csrf result apis return structured errors" test_csrf_result_apis_return_structured_errors;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-csrf" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
