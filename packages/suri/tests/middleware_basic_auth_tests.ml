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

let test_basic_auth_accepts_case_insensitive_scheme = fun _ctx ->
  let encoded = Encoding.Base64.encode "alice:s3cret" in
  Test.assert_equal
    ~expected:(Some ("alice", "s3cret"))
    ~actual:(Basic_auth.For_testing.decode_credentials ("bAsIc " ^ encoded));
  Ok ()

let test_basic_auth_ignores_extra_spaces = fun _ctx ->
  let encoded = Encoding.Base64.encode "alice:s3cret" in
  Test.assert_equal
    ~expected:(Some ("alice", "s3cret"))
    ~actual:(Basic_auth.For_testing.decode_credentials ("  Basic   " ^ encoded ^ "  "));
  Ok ()

let test_basic_auth_preserves_colons_in_password = fun _ctx ->
  let encoded = Encoding.Base64.encode "alice:s3:cr:et" in
  Test.assert_equal
    ~expected:(Some ("alice", "s3:cr:et"))
    ~actual:(Basic_auth.For_testing.decode_credentials ("Basic " ^ encoded));
  Ok ()

let test_basic_auth_rejects_invalid_credentials = fun _ctx ->
  Test.assert_equal
    ~expected:None
    ~actual:(Basic_auth.For_testing.decode_credentials "Bearer token");
  Test.assert_equal
    ~expected:None
    ~actual:(Basic_auth.For_testing.decode_credentials "Basic not-base64");
  Ok ()

let test_basic_auth_sanitizes_realm_header_value = fun _ctx ->
  Test.assert_equal
    ~expected:"AdminPanel"
    ~actual:(Basic_auth.For_testing.sanitize_realm "Admin\r\n\"Panel");
  Ok ()

let tests =
  Test.[
    case
      "basic auth accepts case insensitive scheme"
      test_basic_auth_accepts_case_insensitive_scheme;
    case "basic auth ignores extra spaces" test_basic_auth_ignores_extra_spaces;
    case "basic auth preserves colons in password" test_basic_auth_preserves_colons_in_password;
    case "basic auth rejects invalid credentials" test_basic_auth_rejects_invalid_credentials;
    case "basic auth sanitizes realm header value" test_basic_auth_sanitizes_realm_header_value;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-basic-auth" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
