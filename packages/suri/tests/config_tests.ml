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

let test_config_validates_default_development = fun _ctx ->
  match Config.validate Config.default with
  | Ok _ -> Ok ()
  | Error errors -> Error (Config.errors_to_string errors)

let test_config_parses_env_aliases = fun _ctx ->
  Test.assert_equal ~expected:(Ok Config.Development) ~actual:(Config.env_from_string "dev");
  Test.assert_equal ~expected:(Ok Config.Production) ~actual:(Config.env_from_string "prod");
  Test.assert_equal ~expected:(Ok Config.Test) ~actual:(Config.env_from_string "TEST");
  Ok ()

let test_config_rejects_invalid_env_with_allowed_values = fun _ctx ->
  match Config.env_from_string " staging " with
  | Error (Config.InvalidEnv { value; normalized; allowed }) ->
      Test.assert_equal ~expected:" staging " ~actual:value;
      Test.assert_equal ~expected:"staging" ~actual:normalized;
      Test.assert_equal
        ~expected:[ Config.Development; Config.Test; Config.Production; ]
        ~actual:allowed;
      Ok ()
  | Ok _ -> Error "expected invalid config env to fail"
  | Error error -> Error (Config.error_to_string error)

let test_config_rejects_production_placeholder_secret = fun _ctx ->
  match Config.validate (config_for_test ~env:Config.Production ()) with
  | Error errors ->
      Test.assert_true
        (List.contains errors ~value:(Config.InvalidLiveViewSecret Config.Placeholder));
      Ok ()
  | Ok _ -> Error "expected production placeholder liveview secret to be rejected"

let test_config_rejects_invalid_limits = fun _ctx ->
  let config =
    config_for_test
      ~port:0
      ~acceptors:0
      ~max_request_line_length:0
      ~max_header_count:0
      ~max_header_length:0
      ~max_body_size:0
      ~max_keep_alive_requests:0
      ~max_websocket_frame_size:0
      ~max_websocket_message_size:0
      ~read_header_timeout_ms:0
      ~read_body_timeout_ms:0
      ~idle_timeout_ms:0
      ~write_timeout_ms:0
      ~buffer_size:0
      ~liveview_secret:"short"
      ()
  in
  match Config.validate config with
  | Error errors ->
      Test.assert_true (List.contains errors ~value:(Config.InvalidPort 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidAcceptors 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidMaxRequestLineLength 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidMaxHeaderCount 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidMaxHeaderLength 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidMaxBodySize 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidMaxKeepAliveRequests 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidMaxWebSocketFrameSize 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidMaxWebSocketMessageSize 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidReadHeaderTimeoutMs 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidReadBodyTimeoutMs 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidIdleTimeoutMs 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidWriteTimeoutMs 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidBufferSize 0));
      Test.assert_true
        (List.contains errors ~value:(Config.InvalidLiveViewSecret (Config.TooShort 5)));
      Ok ()
  | Ok _ -> Error "expected invalid config limits to be rejected"

let test_start_link_returns_invalid_config = fun _ctx ->
  let config = config_for_test ~port:0 () in
  match Suri.start_link ~config [] with
  | Error (Suri.InvalidConfig errors) ->
      Test.assert_true (List.contains errors ~value:(Config.InvalidPort 0));
      Ok ()
  | Error _ -> Error "expected Suri.start_link to return InvalidConfig"
  | Ok _ -> Error "expected Suri.start_link with invalid config to fail"

let test_suri_config_returns_validation_errors = fun _ctx ->
  match Suri.config ~port:0 () with
  | Error errors ->
      Test.assert_true (List.contains errors ~value:(Config.InvalidPort 0));
      Ok ()
  | Ok _ -> Error "expected Suri.config with invalid port to return errors"

let tests =
  Test.[
    case "config validates default development" test_config_validates_default_development;
    case "config parses env aliases" test_config_parses_env_aliases;
    case
      "config rejects invalid env with allowed values"
      test_config_rejects_invalid_env_with_allowed_values;
    case
      "config rejects production placeholder secret"
      test_config_rejects_production_placeholder_secret;
    case "config rejects invalid limits" test_config_rejects_invalid_limits;
    case "start link returns invalid config" test_start_link_returns_invalid_config;
    case "suri config returns validation errors" test_suri_config_returns_validation_errors;
  ]

let main ~args = Test.Cli.main ~name:"suri:config" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
