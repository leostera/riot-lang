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

let test_cors_rejects_wildcard_origin_with_credentials = fun _ctx ->
  match Cors.middleware ~origins:[ "*" ] ~credentials:true () with
  | Error Cors.WildcardOriginWithCredentials -> Ok ()
  | Ok _ -> Error "expected wildcard credentials CORS config to be rejected"

let test_cors_reports_disallowed_origin = fun _ctx ->
  Test.assert_equal
    ~expected:(Error (Cors.OriginNotAllowed {
      origin = "https://evil.example";
      allowed = [ "https://example.com"; ];
    }))
    ~actual:(Cors.validate_origin ~origins:[ "https://example.com"; ] ~origin:"https://evil.example");
  Ok ()

let test_cors_middleware_renders_disallowed_origin = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ~headers:[ ("origin", "https://evil.example"); ] ()
    |> Result.unwrap
  in
  match Cors.middleware ~origins:[ "https://example.com"; ] () with
  | Error error -> Error (Cors.config_error_to_string error)
  | Ok middleware ->
      let response =
        middleware
          ~conn
          ~next:(fun conn ->
            conn
            |> Conn.respond ~status:Net.Http.Status.Ok ~body:"ok"
            |> Conn.send)
        |> Conn.to_response
      in
      Test.assert_equal ~expected:Net.Http.Status.Forbidden ~actual:response.status;
      Test.assert_equal
        ~expected:"CORS origin is not allowed: https://evil.example; allowed origins: https://example.com"
        ~actual:response.body;
      Ok ()

let test_cors_preflight_rejects_missing_method = fun _ctx ->
  match Cors.validate_preflight
    ~methods:[ Net.Http.Method.Put; ]
    ~headers:[]
    ~request_method:" "
    ~request_headers:None with
  | Error Cors.MissingRequestMethod -> Ok ()
  | Ok () -> Error "expected missing CORS preflight method"
  | Error error -> Error (Cors.preflight_error_to_string error)

let test_cors_preflight_rejects_disallowed_method = fun _ctx ->
  match Cors.validate_preflight
    ~methods:[ Net.Http.Method.Put; ]
    ~headers:[]
    ~request_method:"delete"
    ~request_headers:None with
  | Error (Cors.MethodNotAllowed method_) ->
      Test.assert_equal ~expected:"DELETE" ~actual:(Net.Http.Method.to_string method_);
      Ok ()
  | Ok () -> Error "expected disallowed CORS preflight method"
  | Error error -> Error (Cors.preflight_error_to_string error)

let test_cors_preflight_rejects_disallowed_headers = fun _ctx ->
  match Cors.validate_preflight
    ~methods:[ Net.Http.Method.Put; ]
    ~headers:[ "authorization"; ]
    ~request_method:"PUT"
    ~request_headers:(Some "Authorization, X-Evil") with
  | Error (Cors.HeadersNotAllowed { requested; allowed }) ->
      Test.assert_equal ~expected:[ "x-evil"; ] ~actual:requested;
      Test.assert_true (List.contains allowed ~value:"authorization");
      Test.assert_true (List.contains allowed ~value:"content-type");
      Ok ()
  | Ok () -> Error "expected disallowed CORS preflight headers"
  | Error error -> Error (Cors.preflight_error_to_string error)

let test_cors_preflight_allows_configured_headers = fun _ctx ->
  match Cors.validate_preflight
    ~methods:[ Net.Http.Method.Put; ]
    ~headers:[ "authorization"; "x-client"; ]
    ~request_method:"put"
    ~request_headers:(Some "Authorization, X-Client, Content-Type") with
  | Ok () -> Ok ()
  | Error error -> Error (Cors.preflight_error_to_string error)

let test_cors_preflight_returns_no_content = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make
      ~method_:Net.Http.Method.Options
      ~headers:[
        ("origin", "https://example.com");
        ("access-control-request-method", "PUT");
        ("access-control-request-headers", "Authorization");
      ]
      ()
    |> Result.unwrap
  in
  let continued = ref false in
  match Cors.middleware
    ~origins:[ "https://example.com"; ]
    ~methods:[ Net.Http.Method.Put; ]
    ~headers:[ "authorization"; ]
    () with
  | Error error -> Error (Cors.config_error_to_string error)
  | Ok middleware ->
      let conn' =
        middleware
          ~conn
          ~next:(fun conn ->
            continued := true;
            conn)
      in
      let response = Conn.to_response conn' in
      Test.assert_false !continued;
      Test.assert_equal ~expected:Net.Http.Status.NoContent ~actual:response.status;
      Ok ()

let test_cors_simple_request_merges_vary_origin = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ~headers:[ ("origin", "https://example.com"); ] ()
    |> Result.unwrap
  in
  match Cors.middleware ~origins:[ "https://example.com"; ] () with
  | Error error -> Error (Cors.config_error_to_string error)
  | Ok middleware ->
      let response =
        middleware
          ~conn
          ~next:(fun conn ->
            conn
            |> Conn.with_header "vary" "Accept-Encoding"
            |> Conn.respond ~status:Net.Http.Status.Ok ~body:"ok"
            |> Conn.send)
        |> Conn.to_response
      in
      Test.assert_equal
        ~expected:[ "Accept-Encoding, Origin"; ]
        ~actual:(Net.Http.Header.get_all response.headers "vary");
      Ok ()

let test_cors_preflight_merges_vary_origin = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make
      ~method_:Net.Http.Method.Options
      ~headers:[ ("origin", "https://example.com"); ("access-control-request-method", "PUT"); ]
      ()
    |> Result.unwrap
    |> Conn.with_header "vary" "Accept-Encoding"
  in
  match Cors.middleware ~origins:[ "https://example.com"; ] ~methods:[ Net.Http.Method.Put; ] () with
  | Error error -> Error (Cors.config_error_to_string error)
  | Ok middleware ->
      let response =
        middleware
          ~conn
          ~next:(fun conn ->
            conn
            |> Conn.respond ~status:Net.Http.Status.Ok ~body:"next"
            |> Conn.send)
        |> Conn.to_response
      in
      Test.assert_equal
        ~expected:[ "Accept-Encoding, Origin"; ]
        ~actual:(Net.Http.Header.get_all response.headers "vary");
      Ok ()

let tests =
  Test.[
    case
      "cors rejects wildcard origin with credentials"
      test_cors_rejects_wildcard_origin_with_credentials;
    case "cors reports disallowed origin" test_cors_reports_disallowed_origin;
    case "cors middleware renders disallowed origin" test_cors_middleware_renders_disallowed_origin;
    case "cors preflight rejects missing method" test_cors_preflight_rejects_missing_method;
    case "cors preflight rejects disallowed method" test_cors_preflight_rejects_disallowed_method;
    case "cors preflight rejects disallowed headers" test_cors_preflight_rejects_disallowed_headers;
    case "cors preflight allows configured headers" test_cors_preflight_allows_configured_headers;
    case "cors preflight returns no content" test_cors_preflight_returns_no_content;
    case "cors simple request merges vary origin" test_cors_simple_request_merges_vary_origin;
    case "cors preflight merges vary origin" test_cors_preflight_merges_vary_origin;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-cors" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
