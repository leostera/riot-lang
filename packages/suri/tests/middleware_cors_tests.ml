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

let test_cors_rejects_wildcard_origin_with_credentials = fun _ctx ->
  try
    let _middleware = Cors.middleware ~origins:[ "*" ] ~credentials:true () in
    Error "expected wildcard credentials CORS config to be rejected"
  with
  | Cors.Invalid_config Cors.WildcardOriginWithCredentials -> Ok ()
  | _ -> Error "unexpected CORS config exception"

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
  in
  let continued = ref false in
  let middleware =
    Cors.middleware
      ~origins:[ "https://example.com"; ]
      ~methods:[ Net.Http.Method.Put; ]
      ~headers:[ "authorization"; ]
      ()
  in
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

let tests =
  Test.[
    case
      "cors rejects wildcard origin with credentials"
      test_cors_rejects_wildcard_origin_with_credentials;
    case "cors preflight rejects disallowed method" test_cors_preflight_rejects_disallowed_method;
    case "cors preflight rejects disallowed headers" test_cors_preflight_rejects_disallowed_headers;
    case "cors preflight allows configured headers" test_cors_preflight_allows_configured_headers;
    case "cors preflight returns no content" test_cors_preflight_returns_no_content;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-cors" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
