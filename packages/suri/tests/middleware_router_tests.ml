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

let test_router_matcher_ignores_empty_path_segments = fun _ctx ->
  Test.assert_equal
    ~expected:(Some [ ("id", "123"); ])
    ~actual:(Router.match_path "/users/:id" "//users/123/");
  Ok ()

let test_router_matcher_keeps_root_exact = fun _ctx ->
  Test.assert_equal ~expected:(Some []) ~actual:(Router.match_path "/" "/");
  Test.assert_equal ~expected:None ~actual:(Router.match_path "/" "/assets");
  Ok ()

let test_router_matcher_rejects_partial_literal_segments = fun _ctx ->
  Test.assert_equal ~expected:None ~actual:(Router.match_path "/assets" "/assets2");
  Test.assert_equal ~expected:(Some []) ~actual:(Router.match_path "/assets" "/assets/");
  Ok ()

let router_handler = fun body conn _req ->
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body
  |> Conn.send

let test_router_returns_method_not_allowed_for_known_path = fun _ctx ->
  let app = [
    Router.middleware
      [
        Router.post "/messages" (router_handler "post");
        Router.get "/messages" (router_handler "get");
      ];
  ]
  in
  match Testing.App.delete app "/messages" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_equal ~expected:Net.Http.Status.MethodNotAllowed ~actual:response.status;
      Test.assert_equal
        ~expected:(Some "GET, POST")
        ~actual:(Net.Http.Header.get response.headers "allow");
      Test.assert_equal ~expected:"Method Not Allowed" ~actual:response.body;
      Ok ()

let test_router_keeps_scanning_after_method_mismatch = fun _ctx ->
  let app = [
    Router.middleware
      [
        Router.post "/messages" (router_handler "post");
        Router.delete "/messages" (router_handler "delete");
      ];
  ]
  in
  match Testing.App.delete app "/messages" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_equal ~expected:Net.Http.Status.Ok ~actual:response.status;
      Test.assert_equal ~expected:"delete" ~actual:response.body;
      Test.assert_equal ~expected:None ~actual:(Net.Http.Header.get response.headers "allow");
      Ok ()

let test_router_passes_unmatched_paths_to_next_middleware = fun _ctx ->
  let app = [
    Router.middleware [ Router.get "/messages" (router_handler "get"); ];
    (
      fun ~conn ~next:_ ->
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:"fallback"
        |> Conn.send
    );
  ]
  in
  match Testing.App.get app "/missing" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_equal ~expected:Net.Http.Status.Ok ~actual:response.status;
      Test.assert_equal ~expected:"fallback" ~actual:response.body;
      Ok ()

let tests =
  Test.[
    case
      "router matcher ignores empty path segments"
      test_router_matcher_ignores_empty_path_segments;
    case "router matcher keeps root exact" test_router_matcher_keeps_root_exact;
    case
      "router matcher rejects partial literal segments"
      test_router_matcher_rejects_partial_literal_segments;
    case
      "router returns method not allowed for known path"
      test_router_returns_method_not_allowed_for_known_path;
    case
      "router keeps scanning after method mismatch"
      test_router_keeps_scanning_after_method_mismatch;
    case
      "router passes unmatched paths to next middleware"
      test_router_passes_unmatched_paths_to_next_middleware;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-router" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
