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

let test_conn_query_params_handle_missing_and_blank_values = fun _ctx ->
  Test.assert_equal
    ~expected:[ ("flag", ""); ("empty", ""); ("name", "John Doe"); ]
    ~actual:(Conn.For_testing.parse_query_params "flag&empty=&name=John+Doe");
  Ok ()

let test_conn_query_params_preserve_repeated_keys = fun _ctx ->
  Test.assert_equal
    ~expected:[ ("tag", "one"); ("tag", "two"); ("tag", "three"); ]
    ~actual:(Conn.For_testing.parse_query_params "tag=one&tag=two&tag=three");
  Ok ()

let test_conn_query_params_decode_percent_and_skip_empty_pairs = fun _ctx ->
  Test.assert_equal
    ~expected:[ ("encoded", "&="); ("bad", "%ZZ"); ("incomplete", "%2"); ]
    ~actual:(Conn.For_testing.parse_query_params "encoded=%26%3D&&bad=%ZZ&incomplete=%2&");
  Ok ()

let test_conn_to_response_returns_not_found_when_unsent = fun _ctx ->
  let response = Conn.to_response (Conn.For_testing.make ()) in
  Test.assert_equal ~expected:Net.Http.Status.NotFound ~actual:response.status;
  Test.assert_equal ~expected:"Not Found" ~actual:response.body;
  Ok ()

let tests =
  Test.[
    case
      "conn query params handle missing and blank values"
      test_conn_query_params_handle_missing_and_blank_values;
    case "conn query params preserve repeated keys" test_conn_query_params_preserve_repeated_keys;
    case
      "conn query params decode percent and skip empty pairs"
      test_conn_query_params_decode_percent_and_skip_empty_pairs;
    case
      "conn to response returns not found when unsent"
      test_conn_to_response_returns_not_found_when_unsent;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-conn" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
