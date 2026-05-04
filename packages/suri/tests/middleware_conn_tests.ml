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

let test_conn_query_params_handle_missing_and_blank_values = fun _ctx ->
  Test.assert_equal
    ~expected:[ ("flag", ""); ("empty", ""); ("name", "John Doe"); ]
    ~actual:(Conn.parse_query_params "flag&empty=&name=John+Doe");
  Ok ()

let test_conn_query_params_preserve_repeated_keys = fun _ctx ->
  Test.assert_equal
    ~expected:[ ("tag", "one"); ("tag", "two"); ("tag", "three"); ]
    ~actual:(Conn.parse_query_params "tag=one&tag=two&tag=three");
  Ok ()

let test_conn_query_params_decode_percent_and_skip_empty_pairs = fun _ctx ->
  Test.assert_equal
    ~expected:[ ("encoded", "&="); ("bad", "%ZZ"); ("incomplete", "%2"); ]
    ~actual:(Conn.parse_query_params "encoded=%26%3D&&bad=%ZZ&incomplete=%2&");
  Ok ()

let test_conn_to_response_returns_not_found_when_unsent = fun _ctx ->
  let response =
    Conn.to_response
      (
        Suri.Testing.Conn.make ()
        |> Result.unwrap
      )
  in
  Test.assert_equal ~expected:Net.Http.Status.NotFound ~actual:response.status;
  Test.assert_equal ~expected:"Not Found" ~actual:response.body;
  Ok ()

let test_conn_set_header_replaces_case_insensitively = fun _ctx ->
  let response =
    Suri.Testing.Conn.make ()
    |> Result.unwrap
    |> Conn.with_header "Vary" "Accept-Encoding"
    |> Conn.with_header "vary" "Origin"
    |> Conn.set_header "VARY" "Accept-Encoding, Origin"
    |> Conn.send
    |> Conn.to_response
  in
  Test.assert_equal
    ~expected:[ "Accept-Encoding, Origin"; ]
    ~actual:(Net.Http.Header.get_all response.headers "vary");
  Ok ()

let test_conn_assign_returns_updated_connection = fun _ctx ->
  let key: string Conn.assign_key = Conn.assign_key () in
  let conn =
    Suri.Testing.Conn.make ()
    |> Result.unwrap
  in
  let conn' = Conn.assign key "alice" conn in
  Test.assert_equal ~expected:None ~actual:(Conn.get_assign key conn);
  Test.assert_equal ~expected:(Some "alice") ~actual:(Conn.get_assign key conn');
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
    case
      "conn set header replaces case insensitively"
      test_conn_set_header_replaces_case_insensitively;
    case "conn assign returns updated connection" test_conn_assign_returns_updated_connection;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-conn" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
