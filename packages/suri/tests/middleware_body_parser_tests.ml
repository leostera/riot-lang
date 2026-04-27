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

let test_body_parser_rejects_oversized_bodies = fun _ctx ->
  let config = { Body_parser.parsers = [ Body_parser.Json ]; max_body_size = 2 } in
  match Body_parser.For_testing.parse_body config ~content_type:"application/json" ~body:"{} " with
  | Error (Body_parser.BodyTooLarge { size; max_size }) ->
      Test.assert_equal ~expected:3 ~actual:size;
      Test.assert_equal ~expected:2 ~actual:max_size;
      Ok ()
  | Ok _ -> Error "expected oversized body to fail"
  | Error error -> Error (Body_parser.parse_error_to_string error)

let test_body_parser_rejects_invalid_json = fun _ctx ->
  match Body_parser.For_testing.parse_body
    (Body_parser.default_config ())
    ~content_type:"application/json"
    ~body:{|{"name":|} with
  | Error (Body_parser.InvalidJson _) -> Ok ()
  | Ok _ -> Error "expected invalid JSON to fail"
  | Error error -> Error (Body_parser.parse_error_to_string error)

let test_body_parser_rejects_json_root_arrays = fun _ctx ->
  match Body_parser.For_testing.parse_body
    (Body_parser.default_config ())
    ~content_type:"application/json"
    ~body:{|["alice"]|} with
  | Error (Body_parser.JsonRootNotObject "array") -> Ok ()
  | Ok _ -> Error "expected JSON array body to fail"
  | Error error -> Error (Body_parser.parse_error_to_string error)

let test_body_parser_accepts_case_insensitive_json_content_type = fun _ctx ->
  Test.assert_equal
    ~expected:[ ("name", "Alice"); ("active", "true"); ]
    ~actual:(
      Body_parser.For_testing.parse_body
        (Body_parser.default_config ())
        ~content_type:"Application/JSON; Charset=utf-8"
        ~body:{|{"name":"Alice","active":true}|}
      |> Result.unwrap
    );
  Ok ()

let test_body_parser_rejects_multipart_without_boundary = fun _ctx ->
  let config = { Body_parser.parsers = [ Body_parser.Multipart ]; max_body_size = 1_024 } in
  match Body_parser.For_testing.parse_body
    config
    ~content_type:"multipart/form-data"
    ~body:"field=value" with
  | Error Body_parser.MissingMultipartBoundary -> Ok ()
  | Ok _ -> Error "expected missing multipart boundary to fail"
  | Error error -> Error (Body_parser.parse_error_to_string error)

let tests =
  Test.[
    case "body parser rejects oversized bodies" test_body_parser_rejects_oversized_bodies;
    case "body parser rejects invalid json" test_body_parser_rejects_invalid_json;
    case "body parser rejects json root arrays" test_body_parser_rejects_json_root_arrays;
    case
      "body parser accepts case insensitive json content type"
      test_body_parser_accepts_case_insensitive_json_content_type;
    case
      "body parser rejects multipart without boundary"
      test_body_parser_rejects_multipart_without_boundary;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-body-parser" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
