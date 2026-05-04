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

let test_body_parser_rejects_oversized_bodies = fun _ctx ->
  let config = { Body_parser.parsers = [ Body_parser.Json ]; max_body_size = 2 } in
  match Body_parser.parse_body config ~content_type:"application/json" ~body:"{} " with
  | Error (Body_parser.BodyTooLarge { size; max_size }) ->
      Test.assert_equal ~expected:3 ~actual:size;
      Test.assert_equal ~expected:2 ~actual:max_size;
      Ok ()
  | Ok _ -> Error "expected oversized body to fail"
  | Error error -> Error (Body_parser.parse_error_to_string error)

let test_body_parser_rejects_invalid_json = fun _ctx ->
  match Body_parser.parse_body
    (Body_parser.default_config ())
    ~content_type:"application/json"
    ~body:{|{"name":|} with
  | Error (Body_parser.InvalidJson _) -> Ok ()
  | Ok _ -> Error "expected invalid JSON to fail"
  | Error error -> Error (Body_parser.parse_error_to_string error)

let test_body_parser_rejects_json_root_arrays = fun _ctx ->
  match Body_parser.parse_body
    (Body_parser.default_config ())
    ~content_type:"application/json"
    ~body:{|["alice"]|} with
  | Error (Body_parser.JsonRootNotObject Body_parser.JsonArray) -> Ok ()
  | Ok _ -> Error "expected JSON array body to fail"
  | Error error -> Error (Body_parser.parse_error_to_string error)

let test_body_parser_accepts_case_insensitive_json_content_type = fun _ctx ->
  Test.assert_equal
    ~expected:[ ("name", "Alice"); ("active", "true"); ]
    ~actual:(
      Body_parser.parse_body
        (Body_parser.default_config ())
        ~content_type:"Application/JSON; Charset=utf-8"
        ~body:{|{"name":"Alice","active":true}|}
      |> Result.unwrap
    );
  Ok ()

let test_body_parser_rejects_multipart_without_boundary = fun _ctx ->
  let config = { Body_parser.parsers = [ Body_parser.Multipart ]; max_body_size = 1_024 } in
  match Body_parser.parse_body config ~content_type:"multipart/form-data" ~body:"field=value" with
  | Error Body_parser.MissingMultipartBoundary -> Ok ()
  | Ok _ -> Error "expected missing multipart boundary to fail"
  | Error error -> Error (Body_parser.parse_error_to_string error)

let test_body_parser_rejects_unsupported_multipart = fun _ctx ->
  let config = { Body_parser.parsers = [ Body_parser.Multipart ]; max_body_size = 1_024 } in
  match Body_parser.parse_body
    config
    ~content_type:"multipart/form-data; boundary=\"abc123\""
    ~body:"--abc123\r\n" with
  | Error (Body_parser.UnsupportedMultipart { boundary = "abc123" }) -> Ok ()
  | Ok _ -> Error "expected multipart body to be unsupported"
  | Error error -> Error (Body_parser.parse_error_to_string error)

let test_body_parser_responds_unsupported_media_type_for_multipart = fun _ctx ->
  let config = { Body_parser.parsers = [ Body_parser.Multipart ]; max_body_size = 1_024 } in
  let conn =
    Suri.Testing.Conn.make
      ~headers:[ ("content-type", "multipart/form-data; boundary=abc123"); ]
      ~body:"--abc123\r\n"
      ()
    |> Result.unwrap
  in
  let conn = Body_parser.make ~config () ~conn ~next:(fun conn -> conn) in
  Test.assert_equal
    ~expected:Net.Http.Status.UnsupportedMediaType
    ~actual:(Conn.to_response conn).status;
  Ok ()

let test_body_parser_stores_raw_json = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make
      ~headers:[ ("content-type", "application/json"); ]
      ~body:{|{"name":"Alice","nested":{"role":"admin"}}|}
      ()
    |> Result.unwrap
  in
  let conn = Body_parser.make () ~conn ~next:(fun conn -> conn) in
  Test.assert_equal ~expected:[ ("name", "Alice"); ] ~actual:(Conn.body_params conn);
  match Body_parser.parsed_json conn with
  | Some json ->
      let nested =
        Data.Json.get_field "nested" json
        |> Option.and_then ~fn:(Data.Json.get_field "role")
        |> Option.and_then ~fn:Data.Json.get_string
      in
      Test.assert_equal ~expected:(Some "admin") ~actual:nested;
      Ok ()
  | None -> Error "expected body parser to store parsed JSON"

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
    case "body parser rejects unsupported multipart" test_body_parser_rejects_unsupported_multipart;
    case
      "body parser responds unsupported media type for multipart"
      test_body_parser_responds_unsupported_media_type_for_multipart;
    case "body parser stores raw json" test_body_parser_stores_raw_json;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-body-parser" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
