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

let test_accepts_rejects_invalid_quality = fun _ctx ->
  match Accepts.parse_accept "application/json;q=wat" with
  | Error (Accepts.InvalidQuality (Accepts.InvalidQualityValue { value = "wat" })) -> Ok ()
  | Ok _ -> Error "expected invalid Accept quality to fail parsing"
  | Error _ -> Error "unexpected Accept parse error"

let test_accepts_rejects_q_zero_matches = fun _ctx ->
  match Accepts.accept_header_matches ~types:[ "application/json"; ] "application/json;q=0" with
  | Ok false -> Ok ()
  | Ok true -> Error "expected q=0 Accept entry to be unacceptable"
  | Error _ -> Error "unexpected Accept parse error"

let test_accepts_matches_client_wildcards = fun _ctx ->
  match Accepts.accept_header_matches ~types:[ "application/json"; ] "*/*;q=0.5" with
  | Ok true -> Ok ()
  | Ok false -> Error "expected */* Accept entry to match supported JSON"
  | Error _ -> Error "unexpected Accept parse error"

let test_accepts_only_requires_content_type_for_declared_body = fun _ctx ->
  Test.assert_false
    (Accepts.request_declares_body ~method_:Net.Http.Method.Post ~headers:Net.Http.Header.empty);
  Test.assert_false
    (Accepts.request_declares_body
      ~method_:Net.Http.Method.Post
      ~headers:(Net.Http.Header.of_list [ ("content-length", "0"); ]));
  Test.assert_true
    (Accepts.request_declares_body
      ~method_:Net.Http.Method.Post
      ~headers:(Net.Http.Header.of_list [ ("content-length", "12"); ]));
  Test.assert_true
    (Accepts.request_declares_body
      ~method_:Net.Http.Method.Patch
      ~headers:(Net.Http.Header.of_list [ ("transfer-encoding", "chunked"); ]));
  Ok ()

let tests =
  Test.[
    case "accepts rejects invalid quality" test_accepts_rejects_invalid_quality;
    case "accepts rejects q zero matches" test_accepts_rejects_q_zero_matches;
    case "accepts matches client wildcards" test_accepts_matches_client_wildcards;
    case
      "accepts only requires content type for declared body"
      test_accepts_only_requires_content_type_for_declared_body;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-accepts" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
