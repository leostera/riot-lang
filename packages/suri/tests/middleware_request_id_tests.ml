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

let test_request_id_accepts_valid_client_id = fun _ctx ->
  Test.assert_true (Request_id.For_testing.is_valid_request_id "trace-123_ABC.~");
  Test.assert_equal
    ~expected:"trace-123_ABC.~"
    ~actual:(Request_id.For_testing.choose_request_id
      ~generate:(fun () -> "generated")
      (Some "trace-123_ABC.~"));
  Ok ()

let test_request_id_rejects_control_characters = fun _ctx ->
  Test.assert_false (Request_id.For_testing.is_valid_request_id "trace\r\nx-evil: yes");
  Test.assert_equal
    ~expected:"generated"
    ~actual:(Request_id.For_testing.choose_request_id
      ~generate:(fun () -> "generated")
      (Some "trace\r\nx-evil: yes"));
  Ok ()

let test_request_id_rejects_empty_and_overlong_values = fun _ctx ->
  let too_long = String.make ~len:(Request_id.For_testing.max_request_id_length + 1) ~char:'a' in
  Test.assert_false (Request_id.For_testing.is_valid_request_id "");
  Test.assert_false (Request_id.For_testing.is_valid_request_id too_long);
  Test.assert_equal
    ~expected:"generated"
    ~actual:(Request_id.For_testing.choose_request_id ~generate:(fun () -> "generated") None);
  Ok ()

let tests =
  Test.[
    case "request id accepts valid client id" test_request_id_accepts_valid_client_id;
    case "request id rejects control characters" test_request_id_rejects_control_characters;
    case
      "request id rejects empty and overlong values"
      test_request_id_rejects_empty_and_overlong_values;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-request-id" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
