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

let test_remote_ip_ignores_forwarded_header_from_untrusted_peer = fun _ctx ->
  Test.assert_equal
    ~expected:(Error (Remote_ip.UntrustedPeer { peer_ip = "203.0.113.10" }))
    ~actual:(Remote_ip.resolve_real_ip_result
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"203.0.113.10"
      ~header_value:"127.0.0.1");
  Test.assert_equal
    ~expected:None
    ~actual:(Remote_ip.resolve_real_ip
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"203.0.113.10"
      ~header_value:"127.0.0.1");
  Ok ()

let test_remote_ip_resolves_forwarded_header_from_trusted_peer = fun _ctx ->
  Test.assert_equal
    ~expected:(Ok "1.2.3.4")
    ~actual:(Remote_ip.resolve_real_ip_result
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"10.0.1.50"
      ~header_value:"1.2.3.4, 10.0.1.50");
  Test.assert_equal
    ~expected:(Some "1.2.3.4")
    ~actual:(Remote_ip.resolve_real_ip
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"10.0.1.50"
      ~header_value:"1.2.3.4, 10.0.1.50");
  Ok ()

let test_remote_ip_walks_trusted_proxy_chain = fun _ctx ->
  Test.assert_equal
    ~expected:(Some "5.6.7.8")
    ~actual:(Remote_ip.resolve_real_ip
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"10.0.1.50"
      ~header_value:"1.2.3.4, 5.6.7.8, 10.0.1.50");
  Ok ()

let test_remote_ip_rejects_invalid_forwarded_tokens = fun _ctx ->
  Test.assert_false (Remote_ip.is_valid_ip_literal "example.com");
  Test.assert_false (Remote_ip.is_valid_ip_literal "999.1.2.3");
  Test.assert_true (Remote_ip.is_valid_ip_literal "127.0.0.1");
  Test.assert_true (Remote_ip.is_valid_ip_literal "2001:db8::1");
  Test.assert_equal
    ~expected:(Error (Remote_ip.InvalidForwardedIp { value = "example.com" }))
    ~actual:(Remote_ip.resolve_real_ip_result
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"10.0.1.50"
      ~header_value:"example.com, 10.0.1.50");
  Test.assert_equal
    ~expected:None
    ~actual:(Remote_ip.resolve_real_ip
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"10.0.1.50"
      ~header_value:"example.com, 10.0.1.50");
  Ok ()

let test_remote_ip_reports_empty_forwarded_chain = fun _ctx ->
  Test.assert_equal
    ~expected:(Error Remote_ip.EmptyForwardedFor)
    ~actual:(Remote_ip.resolve_real_ip_result
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"10.0.1.50"
      ~header_value:" , ");
  Ok ()

let test_remote_ip_reports_all_trusted_chain = fun _ctx ->
  Test.assert_equal
    ~expected:(Error Remote_ip.NoClientIpInForwardedChain)
    ~actual:(Remote_ip.resolve_real_ip_result
      ~proxies:[ "10.0.1.50"; "10.0.1.51"; ]
      ~peer_ip:"10.0.1.50"
      ~header_value:"10.0.1.51, 10.0.1.50");
  Ok ()

let test_remote_ip_middleware_ignores_spoofed_header = fun _ctx ->
  let peer = Conn.{ ip = "203.0.113.10"; port = 1_234 } in
  let conn =
    Testing.Conn.make ~peer ~headers:[ ("x-forwarded-for", "127.0.0.1"); ] ()
    |> Result.unwrap
  in
  let conn = Remote_ip.middleware () ~proxies:[ "10.0.1.50"; ] ~conn ~next:(fun conn -> conn) in
  Test.assert_equal ~expected:"203.0.113.10" ~actual:(Conn.peer conn).ip;
  Ok ()

let test_remote_ip_middleware_rewrites_trusted_proxy_peer = fun _ctx ->
  let peer = Conn.{ ip = "10.0.1.50"; port = 1_234 } in
  let conn =
    Testing.Conn.make ~peer ~headers:[ ("x-forwarded-for", "1.2.3.4, 10.0.1.50"); ] ()
    |> Result.unwrap
  in
  let conn = Remote_ip.middleware () ~proxies:[ "10.0.1.50"; ] ~conn ~next:(fun conn -> conn) in
  Test.assert_equal ~expected:"1.2.3.4" ~actual:(Conn.peer conn).ip;
  Ok ()

let test_remote_ip_middleware_ignores_invalid_forwarded_chain = fun _ctx ->
  let peer = Conn.{ ip = "10.0.1.50"; port = 1_234 } in
  let conn =
    Testing.Conn.make ~peer ~headers:[ ("x-forwarded-for", "example.com, 10.0.1.50"); ] ()
    |> Result.unwrap
  in
  let conn = Remote_ip.middleware () ~proxies:[ "10.0.1.50"; ] ~conn ~next:(fun conn -> conn) in
  Test.assert_equal ~expected:"10.0.1.50" ~actual:(Conn.peer conn).ip;
  Ok ()

let tests =
  Test.[
    case
      "remote ip ignores forwarded header from untrusted peer"
      test_remote_ip_ignores_forwarded_header_from_untrusted_peer;
    case
      "remote ip resolves forwarded header from trusted peer"
      test_remote_ip_resolves_forwarded_header_from_trusted_peer;
    case "remote ip walks trusted proxy chain" test_remote_ip_walks_trusted_proxy_chain;
    case
      "remote ip rejects invalid forwarded tokens"
      test_remote_ip_rejects_invalid_forwarded_tokens;
    case "remote ip reports empty forwarded chain" test_remote_ip_reports_empty_forwarded_chain;
    case "remote ip reports all trusted chain" test_remote_ip_reports_all_trusted_chain;
    case
      "remote ip middleware ignores spoofed header"
      test_remote_ip_middleware_ignores_spoofed_header;
    case
      "remote ip middleware rewrites trusted proxy peer"
      test_remote_ip_middleware_rewrites_trusted_proxy_peer;
    case
      "remote ip middleware ignores invalid forwarded chain"
      test_remote_ip_middleware_ignores_invalid_forwarded_chain;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-remote-ip" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
