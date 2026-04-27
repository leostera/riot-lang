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

let test_static_mount_matching_respects_segment_boundaries = fun _ctx ->
  Test.assert_true (Static.For_testing.matches_mount ~at:"/assets" ~request_path:"/assets");
  Test.assert_true (Static.For_testing.matches_mount ~at:"/assets" ~request_path:"/assets/app.css");
  Test.assert_false
    (Static.For_testing.matches_mount ~at:"/assets" ~request_path:"/assets2/app.css");
  Test.assert_false (Static.For_testing.matches_mount ~at:"/assets" ~request_path:"/asset");
  Ok ()

let test_static_root_boundary_is_component_based = fun _ctx ->
  Test.assert_true
    (Static.For_testing.path_is_within_root
      ~root:(Path.v "/var/www")
      (Path.v "/var/www/images/logo.png"));
  Test.assert_true
    (Static.For_testing.path_is_within_root ~root:(Path.v "/var/www") (Path.v "/var/www"));
  Test.assert_false
    (Static.For_testing.path_is_within_root ~root:(Path.v "/var/www") (Path.v "/var/www2/file"));
  Ok ()

let test_static_dotfile_detection_checks_all_segments = fun _ctx ->
  Test.assert_true (Static.For_testing.path_has_dot_segment (Path.v ".env"));
  Test.assert_true (Static.For_testing.path_has_dot_segment (Path.v "public/.git/config"));
  Test.assert_true (Static.For_testing.path_has_dot_segment (Path.v "nested/.well-known/token"));
  Test.assert_false (Static.For_testing.path_has_dot_segment (Path.v "public/assets/app.css"));
  Ok ()

let test_static_directory_listing_escapes_displayed_values = fun _ctx ->
  let html =
    Static.For_testing.directory_listing_html
      ~request_path:"/files/<root>"
      ~path:(Path.v "/tmp/<root>")
      ~entries:[ ("<script>alert(1)</script>", false, 12, 0.0); ]
  in
  Test.assert_true (String.contains html "Index of /tmp/&lt;root&gt;");
  Test.assert_true (String.contains html "&lt;script&gt;alert(1)&lt;/script&gt;");
  Test.assert_false (String.contains html "<script>alert(1)</script>");
  Ok ()

let tests =
  Test.[
    case
      "static mount matching respects segment boundaries"
      test_static_mount_matching_respects_segment_boundaries;
    case "static root boundary is component based" test_static_root_boundary_is_component_based;
    case
      "static dotfile detection checks all segments"
      test_static_dotfile_detection_checks_all_segments;
    case
      "static directory listing escapes displayed values"
      test_static_directory_listing_escapes_displayed_values;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-static" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
