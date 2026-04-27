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

let test_static_mount_matching_respects_segment_boundaries = fun _ctx ->
  Test.assert_true (Static.matches_mount ~at:"/assets" ~request_path:"/assets");
  Test.assert_true (Static.matches_mount ~at:"/assets" ~request_path:"/assets/app.css");
  Test.assert_false (Static.matches_mount ~at:"/assets" ~request_path:"/assets2/app.css");
  Test.assert_false (Static.matches_mount ~at:"/assets" ~request_path:"/asset");
  Ok ()

let test_static_root_boundary_is_component_based = fun _ctx ->
  Test.assert_true
    (Static.path_is_within_root ~root:(Path.v "/var/www") (Path.v "/var/www/images/logo.png"));
  Test.assert_true (Static.path_is_within_root ~root:(Path.v "/var/www") (Path.v "/var/www"));
  Test.assert_false (Static.path_is_within_root ~root:(Path.v "/var/www") (Path.v "/var/www2/file"));
  Ok ()

let test_static_dotfile_detection_checks_all_segments = fun _ctx ->
  Test.assert_true (Static.path_has_dot_segment (Path.v ".env"));
  Test.assert_true (Static.path_has_dot_segment (Path.v "public/.git/config"));
  Test.assert_true (Static.path_has_dot_segment (Path.v "nested/.well-known/token"));
  Test.assert_false (Static.path_has_dot_segment (Path.v "public/assets/app.css"));
  Ok ()

let test_static_directory_listing_escapes_displayed_values = fun _ctx ->
  let html =
    Static.directory_listing_html
      ~request_path:"/files/<root>"
      ~path:(Path.v "/tmp/<root>")
      ~entries:[ ("<script>alert(1)</script>", false, 12, 0.0); ]
  in
  Test.assert_true (String.contains html "Index of /tmp/&lt;root&gt;");
  Test.assert_true (String.contains html "&lt;script&gt;alert(1)&lt;/script&gt;");
  Test.assert_false (String.contains html "<script>alert(1)</script>");
  Ok ()

let expect_tempdir = fun result ->
  match result with
  | Ok result -> result
  | Error error ->
      Error ("failed to create temporary static test directory: " ^ IO.error_message error)

let test_static_normalize_path_returns_structured_errors = fun _ctx ->
  Fs.with_tempdir
    (fun root ->
      let config = Static.default_config in
      match Fs.canonicalize root with
      | Error _ -> Error "failed to canonicalize static test root"
      | Ok canonical_root ->
          let missing_root = Path.join root (Path.v "missing-root") in
          let missing_file = Path.v "missing.txt" in
          let expected_missing =
            Path.join canonical_root missing_file
            |> Path.normalize
          in
          let traversal = Path.v "../outside.txt" in
          let link_path = Path.join root (Path.v "link.txt") in
          let expected_link =
            Path.join canonical_root (Path.v "link.txt")
            |> Path.normalize
          in
          let target_path = Path.join root (Path.v "target.txt") in
          match Fs.write "ok" target_path with
          | Error _ -> Error "failed to create static symlink target"
          | Ok () -> (
              match Fs.symlink ~src:(Path.v "target.txt") ~dst:link_path with
              | Error _ -> Error "failed to create static symlink"
              | Ok () ->
                  let checks = [
                    (
                      fun () ->
                        match Static.normalize_path config missing_root (Path.v ".") with
                        | Error (Static.InvalidRoot { root; _ }) ->
                            Test.assert_true (Path.equal root missing_root);
                            Ok ()
                        | Ok _ -> Error "expected invalid root error"
                        | Error _ -> Error "expected invalid root error"
                    );
                    (
                      fun () ->
                        match Static.normalize_path config root missing_file with
                        | Error (Static.MissingPath { path }) ->
                            Test.assert_true (Path.equal path expected_missing);
                            Ok ()
                        | Ok _ -> Error "expected missing path error"
                        | Error _ -> Error "expected missing path error"
                    );
                    (
                      fun () ->
                        match Static.normalize_path config root traversal with
                        | Error (Static.PathTraversal { requested; resolved = None; _ }) ->
                            Test.assert_true (Path.equal requested traversal);
                            Ok ()
                        | Ok _ -> Error "expected traversal error"
                        | Error _ -> Error "expected traversal error"
                    );
                    (
                      fun () ->
                        let deny_config = Static.{ default_config with symlinks = DenySymlinks } in
                        match Static.normalize_path deny_config root (Path.v "link.txt") with
                        | Error (Static.SymlinkDenied { symlink; requested; _ }) ->
                            Test.assert_true (Path.equal symlink expected_link);
                            Test.assert_true (Path.equal requested (Path.v "link.txt"));
                            Ok ()
                        | Ok _ -> Error "expected symlink denied error"
                        | Error _ -> Error "expected symlink denied error"
                    );
                  ]
                  in
                  List.fold_left
                    checks
                    ~init:(Ok ())
                    ~fn:(fun result check ->
                      match result with
                      | Error _ -> result
                      | Ok () -> check ())
            ))
  |> expect_tempdir

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
    case
      "static normalize path returns structured errors"
      test_static_normalize_path_returns_structured_errors;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-static" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
