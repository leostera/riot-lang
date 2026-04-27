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

let test_config_validates_default_development = fun _ctx ->
  match Config.validate Config.default with
  | Ok _ -> Ok ()
  | Error errors -> Error (Config.errors_to_string errors)

let test_config_parses_env_aliases = fun _ctx ->
  Test.assert_equal ~expected:(Ok Config.Development) ~actual:(Config.env_from_string "dev");
  Test.assert_equal ~expected:(Ok Config.Production) ~actual:(Config.env_from_string "prod");
  Test.assert_equal ~expected:(Ok Config.Test) ~actual:(Config.env_from_string "TEST");
  Ok ()

let test_config_rejects_production_placeholder_secret = fun _ctx ->
  match Config.validate (config_for_test ~env:Config.Production ()) with
  | Error errors ->
      Test.assert_true
        (List.contains errors ~value:(Config.InvalidLiveViewSecret Config.Placeholder));
      Ok ()
  | Ok _ -> Error "expected production placeholder liveview secret to be rejected"

let test_config_rejects_invalid_limits = fun _ctx ->
  let config =
    config_for_test
      ~port:0
      ~acceptors:0
      ~max_request_line_length:0
      ~max_header_count:0
      ~max_header_length:0
      ~buffer_size:0
      ~liveview_secret:"short"
      ()
  in
  match Config.validate config with
  | Error errors ->
      Test.assert_true (List.contains errors ~value:(Config.InvalidPort 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidAcceptors 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidMaxRequestLineLength 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidMaxHeaderCount 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidMaxHeaderLength 0));
      Test.assert_true (List.contains errors ~value:(Config.InvalidBufferSize 0));
      Test.assert_true
        (List.contains errors ~value:(Config.InvalidLiveViewSecret (Config.TooShort 5)));
      Ok ()
  | Ok _ -> Error "expected invalid config limits to be rejected"

let test_component_text_is_escaped = fun _ctx ->
  let html =
    Component.div [ Component.text "<script>alert('x') & \"y\"</script>"; ]
    |> Component.to_html
  in
  Test.assert_equal
    ~expected:"<div>&lt;script&gt;alert(&#39;x&#39;) &amp; &quot;y&quot;&lt;/script&gt;</div>"
    ~actual:html;
  Ok ()

let test_component_attrs_are_escaped = fun _ctx ->
  let html =
    Component.div
      ~attrs:[ Component.attr "title" "\"<&>'"; Component.attr "data-user" "alice&bob"; ]
      []
    |> Component.to_html
  in
  Test.assert_equal
    ~expected:"<div title=\"&quot;&lt;&amp;&gt;&#39;\" data-user=\"alice&amp;bob\"></div>"
    ~actual:html;
  Ok ()

let test_component_invalid_attr_name_is_omitted = fun _ctx ->
  let html =
    Component.div
      ~attrs:[ Component.attr "title" "safe"; Component.attr "onload x" "alert(1)"; ]
      [ Component.text "ok"; ]
    |> Component.to_html
  in
  Test.assert_equal ~expected:"<div title=\"safe\">ok</div>" ~actual:html;
  Ok ()

let test_component_invalid_tag_name_renders_children_safely = fun _ctx ->
  let html =
    Component.el "img src=x onerror=alert(1)" [ Component.text "fallback <b>text</b>"; ]
    |> Component.to_html
  in
  Test.assert_equal ~expected:"fallback &lt;b&gt;text&lt;/b&gt;" ~actual:html;
  Ok ()

let test_component_script_and_style_are_raw_text = fun _ctx ->
  let html =
    Component.fragment
      [
        Component.script "const ok = value => value < 3 && value > 0;";
        Component.style ".icon::before { content: \"<\"; }";
      ]
    |> Component.to_html
  in
  Test.assert_equal
    ~expected:"<script>const ok = value => value < 3 && value > 0;</script><style>.icon::before { content: \"<\"; }</style>"
    ~actual:html;
  Ok ()

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

let test_router_matcher_ignores_empty_path_segments = fun _ctx ->
  Test.assert_equal
    ~expected:(Some [ ("id", "123"); ])
    ~actual:(Router.For_testing.match_path "/users/:id" "//users/123/");
  Ok ()

let test_router_matcher_keeps_root_exact = fun _ctx ->
  Test.assert_equal ~expected:(Some []) ~actual:(Router.For_testing.match_path "/" "/");
  Test.assert_equal ~expected:None ~actual:(Router.For_testing.match_path "/" "/assets");
  Ok ()

let test_router_matcher_rejects_partial_literal_segments = fun _ctx ->
  Test.assert_equal ~expected:None ~actual:(Router.For_testing.match_path "/assets" "/assets2");
  Test.assert_equal ~expected:(Some []) ~actual:(Router.For_testing.match_path "/assets" "/assets/");
  Ok ()

let test_cors_rejects_wildcard_origin_with_credentials = fun _ctx ->
  try
    let _middleware = Cors.middleware ~origins:[ "*" ] ~credentials:true () in
    Error "expected wildcard credentials CORS config to be rejected"
  with
  | Cors.Invalid_config Cors.WildcardOriginWithCredentials -> Ok ()
  | _ -> Error "unexpected CORS config exception"

let test_cors_preflight_rejects_disallowed_method = fun _ctx ->
  match Cors.For_testing.validate_preflight
    ~methods:[ Net.Http.Method.Put; ]
    ~headers:[]
    ~request_method:"delete"
    ~request_headers:None with
  | Error (Cors.MethodNotAllowed "DELETE") -> Ok ()
  | Ok () -> Error "expected disallowed CORS preflight method"
  | Error error -> Error (Cors.preflight_error_to_string error)

let test_cors_preflight_rejects_disallowed_headers = fun _ctx ->
  match Cors.For_testing.validate_preflight
    ~methods:[ Net.Http.Method.Put; ]
    ~headers:[ "authorization"; ]
    ~request_method:"PUT"
    ~request_headers:(Some "Authorization, X-Evil") with
  | Error (Cors.HeadersNotAllowed headers) ->
      Test.assert_equal ~expected:[ "x-evil"; ] ~actual:headers;
      Ok ()
  | Ok () -> Error "expected disallowed CORS preflight headers"
  | Error error -> Error (Cors.preflight_error_to_string error)

let test_cors_preflight_allows_configured_headers = fun _ctx ->
  match Cors.For_testing.validate_preflight
    ~methods:[ Net.Http.Method.Put; ]
    ~headers:[ "authorization"; "x-client"; ]
    ~request_method:"put"
    ~request_headers:(Some "Authorization, X-Client, Content-Type") with
  | Ok () -> Ok ()
  | Error error -> Error (Cors.preflight_error_to_string error)

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

let test_remote_ip_ignores_forwarded_header_from_untrusted_peer = fun _ctx ->
  Test.assert_equal
    ~expected:None
    ~actual:(Remote_ip.For_testing.resolve_real_ip
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"203.0.113.10"
      ~header_value:"127.0.0.1");
  Ok ()

let test_remote_ip_resolves_forwarded_header_from_trusted_peer = fun _ctx ->
  Test.assert_equal
    ~expected:(Some "1.2.3.4")
    ~actual:(Remote_ip.For_testing.resolve_real_ip
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"10.0.1.50"
      ~header_value:"1.2.3.4, 10.0.1.50");
  Ok ()

let test_remote_ip_walks_trusted_proxy_chain = fun _ctx ->
  Test.assert_equal
    ~expected:(Some "5.6.7.8")
    ~actual:(Remote_ip.For_testing.resolve_real_ip
      ~proxies:[ "10.0.1.50"; ]
      ~peer_ip:"10.0.1.50"
      ~header_value:"1.2.3.4, 5.6.7.8, 10.0.1.50");
  Ok ()

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

let test_accepts_rejects_invalid_quality = fun _ctx ->
  match Accepts.parse_accept_result "application/json;q=wat" with
  | Error (Accepts.InvalidQuality "wat") -> Ok ()
  | Ok _ -> Error "expected invalid Accept quality to fail parsing"
  | Error _ -> Error "unexpected Accept parse error"

let test_accepts_rejects_q_zero_matches = fun _ctx ->
  match Accepts.For_testing.accept_header_matches
    ~types:[ "application/json"; ]
    "application/json;q=0" with
  | Ok false -> Ok ()
  | Ok true -> Error "expected q=0 Accept entry to be unacceptable"
  | Error _ -> Error "unexpected Accept parse error"

let test_accepts_matches_client_wildcards = fun _ctx ->
  match Accepts.For_testing.accept_header_matches ~types:[ "application/json"; ] "*/*;q=0.5" with
  | Ok true -> Ok ()
  | Ok false -> Error "expected */* Accept entry to match supported JSON"
  | Error _ -> Error "unexpected Accept parse error"

let test_accepts_only_requires_content_type_for_declared_body = fun _ctx ->
  Test.assert_false
    (Accepts.For_testing.request_declares_body
      ~method_:Net.Http.Method.Post
      ~headers:Net.Http.Header.empty);
  Test.assert_false
    (Accepts.For_testing.request_declares_body
      ~method_:Net.Http.Method.Post
      ~headers:(Net.Http.Header.of_list [ ("content-length", "0"); ]));
  Test.assert_true
    (Accepts.For_testing.request_declares_body
      ~method_:Net.Http.Method.Post
      ~headers:(Net.Http.Header.of_list [ ("content-length", "12"); ]));
  Test.assert_true
    (Accepts.For_testing.request_declares_body
      ~method_:Net.Http.Method.Patch
      ~headers:(Net.Http.Header.of_list [ ("transfer-encoding", "chunked"); ]));
  Ok ()

let test_logger_sanitizes_control_characters_in_paths = fun _ctx ->
  Test.assert_equal
    ~expected:"/login%0D%0Ax-evil: yes/%7F"
    ~actual:(Logger.For_testing.sanitize_path "/login\r\nx-evil: yes/\x7F");
  Ok ()

let test_basic_auth_accepts_case_insensitive_scheme = fun _ctx ->
  let encoded = Encoding.Base64.encode "alice:s3cret" in
  Test.assert_equal
    ~expected:(Some ("alice", "s3cret"))
    ~actual:(Basic_auth.For_testing.decode_credentials ("bAsIc " ^ encoded));
  Ok ()

let test_basic_auth_ignores_extra_spaces = fun _ctx ->
  let encoded = Encoding.Base64.encode "alice:s3cret" in
  Test.assert_equal
    ~expected:(Some ("alice", "s3cret"))
    ~actual:(Basic_auth.For_testing.decode_credentials ("  Basic   " ^ encoded ^ "  "));
  Ok ()

let test_basic_auth_preserves_colons_in_password = fun _ctx ->
  let encoded = Encoding.Base64.encode "alice:s3:cr:et" in
  Test.assert_equal
    ~expected:(Some ("alice", "s3:cr:et"))
    ~actual:(Basic_auth.For_testing.decode_credentials ("Basic " ^ encoded));
  Ok ()

let test_basic_auth_rejects_invalid_credentials = fun _ctx ->
  Test.assert_equal
    ~expected:None
    ~actual:(Basic_auth.For_testing.decode_credentials "Bearer token");
  Test.assert_equal
    ~expected:None
    ~actual:(Basic_auth.For_testing.decode_credentials "Basic not-base64");
  Ok ()

let test_basic_auth_sanitizes_realm_header_value = fun _ctx ->
  Test.assert_equal
    ~expected:"AdminPanel"
    ~actual:(Basic_auth.For_testing.sanitize_realm "Admin\r\n\"Panel");
  Ok ()

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

let test_csrf_generates_raw_hex_tokens = fun _ctx ->
  let token = Csrf.For_testing.generate_token () in
  Test.assert_equal ~expected:64 ~actual:(String.length token);
  Test.assert_true (Csrf.For_testing.is_raw_token token);
  Ok ()

let test_csrf_masking_roundtrips_and_uses_unique_masks = fun _ctx ->
  let token = Csrf.For_testing.generate_token () in
  let masked1 = Csrf.For_testing.mask_token token in
  let masked2 = Csrf.For_testing.mask_token token in
  Test.assert_false (String.equal masked1 masked2);
  Test.assert_equal ~expected:(Some token) ~actual:(Csrf.For_testing.unmask_token masked1);
  Test.assert_equal ~expected:(Some token) ~actual:(Csrf.For_testing.unmask_token masked2);
  Ok ()

let test_csrf_rejects_malformed_masked_tokens = fun _ctx ->
  Test.assert_equal ~expected:None ~actual:(Csrf.For_testing.unmask_token "not-base64");
  Test.assert_equal
    ~expected:None
    ~actual:(Csrf.For_testing.unmask_token (Encoding.Base64.encode "too-short"));
  Ok ()

let test_csrf_secure_equal_checks_full_token = fun _ctx ->
  let token = Csrf.For_testing.generate_token () in
  let last = String.get_unchecked token ~at:(String.length token - 1) in
  let replacement =
    if last = '0' then
      "1"
    else
      "0"
  in
  Test.assert_true (Csrf.For_testing.secure_equal token token);
  Test.assert_false (Csrf.For_testing.secure_equal token (String.sub token ~offset:0 ~len:63));
  Test.assert_false
    (Csrf.For_testing.secure_equal token (String.sub token ~offset:0 ~len:63 ^ replacement));
  Ok ()

let test_session_middleware_installs_session = fun _ctx ->
  let conn = Conn.For_testing.make () in
  let found_session = ref false in
  let middleware = Session.middleware ~secret:"testing-session-secret" () in
  let _conn' =
    middleware
      ~conn
      ~next:(fun conn ->
        found_session := Option.is_some (Session.find conn);
        conn)
  in
  Test.assert_true !found_session;
  Ok ()

let test_csrf_requires_session_middleware = fun _ctx ->
  let conn =
    Conn.For_testing.make
      ~method_:Net.Http.Method.Post
      ~body_params:[ ("_csrf_token", Csrf.For_testing.generate_token ()); ]
      ()
  in
  let continued = ref false in
  let middleware = Csrf.middleware () in
  let conn' =
    middleware
      ~conn
      ~next:(fun conn ->
        continued := true;
        conn)
  in
  let response = Conn.to_response conn' in
  Test.assert_false !continued;
  Test.assert_equal ~expected:Net.Http.Status.InternalServerError ~actual:response.status;
  Test.assert_equal ~expected:Csrf.For_testing.missing_session_body ~actual:response.body;
  Ok ()

let test_http1_websocket_accept_matches_rfc_example = fun _ctx ->
  Test.assert_equal
    ~expected:"s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    ~actual:(Http1.compute_websocket_accept valid_websocket_key);
  Ok ()

let test_http1_websocket_upgrade_accepts_valid_request = fun _ctx ->
  match Http1.validate_websocket_upgrade (websocket_request ()) with
  | Ok key ->
      Test.assert_equal ~expected:valid_websocket_key ~actual:key;
      Ok ()
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_rejects_non_get = fun _ctx ->
  match Http1.validate_websocket_upgrade (websocket_request ~method_:Net.Http.Method.Post ()) with
  | Error (Http1.InvalidWebSocketMethod Net.Http.Method.Post) -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to reject non-GET request"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_rejects_http10 = fun _ctx ->
  match Http1.validate_websocket_upgrade (websocket_request ~version:Net.Http.Version.Http10 ()) with
  | Error (Http1.InvalidWebSocketVersion Net.Http.Version.Http10) -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to reject HTTP/1.0 request"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_requires_upgrade_header = fun _ctx ->
  let headers = [
    ("connection", "keep-alive, Upgrade");
    ("sec-websocket-version", "13");
    ("sec-websocket-key", valid_websocket_key);
  ]
  in
  match Http1.validate_websocket_upgrade (websocket_request ~headers ()) with
  | Error Http1.MissingWebSocketUpgrade -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to require Upgrade header"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_rejects_invalid_upgrade_header = fun _ctx ->
  let headers = [
    ("upgrade", "h2c");
    ("connection", "Upgrade");
    ("sec-websocket-version", "13");
    ("sec-websocket-key", valid_websocket_key);
  ]
  in
  match Http1.validate_websocket_upgrade (websocket_request ~headers ()) with
  | Error (Http1.InvalidWebSocketUpgrade "h2c") -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to reject non-websocket Upgrade header"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_requires_connection_token = fun _ctx ->
  let headers = [
    ("upgrade", "websocket");
    ("connection", "keep-alive");
    ("sec-websocket-version", "13");
    ("sec-websocket-key", valid_websocket_key);
  ]
  in
  match Http1.validate_websocket_upgrade (websocket_request ~headers ()) with
  | Error Http1.MissingWebSocketConnectionUpgrade -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to require Connection upgrade token"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_requires_version_13 = fun _ctx ->
  let headers = [
    ("upgrade", "websocket");
    ("connection", "Upgrade");
    ("sec-websocket-version", "12");
    ("sec-websocket-key", valid_websocket_key);
  ]
  in
  match Http1.validate_websocket_upgrade (websocket_request ~headers ()) with
  | Error (Http1.UnsupportedWebSocketVersion "12") -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to require Sec-WebSocket-Version: 13"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_rejects_invalid_key = fun _ctx ->
  let headers = [
    ("upgrade", "websocket");
    ("connection", "Upgrade");
    ("sec-websocket-version", "13");
    ("sec-websocket-key", Encoding.Base64.encode "too-short");
  ]
  in
  match Http1.validate_websocket_upgrade (websocket_request ~headers ()) with
  | Error (Http1.InvalidWebSocketKey _) -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to reject invalid Sec-WebSocket-Key"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_connection_write_all_retries_short_writes = fun _ctx ->
  let calls = ref [] in
  let write _buf ~pos ~len =
    calls := (pos, len) :: !calls;
    if len > 3 then
      Ok 3
    else
      Ok len
  in
  match Connection.write_all_with ~write "abcdefgh" with
  | Ok () ->
      Test.assert_equal ~expected:[ (6, 2); (3, 5); (0, 8); ] ~actual:!calls;
      Ok ()
  | Error `Closed -> Error "expected short writes to complete"

let test_connection_write_all_treats_zero_write_as_closed = fun _ctx ->
  let calls = ref 0 in
  let write _buf ~pos:_ ~len:_ =
    calls := !calls + 1;
    Ok 0
  in
  match Connection.write_all_with ~write "abc" with
  | Error `Closed ->
      Test.assert_equal ~expected:1 ~actual:!calls;
      Ok ()
  | Ok () -> Error "expected zero-byte write to close connection"

let test_connection_write_all_skips_empty_payload = fun _ctx ->
  let calls = ref 0 in
  let write _buf ~pos:_ ~len:_ =
    calls := !calls + 1;
    Ok 1
  in
  match Connection.write_all_with ~write "" with
  | Ok () ->
      Test.assert_equal ~expected:0 ~actual:!calls;
      Ok ()
  | Error `Closed -> Error "expected empty payload to complete without writes"

let test_connection_send_file_slice_extracts_range = fun _ctx ->
  match Connection.send_file_slice ~off:2 ~len:4 "abcdefgh" with
  | Ok chunk ->
      Test.assert_equal ~expected:"cdef" ~actual:chunk;
      Ok ()
  | Error (`Invalid_range _) -> Error "expected valid send_file range"

let test_connection_send_file_slice_allows_zero_length = fun _ctx ->
  match Connection.send_file_slice ~off:8 ~len:0 "abcdefgh" with
  | Ok chunk ->
      Test.assert_equal ~expected:"" ~actual:chunk;
      Ok ()
  | Error (`Invalid_range _) -> Error "expected zero-length send_file range"

let test_connection_send_file_slice_rejects_invalid_range = fun _ctx ->
  match Connection.send_file_slice ~off:6 ~len:3 "abcdefgh" with
  | Error (`Invalid_range { off; len; size }) ->
      Test.assert_equal ~expected:6 ~actual:off;
      Test.assert_equal ~expected:3 ~actual:len;
      Test.assert_equal ~expected:8 ~actual:size;
      Ok ()
  | Ok _ -> Error "expected send_file range beyond file size to fail"

let test_http1_response_rejects_invalid_header_name = fun _ctx ->
  let res = Response.ok ~headers:[ ("bad name", "value"); ] ~body:"ok" () in
  match Http1.serialize_response res with
  | Error (Http1.InvalidHeaderName name) ->
      Test.assert_equal ~expected:"bad name" ~actual:name;
      Ok ()
  | _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_rejects_header_injection = fun _ctx ->
  let res = Response.ok ~headers:[ ("x-test", "ok\r\nx-evil: yes"); ] ~body:"ok" () in
  match Http1.serialize_response res with
  | Error (Http1.InvalidHeaderValue { name; value = _ }) ->
      Test.assert_equal ~expected:"x-test" ~actual:name;
      Ok ()
  | _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_omits_body_for_no_content = fun _ctx ->
  let res = Response.no_content ~headers:[ ("content-length", "7"); ] ~body:"ignored" () in
  match Http1.serialize_response res with
  | Ok bytes ->
      Test.assert_false (String.contains bytes "content-length");
      Test.assert_false (String.contains bytes "ignored");
      Ok ()
  | Error _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_omits_body_for_not_modified = fun _ctx ->
  let res = Response.not_modified ~headers:[ ("content-length", "7"); ] ~body:"ignored" () in
  match Http1.serialize_response res with
  | Ok bytes ->
      Test.assert_false (String.contains bytes "content-length");
      Test.assert_false (String.contains bytes "ignored");
      Ok ()
  | Error _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_omits_body_for_informational_status = fun _ctx ->
  let res = Response.continue ~headers:[ ("content-length", "7"); ] ~body:"ignored" () in
  match Http1.serialize_response res with
  | Ok bytes ->
      Test.assert_false (String.contains bytes "content-length");
      Test.assert_false (String.contains bytes "ignored");
      Ok ()
  | Error _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_sets_content_length_for_body = fun _ctx ->
  let res = Response.ok ~body:"hello" () in
  match Http1.serialize_response res with
  | Ok bytes ->
      Test.assert_true (String.contains bytes "content-length: 5");
      Test.assert_true (String.contains bytes "\r\n\r\nhello");
      Ok ()
  | Error _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_does_not_add_vary_without_compression = fun _ctx ->
  let res = Response.ok ~body:"hello" () in
  match Http1.serialize_response res with
  | Ok bytes ->
      Test.assert_false (String.contains bytes "vary: accept-encoding");
      Test.assert_false (String.contains bytes "Vary: accept-encoding");
      Ok ()
  | Error _ ->
      Test.assert_true false;
      Ok ()

let tests =
  Test.[
    case "config validates default development" test_config_validates_default_development;
    case "config parses env aliases" test_config_parses_env_aliases;
    case
      "config rejects production placeholder secret"
      test_config_rejects_production_placeholder_secret;
    case "config rejects invalid limits" test_config_rejects_invalid_limits;
    case "component text is escaped" test_component_text_is_escaped;
    case "component attributes are escaped" test_component_attrs_are_escaped;
    case "component invalid attributes are omitted" test_component_invalid_attr_name_is_omitted;
    case
      "component invalid tags render children safely"
      test_component_invalid_tag_name_renders_children_safely;
    case "component script and style remain raw text" test_component_script_and_style_are_raw_text;
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
      "router matcher ignores empty path segments"
      test_router_matcher_ignores_empty_path_segments;
    case "router matcher keeps root exact" test_router_matcher_keeps_root_exact;
    case
      "router matcher rejects partial literal segments"
      test_router_matcher_rejects_partial_literal_segments;
    case
      "cors rejects wildcard origin with credentials"
      test_cors_rejects_wildcard_origin_with_credentials;
    case "cors preflight rejects disallowed method" test_cors_preflight_rejects_disallowed_method;
    case "cors preflight rejects disallowed headers" test_cors_preflight_rejects_disallowed_headers;
    case "cors preflight allows configured headers" test_cors_preflight_allows_configured_headers;
    case
      "conn query params handle missing and blank values"
      test_conn_query_params_handle_missing_and_blank_values;
    case "conn query params preserve repeated keys" test_conn_query_params_preserve_repeated_keys;
    case
      "conn query params decode percent and skip empty pairs"
      test_conn_query_params_decode_percent_and_skip_empty_pairs;
    case
      "remote ip ignores forwarded header from untrusted peer"
      test_remote_ip_ignores_forwarded_header_from_untrusted_peer;
    case
      "remote ip resolves forwarded header from trusted peer"
      test_remote_ip_resolves_forwarded_header_from_trusted_peer;
    case "remote ip walks trusted proxy chain" test_remote_ip_walks_trusted_proxy_chain;
    case "request id accepts valid client id" test_request_id_accepts_valid_client_id;
    case "request id rejects control characters" test_request_id_rejects_control_characters;
    case
      "request id rejects empty and overlong values"
      test_request_id_rejects_empty_and_overlong_values;
    case "accepts rejects invalid quality" test_accepts_rejects_invalid_quality;
    case "accepts rejects q zero matches" test_accepts_rejects_q_zero_matches;
    case "accepts matches client wildcards" test_accepts_matches_client_wildcards;
    case
      "accepts only requires content type for declared body"
      test_accepts_only_requires_content_type_for_declared_body;
    case
      "logger sanitizes control characters in paths"
      test_logger_sanitizes_control_characters_in_paths;
    case
      "basic auth accepts case insensitive scheme"
      test_basic_auth_accepts_case_insensitive_scheme;
    case "basic auth ignores extra spaces" test_basic_auth_ignores_extra_spaces;
    case "basic auth preserves colons in password" test_basic_auth_preserves_colons_in_password;
    case "basic auth rejects invalid credentials" test_basic_auth_rejects_invalid_credentials;
    case "basic auth sanitizes realm header value" test_basic_auth_sanitizes_realm_header_value;
    case "body parser rejects oversized bodies" test_body_parser_rejects_oversized_bodies;
    case "body parser rejects invalid json" test_body_parser_rejects_invalid_json;
    case "body parser rejects json root arrays" test_body_parser_rejects_json_root_arrays;
    case
      "body parser accepts case insensitive json content type"
      test_body_parser_accepts_case_insensitive_json_content_type;
    case
      "body parser rejects multipart without boundary"
      test_body_parser_rejects_multipart_without_boundary;
    case "csrf generates raw hex tokens" test_csrf_generates_raw_hex_tokens;
    case
      "csrf masking roundtrips and uses unique masks"
      test_csrf_masking_roundtrips_and_uses_unique_masks;
    case "csrf rejects malformed masked tokens" test_csrf_rejects_malformed_masked_tokens;
    case "csrf secure equal checks full token" test_csrf_secure_equal_checks_full_token;
    case "session middleware installs session" test_session_middleware_installs_session;
    case "csrf requires session middleware" test_csrf_requires_session_middleware;
    case
      "http1 websocket accept matches rfc example"
      test_http1_websocket_accept_matches_rfc_example;
    case
      "http1 websocket upgrade accepts valid request"
      test_http1_websocket_upgrade_accepts_valid_request;
    case "http1 websocket upgrade rejects non get" test_http1_websocket_upgrade_rejects_non_get;
    case "http1 websocket upgrade rejects http10" test_http1_websocket_upgrade_rejects_http10;
    case
      "http1 websocket upgrade requires upgrade header"
      test_http1_websocket_upgrade_requires_upgrade_header;
    case
      "http1 websocket upgrade rejects invalid upgrade header"
      test_http1_websocket_upgrade_rejects_invalid_upgrade_header;
    case
      "http1 websocket upgrade requires connection token"
      test_http1_websocket_upgrade_requires_connection_token;
    case
      "http1 websocket upgrade requires version 13"
      test_http1_websocket_upgrade_requires_version_13;
    case
      "http1 websocket upgrade rejects invalid key"
      test_http1_websocket_upgrade_rejects_invalid_key;
    case "connection write all retries short writes" test_connection_write_all_retries_short_writes;
    case
      "connection write all treats zero write as closed"
      test_connection_write_all_treats_zero_write_as_closed;
    case "connection write all skips empty payload" test_connection_write_all_skips_empty_payload;
    case "connection send file slice extracts range" test_connection_send_file_slice_extracts_range;
    case
      "connection send file slice allows zero length"
      test_connection_send_file_slice_allows_zero_length;
    case
      "connection send file slice rejects invalid range"
      test_connection_send_file_slice_rejects_invalid_range;
    case
      "http1 response rejects invalid header name"
      test_http1_response_rejects_invalid_header_name;
    case "http1 response rejects header injection" test_http1_response_rejects_header_injection;
    case "http1 response omits body for no content" test_http1_response_omits_body_for_no_content;
    case
      "http1 response omits body for not modified"
      test_http1_response_omits_body_for_not_modified;
    case
      "http1 response omits body for informational status"
      test_http1_response_omits_body_for_informational_status;
    case
      "http1 response sets content length for body"
      test_http1_response_sets_content_length_for_body;
    case
      "http1 response does not add vary without compression"
      test_http1_response_does_not_add_vary_without_compression;
  ]

let main ~args = Test.Cli.main ~name:"suri_hardening_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
