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

let test_basic_auth_accepts_case_insensitive_scheme = fun _ctx ->
  let encoded = Encoding.Base64.encode "alice:s3cret" in
  Test.assert_equal
    ~expected:(Ok ("alice", "s3cret"))
    ~actual:(Basic_auth.decode_credentials ("bAsIc " ^ encoded));
  Ok ()

let test_basic_auth_ignores_extra_spaces = fun _ctx ->
  let encoded = Encoding.Base64.encode "alice:s3cret" in
  Test.assert_equal
    ~expected:(Ok ("alice", "s3cret"))
    ~actual:(Basic_auth.decode_credentials ("  Basic   " ^ encoded ^ "  "));
  Ok ()

let test_basic_auth_preserves_colons_in_password = fun _ctx ->
  let encoded = Encoding.Base64.encode "alice:s3:cr:et" in
  Test.assert_equal
    ~expected:(Ok ("alice", "s3:cr:et"))
    ~actual:(Basic_auth.decode_credentials ("Basic " ^ encoded));
  Ok ()

let test_basic_auth_rejects_invalid_credentials = fun _ctx ->
  Test.assert_equal
    ~expected:(Error Basic_auth.InvalidAuthorizationFormat)
    ~actual:(Basic_auth.decode_credentials "Bearer token");
  Test.assert_equal
    ~expected:(Error Basic_auth.InvalidBase64Credentials)
    ~actual:(Basic_auth.decode_credentials "Basic not-base64");
  Ok ()

let test_basic_auth_reports_missing_authorization_header = fun _ctx ->
  let conn =
    Testing.Conn.make ()
    |> Result.unwrap
  in
  Test.assert_equal
    ~expected:(Error Basic_auth.MissingAuthorizationHeader)
    ~actual:(Basic_auth.get_credentials_result conn);
  Test.assert_equal
    ~expected:(Error Basic_auth.InvalidAuthorizationFormat)
    ~actual:(Basic_auth.get_credentials conn);
  Ok ()

let test_basic_auth_reports_malformed_authorization_header = fun _ctx ->
  let conn =
    Testing.Conn.make ~headers:[ ("authorization", "Bearer token"); ] ()
    |> Result.unwrap
  in
  Test.assert_equal
    ~expected:(Error (Basic_auth.InvalidAuthorizationHeader Basic_auth.InvalidAuthorizationFormat))
    ~actual:(Basic_auth.get_credentials_result conn);
  Ok ()

let test_basic_auth_sanitizes_realm_header_value = fun _ctx ->
  Test.assert_equal ~expected:"AdminPanel" ~actual:(Basic_auth.sanitize_realm "Admin\r\n\"Panel");
  Ok ()

let auth_header = fun username password ->
  "Basic " ^ Encoding.Base64.encode (username ^ ":" ^ password)

let test_basic_auth_middleware_rejects_missing_credentials = fun _ctx ->
  let app = [
    Basic_auth.middleware ~realm:"Admin\r\n\"Panel" ~username:"alice" ~password:"s3cret" ();
    (
      fun ~conn ~next:_ ->
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:"ok"
        |> Conn.send
    );
  ]
  in
  match Testing.App.get app "/" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_equal ~expected:Net.Http.Status.Unauthorized ~actual:response.status;
      Test.assert_equal ~expected:"Unauthorized" ~actual:response.body;
      Test.assert_equal
        ~expected:(Some "Basic realm=\"AdminPanel\"")
        ~actual:(Net.Http.Header.get response.headers "www-authenticate");
      Ok ()

let test_basic_auth_middleware_allows_valid_credentials = fun _ctx ->
  let app = [
    Basic_auth.middleware ~username:"alice" ~password:"s3cret" ();
    (
      fun ~conn ~next:_ ->
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:"ok"
        |> Conn.send
    );
  ]
  in
  match Testing.App.get app ~headers:[ ("authorization", auth_header "alice" "s3cret"); ] "/" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_equal ~expected:Net.Http.Status.Ok ~actual:response.status;
      Test.assert_equal ~expected:"ok" ~actual:response.body;
      Ok ()

let test_basic_auth_validation_assigns_typed_user_data = fun _ctx ->
  let user_key = Basic_auth.key () in
  let validate = fun ~username ~password ->
    if String.equal username "alice" && String.equal password "s3cret" then
      Some ("user:" ^ username)
    else
      None
  in
  let app = [
    Basic_auth.middleware_with_validation ~assign_to:user_key ~validate ();
    (
      fun ~conn ~next:_ ->
        let body =
          Basic_auth.get user_key conn
          |> Option.unwrap_or ~default:"missing"
        in
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body
        |> Conn.send
    );
  ]
  in
  match Testing.App.get app ~headers:[ ("authorization", auth_header "alice" "s3cret"); ] "/" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_equal ~expected:Net.Http.Status.Ok ~actual:response.status;
      Test.assert_equal ~expected:"user:alice" ~actual:response.body;
      Ok ()

let test_basic_auth_validation_rejects_invalid_credentials = fun _ctx ->
  let validate = fun ~username:_ ~password:_ -> None in
  let app = [
    Basic_auth.middleware_with_validation ~validate ();
    (
      fun ~conn ~next:_ ->
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:"ok"
        |> Conn.send
    );
  ]
  in
  match Testing.App.get app ~headers:[ ("authorization", auth_header "alice" "wrong"); ] "/" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_equal ~expected:Net.Http.Status.Unauthorized ~actual:response.status;
      Test.assert_equal ~expected:"Unauthorized" ~actual:response.body;
      Ok ()

let tests =
  Test.[
    case
      "basic auth accepts case insensitive scheme"
      test_basic_auth_accepts_case_insensitive_scheme;
    case "basic auth ignores extra spaces" test_basic_auth_ignores_extra_spaces;
    case "basic auth preserves colons in password" test_basic_auth_preserves_colons_in_password;
    case "basic auth rejects invalid credentials" test_basic_auth_rejects_invalid_credentials;
    case
      "basic auth reports missing authorization header"
      test_basic_auth_reports_missing_authorization_header;
    case
      "basic auth reports malformed authorization header"
      test_basic_auth_reports_malformed_authorization_header;
    case "basic auth sanitizes realm header value" test_basic_auth_sanitizes_realm_header_value;
    case
      "basic auth middleware rejects missing credentials"
      test_basic_auth_middleware_rejects_missing_credentials;
    case
      "basic auth middleware allows valid credentials"
      test_basic_auth_middleware_allows_valid_credentials;
    case
      "basic auth validation assigns typed user data"
      test_basic_auth_validation_assigns_typed_user_data;
    case
      "basic auth validation rejects invalid credentials"
      test_basic_auth_validation_rejects_invalid_credentials;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-basic-auth" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
