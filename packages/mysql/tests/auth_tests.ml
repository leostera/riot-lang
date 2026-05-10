open Std

module Test = Std.Test
module Protocol = Mysql.Internal.Protocol
module Buffer = Std.StringBuilder

let hex = fun text ->
  let digits = "0123456789abcdef" in
  let buffer = Buffer.create ~size:(String.length text * 2) in
  String.for_each
    text
    ~fn:(fun char ->
      let value = Char.code char in
      Buffer.add_char buffer (String.get_unchecked digits ~at:(value lsr 4));
      Buffer.add_char buffer (String.get_unchecked digits ~at:(value land 0x0f)));
  Buffer.contents buffer

let test_mysql_native_password_matches_reference_vector = fun _ctx ->
  let scramble =
    Protocol.Auth.mysql_native_password ~password:"secret" ~seed:"12345678901234567890"
  in
  Test.assert_equal ~expected:"0f8b9033e0897c0a8338ebe3dea9010dda47ab56" ~actual:(hex scramble);
  Ok ()

let test_caching_sha2_password_matches_reference_vector = fun _ctx ->
  let scramble =
    Protocol.Auth.caching_sha2_password ~password:"secret" ~seed:"12345678901234567890"
  in
  Test.assert_equal
    ~expected:"51ecd6dedbd34d5445c0a190d4f51acf0d23b94db66c91f3f789faa9193751cd"
    ~actual:(hex scramble);
  Ok ()

let test_empty_password_uses_empty_auth_response = fun _ctx ->
  Test.assert_equal
    ~expected:""
    ~actual:(Protocol.Auth.mysql_native_password ~password:"" ~seed:"seed");
  Test.assert_equal
    ~expected:""
    ~actual:(Protocol.Auth.caching_sha2_password ~password:"" ~seed:"seed");
  Ok ()

let test_driver_required_tls_reports_configuration_error = fun _ctx ->
  let config = {
    (Mysql.Config.default ()) with
    host = "127.0.0.1";
    port = 1;
    ssl_mode = Mysql.Config.Require;
  }
  in
  match Mysql.Driver.connect config with
  | Error error ->
      let rendered = Mysql.Driver.error_to_string error in
      Test.assert_true (String.contains rendered "Transport" || String.contains rendered "TLS");
      Ok ()
  | Ok connection ->
      Mysql.Driver.close connection;
      Error "expected connection attempt to fail"

let test_driver_rejects_keepalive_configuration = fun _ctx ->
  let config = {
    (Mysql.Config.default ()) with
    keepalives_idle = Some (Time.Duration.from_secs 30);
  }
  in
  match Mysql.Driver.connect config with
  | Error error ->
      Test.assert_true (String.contains (Mysql.Driver.error_to_string error) "keepalive");
      Ok ()
  | Ok connection ->
      Mysql.Driver.close connection;
      Error "expected keepalive configuration to be rejected"

let tests =
  Test.[
    case
      "mysql_native_password matches reference vector"
      test_mysql_native_password_matches_reference_vector;
    case
      "caching_sha2_password matches reference vector"
      test_caching_sha2_password_matches_reference_vector;
    case "empty password uses empty auth response" test_empty_password_uses_empty_auth_response;
    case
      "driver required tls reports configuration error"
      test_driver_required_tls_reports_configuration_error;
    case "driver rejects keepalive configuration" test_driver_rejects_keepalive_configuration;
  ]

let main ~args = Test.Cli.main ~name:"mysql_auth_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
