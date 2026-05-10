open Std

module Test = Std.Test
module Buffer = Std.StringBuilder
module Bytes = Std.IO.Bytes
module Char = Std.Char
module Int = Std.Int
module List = Std.List
module String = Std.String
module Protocol = Postgres.Internal.Protocol

let byte_at = fun text index -> Char.code (String.get_unchecked text ~at:index)

let int16_at = fun text offset -> (byte_at text offset lsl 8) lor byte_at text (offset + 1)

let int32_at = fun text offset ->
  let unsigned =
    (byte_at text offset lsl 24)
    lor (byte_at text (offset + 1) lsl 16)
    lor (byte_at text (offset + 2) lsl 8)
    lor byte_at text (offset + 3)
  in
  if unsigned >= 0x8000_0000 then
    unsigned - 0x1_0000_0000
  else
    unsigned

let add_int16 = fun buffer value ->
  Buffer.add_char buffer (Char.from_int_unchecked ((value lsr 8) land 0xff));
  Buffer.add_char buffer (Char.from_int_unchecked (value land 0xff))

let add_int32 = fun buffer value ->
  Buffer.add_char buffer (Char.from_int_unchecked ((value lsr 24) land 0xff));
  Buffer.add_char buffer (Char.from_int_unchecked ((value lsr 16) land 0xff));
  Buffer.add_char buffer (Char.from_int_unchecked ((value lsr 8) land 0xff));
  Buffer.add_char buffer (Char.from_int_unchecked (value land 0xff))

let add_cstring = fun buffer value ->
  Buffer.add_string buffer value;
  Buffer.add_char buffer '\x00'

let bytes_of_buffer = fun buffer ->
  Buffer.contents buffer
  |> Bytes.from_string

let test_writer_bind_message_encodes_null_params = fun _ctx ->
  let message =
    Protocol.Writer.bind_message
      ~portal_name:""
      ~statement_name:"stmt"
      ~params:[ Some "abc"; None; Some "" ]
  in
  Test.assert_equal ~expected:(Char.code 'B') ~actual:(byte_at message 0);
  Test.assert_equal ~expected:(String.length message - 1) ~actual:(int32_at message 1);
  Test.assert_equal ~expected:0 ~actual:(byte_at message 5);
  Test.assert_equal ~expected:"stmt" ~actual:(String.sub message ~offset:6 ~len:4);
  Test.assert_equal ~expected:0 ~actual:(byte_at message 10);
  Test.assert_equal ~expected:0 ~actual:(int16_at message 11);
  Test.assert_equal ~expected:3 ~actual:(int16_at message 13);
  Test.assert_equal ~expected:3 ~actual:(int32_at message 15);
  Test.assert_equal ~expected:"abc" ~actual:(String.sub message ~offset:19 ~len:3);
  Test.assert_equal ~expected:(-1) ~actual:(int32_at message 22);
  Test.assert_equal ~expected:0 ~actual:(int32_at message 26);
  Test.assert_equal ~expected:0 ~actual:(int16_at message 30);
  Ok ()

let test_reader_rejects_truncated_authentication = fun _ctx ->
  match Protocol.Reader.parse_backend_message_result
    (Char.code 'R')
    6
    (Bytes.from_string "\x00\x00") with
  | Error error ->
      let rendered = Protocol.Reader.parse_error_to_string error in
      Test.assert_true (String.contains rendered "auth_type");
      Ok ()
  | Ok _ -> Error "expected truncated authentication message to fail"

let test_reader_rejects_unknown_message_type = fun _ctx ->
  match Protocol.Reader.parse_backend_message_result (Char.code '?') 4 (Bytes.from_string "") with
  | Error error ->
      let rendered = Protocol.Reader.parse_error_to_string error in
      Test.assert_true (String.contains rendered "unknown message type");
      Ok ()
  | Ok _ -> Error "expected unknown backend message to fail"

let test_reader_rejects_length_mismatch = fun _ctx ->
  match Protocol.Reader.parse_backend_message_result (Char.code 'Z') 99 (Bytes.from_string "I") with
  | Error error ->
      let rendered = Protocol.Reader.parse_error_to_string error in
      Test.assert_true (String.contains rendered "length");
      Ok ()
  | Ok _ -> Error "expected mismatched frame length to fail"

let test_reader_rejects_unterminated_error_response = fun _ctx ->
  let payload = Buffer.create ~size:32 in
  Buffer.add_char payload 'S';
  add_cstring payload "ERROR";
  Buffer.add_char payload 'M';
  add_cstring payload "missing terminator";
  match Protocol.Reader.parse_backend_message_result
    (Char.code 'E')
    (Buffer.length payload + 4)
    (bytes_of_buffer payload) with
  | Error error ->
      let rendered = Protocol.Reader.parse_error_to_string error in
      Test.assert_true (String.contains rendered "terminator");
      Ok ()
  | Ok _ -> Error "expected unterminated error response to fail"

let test_reader_rejects_invalid_negative_data_row_length = fun _ctx ->
  let payload = Buffer.create ~size:16 in
  add_int16 payload 1;
  add_int32 payload (-2);
  match Protocol.Reader.parse_backend_message_result
    (Char.code 'D')
    (Buffer.length payload + 4)
    (bytes_of_buffer payload) with
  | Error error ->
      let rendered = Protocol.Reader.parse_error_to_string error in
      Test.assert_true (String.contains rendered "negative column length");
      Ok ()
  | Ok _ -> Error "expected invalid negative data row length to fail"

let test_reader_parses_error_response_fields = fun _ctx ->
  let payload = Buffer.create ~size:64 in
  Buffer.add_char payload 'S';
  add_cstring payload "ERROR";
  Buffer.add_char payload 'C';
  add_cstring payload "23505";
  Buffer.add_char payload 'M';
  add_cstring payload "duplicate key value violates unique constraint";
  Buffer.add_char payload 'P';
  add_cstring payload "12";
  Buffer.add_char payload '\x00';
  match Protocol.Reader.parse_backend_message_result
    (Char.code 'E')
    (Buffer.length payload + 4)
    (bytes_of_buffer payload) with
  | Ok (Protocol.ErrorResponse error) ->
      Test.assert_equal ~expected:(Some "ERROR") ~actual:(Protocol.Error.severity error);
      Test.assert_equal
        ~expected:(Some Protocol.Sqlstate.UniqueViolation)
        ~actual:(Protocol.Error.sqlstate error);
      Test.assert_equal
        ~expected:"duplicate key value violates unique constraint"
        ~actual:(Protocol.Error.message error);
      Test.assert_equal ~expected:(Some 12) ~actual:(Protocol.Error.position error);
      Ok ()
  | Ok _ -> Error "expected ErrorResponse"
  | Error error -> Error (Protocol.Reader.parse_error_to_string error)

let test_reader_parses_row_description_and_data_row = fun _ctx ->
  let description = Buffer.create ~size:128 in
  add_int16 description 2;
  add_cstring description "id";
  add_int32 description 0;
  add_int16 description 0;
  add_int32 description 23;
  add_int16 description 4;
  add_int32 description (-1);
  add_int16 description 0;
  add_cstring description "name";
  add_int32 description 0;
  add_int16 description 0;
  add_int32 description 25;
  add_int16 description (-1);
  add_int32 description (-1);
  add_int16 description 0;
  let data = Buffer.create ~size:32 in
  add_int16 data 2;
  add_int32 data 2;
  Buffer.add_string data "42";
  add_int32 data (-1);
  match Protocol.Reader.parse_backend_message_result
    (Char.code 'T')
    (Buffer.length description + 4)
    (bytes_of_buffer description) with
  | Error error -> Error (Protocol.Reader.parse_error_to_string error)
  | Ok (Protocol.RowDescription fields) ->
      Test.assert_equal ~expected:2 ~actual:(List.length fields);
      match Protocol.Reader.parse_backend_message_result
        (Char.code 'D')
        (Buffer.length data + 4)
        (bytes_of_buffer data) with
      | Error error -> Error (Protocol.Reader.parse_error_to_string error)
      | Ok (Protocol.DataRow [ Protocol.Row.Value "42"; Protocol.Row.Null ]) -> Ok ()
      | Ok _ -> Error "expected data row with one value and one null"
  | Ok _ -> Error "expected RowDescription"

let test_config_parses_uri_and_legacy_forms = fun _ctx ->
  match Postgres.Config.from_string "postgresql://alice:secret@localhost:5433/app" with
  | Error error -> Error error
  | Ok uri_config ->
      Test.assert_equal ~expected:5_433 ~actual:uri_config.port;
      Test.assert_equal ~expected:"app" ~actual:uri_config.database;
      Test.assert_equal ~expected:"alice" ~actual:uri_config.user;
      Test.assert_equal ~expected:"secret" ~actual:uri_config.password;
      match Postgres.Config.from_string "db.internal:5432:prod:bob:s3cr3t" with
      | Error error -> Error error
      | Ok legacy_config ->
          Test.assert_equal ~expected:"db.internal" ~actual:legacy_config.host;
          Test.assert_equal ~expected:5_432 ~actual:legacy_config.port;
          Test.assert_equal ~expected:"prod" ~actual:legacy_config.database;
          Test.assert_equal ~expected:"bob" ~actual:legacy_config.user;
          Test.assert_equal ~expected:"s3cr3t" ~actual:legacy_config.password;
          Ok ()

let test_driver_rejects_required_tls_without_plaintext = fun _ctx ->
  let config = { (Postgres.Config.default ()) with ssl_mode = Postgres.Config.Require } in
  match Postgres.Driver.connect config with
  | Error error ->
      Test.assert_true (String.contains (Postgres.Driver.error_to_string error) "TLS");
      Ok ()
  | Ok connection ->
      Postgres.Driver.close connection;
      Error "expected ssl_mode=require to fail until TLS negotiation is implemented"

let tests =
  Test.[
    case "writer bind message encodes null params" test_writer_bind_message_encodes_null_params;
    case "reader rejects truncated authentication" test_reader_rejects_truncated_authentication;
    case "reader rejects unknown message type" test_reader_rejects_unknown_message_type;
    case "reader rejects length mismatch" test_reader_rejects_length_mismatch;
    case
      "reader rejects unterminated error response"
      test_reader_rejects_unterminated_error_response;
    case
      "reader rejects invalid negative data row length"
      test_reader_rejects_invalid_negative_data_row_length;
    case "reader parses error response fields" test_reader_parses_error_response_fields;
    case
      "reader parses row description and data row"
      test_reader_parses_row_description_and_data_row;
    case "config parses uri and legacy forms" test_config_parses_uri_and_legacy_forms;
    case
      "driver rejects required tls without plaintext"
      test_driver_rejects_required_tls_without_plaintext;
  ]

let main ~args = Test.Cli.main ~name:"postgres_protocol_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
