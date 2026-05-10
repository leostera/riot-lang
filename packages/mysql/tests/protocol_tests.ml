open Std

module Test = Std.Test
module Protocol = Mysql.Internal.Protocol
module Buffer = Std.StringBuilder

let byte_at = fun text index -> Char.code (String.get_unchecked text ~at:index)

let int24_le_at = fun text offset ->
  byte_at text offset lor (byte_at text (offset + 1) lsl 8) lor (byte_at text (offset + 2) lsl 16)

let int32_le_at = fun text offset ->
  byte_at text offset
  lor (byte_at text (offset + 1) lsl 8)
  lor (byte_at text (offset + 2) lsl 16)
  lor (byte_at text (offset + 3) lsl 24)

let add_u8 = fun buffer value -> Buffer.add_char buffer (Char.from_int_unchecked (value land 0xff))

let add_int16_le = fun buffer value ->
  add_u8 buffer value;
  add_u8 buffer (value lsr 8)

let add_int32_le = fun buffer value ->
  add_u8 buffer value;
  add_u8 buffer (value lsr 8);
  add_u8 buffer (value lsr 16);
  add_u8 buffer (value lsr 24)

let add_cstring = fun buffer value ->
  Buffer.add_string buffer value;
  add_u8 buffer 0

let add_lenenc_string = fun buffer value ->
  add_u8 buffer (String.length value);
  Buffer.add_string buffer value

let test_packet_roundtrips_header_and_payload = fun _ctx ->
  let payload = "hello" in
  let frames = Protocol.Writer.packet ~sequence:7 ~payload in
  match frames with
  | [ frame ] -> (
      Test.assert_equal ~expected:5 ~actual:(int24_le_at frame 0);
      Test.assert_equal ~expected:7 ~actual:(byte_at frame 3);
      match Protocol.Packet.decode_one frame with
      | Error error -> Error (Protocol.parse_error_to_string error)
      | Ok packet ->
          Test.assert_equal ~expected:7 ~actual:packet.sequence;
          Test.assert_equal ~expected:payload ~actual:packet.payload;
          Ok ()
    )
  | _ -> Error "expected one packet frame"

let test_reader_parses_handshake_v10 = fun _ctx ->
  let payload = Buffer.create ~size:128 in
  add_u8 payload 10;
  add_cstring payload "8.0.36";
  add_int32_le payload 1_234;
  Buffer.add_string payload "abcdefgh";
  add_u8 payload 0;
  add_int16_le payload (Protocol.Capability.protocol_41 lor Protocol.Capability.secure_connection);
  add_u8 payload 45;
  add_int16_le payload 2;
  add_int16_le
    payload
    ((Protocol.Capability.plugin_auth lor Protocol.Capability.plugin_auth_lenenc_client_data) lsr 16);
  add_u8 payload 21;
  Buffer.add_string payload (String.make ~len:10 ~char:'\x00');
  Buffer.add_string payload "ijklmnopqrst";
  add_u8 payload 0;
  add_cstring payload "caching_sha2_password";
  match Protocol.Reader.parse_handshake (Buffer.contents payload) with
  | Error error -> Error (Protocol.parse_error_to_string error)
  | Ok handshake ->
      Test.assert_equal ~expected:10 ~actual:handshake.protocol_version;
      Test.assert_equal ~expected:"8.0.36" ~actual:handshake.server_version;
      Test.assert_equal ~expected:1_234 ~actual:handshake.connection_id;
      Test.assert_equal ~expected:45 ~actual:handshake.character_set;
      Test.assert_equal ~expected:(Some "caching_sha2_password") ~actual:handshake.auth_plugin_name;
      Test.assert_true
        (Protocol.Capability.has
          handshake.capability_flags
          Protocol.Capability.plugin_auth_lenenc_client_data);
      Ok ()

let test_reader_parses_error_packet = fun _ctx ->
  let payload = Buffer.create ~size:64 in
  add_u8 payload 0xff;
  add_int16_le payload 1_064;
  Buffer.add_string payload "#42000";
  Buffer.add_string payload "syntax error";
  match Protocol.Reader.parse_error_packet (Buffer.contents payload) with
  | Error error -> Error (Protocol.parse_error_to_string error)
  | Ok error ->
      Test.assert_equal ~expected:1_064 ~actual:error.code;
      Test.assert_equal ~expected:(Some "42000") ~actual:error.sql_state;
      Test.assert_equal ~expected:"syntax error" ~actual:error.message;
      Ok ()

let test_error_packet_serde_json = fun _ctx ->
  let error: Protocol.Error.t = {
    code = 1_064;
    sql_state = Some "42000";
    message = "syntax error";
  }
  in
  match Protocol.Error.to_json_string error with
  | Error error -> Error (Serde.Error.to_string error)
  | Ok encoded ->
      Test.assert_equal
        ~expected:{|{"type":"mysql_error","code":1064,"sql_state":"42000","message":"syntax error"}|}
        ~actual:encoded;
      Ok ()

let test_reader_parses_column_definition = fun _ctx ->
  let payload = Buffer.create ~size:96 in
  add_lenenc_string payload "def";
  add_lenenc_string payload "app";
  add_lenenc_string payload "users";
  add_lenenc_string payload "users";
  add_lenenc_string payload "id";
  add_lenenc_string payload "id";
  add_u8 payload 0x0c;
  add_int16_le payload 45;
  add_int32_le payload 20;
  add_u8 payload (Protocol.ColumnType.to_int Protocol.ColumnType.LongLong);
  add_int16_le payload 0;
  add_u8 payload 0;
  add_int16_le payload 0;
  match Protocol.Reader.parse_column_definition (Buffer.contents payload) with
  | Error error -> Error (Protocol.parse_error_to_string error)
  | Ok column ->
      Test.assert_equal ~expected:"app" ~actual:column.schema;
      Test.assert_equal ~expected:"users" ~actual:column.table;
      Test.assert_equal ~expected:"id" ~actual:column.name;
      Test.assert_equal ~expected:Protocol.ColumnType.LongLong ~actual:column.column_type;
      Test.assert_equal ~expected:20 ~actual:column.column_length;
      Ok ()

let test_config_parses_uri_and_legacy_forms = fun _ctx ->
  match Mysql.Config.from_string "mysql://alice:secret@localhost:3307/app" with
  | Error error -> Error (Mysql.Config.parse_error_to_string error)
  | Ok uri_config ->
      Test.assert_equal ~expected:"localhost" ~actual:uri_config.host;
      Test.assert_equal ~expected:3_307 ~actual:uri_config.port;
      Test.assert_equal ~expected:(Some "app") ~actual:uri_config.database;
      Test.assert_equal ~expected:"alice" ~actual:uri_config.user;
      Test.assert_equal ~expected:"secret" ~actual:uri_config.password;
      match Mysql.Config.from_string "db.internal:3306:prod:bob:s3cr3t" with
      | Error error -> Error (Mysql.Config.parse_error_to_string error)
      | Ok legacy_config ->
          Test.assert_equal ~expected:"db.internal" ~actual:legacy_config.host;
          Test.assert_equal ~expected:3_306 ~actual:legacy_config.port;
          Test.assert_equal ~expected:(Some "prod") ~actual:legacy_config.database;
          Test.assert_equal ~expected:"bob" ~actual:legacy_config.user;
          Test.assert_equal ~expected:"s3cr3t" ~actual:legacy_config.password;
          Ok ()

let test_config_rejects_invalid_uri_port = fun _ctx ->
  match Mysql.Config.from_string "mysql://alice:secret@db.example:not_a_port/app" with
  | Ok _ -> Error "expected invalid URI port to be rejected"
  | Error (Mysql.Config.InvalidPortNumber value) ->
      Test.assert_equal ~expected:"not_a_port" ~actual:value;
      Ok ()
  | Error error -> Error (Mysql.Config.parse_error_to_string error)

let test_writer_handshake_response_uses_client_capabilities = fun _ctx ->
  let flags = Protocol.Capability.default_client ~database:true ~ssl:true () in
  let payload =
    Protocol.Writer.handshake_response
      ~capability_flags:flags
      ~max_packet_size:1_024
      ~character_set:45
      ~user:"alice"
      ~database:(Some "app")
      ~auth_response:"abc"
      ~auth_plugin:"mysql_native_password"
  in
  Test.assert_equal ~expected:flags ~actual:(int32_le_at payload 0);
  Test.assert_equal ~expected:1_024 ~actual:(int32_le_at payload 4);
  Test.assert_equal ~expected:45 ~actual:(byte_at payload 8);
  Test.assert_true (String.contains payload "alice");
  Test.assert_true (String.contains payload "app");
  Test.assert_true (String.contains payload "mysql_native_password");
  Ok ()

let tests =
  Test.[
    case "packet roundtrips header and payload" test_packet_roundtrips_header_and_payload;
    case "reader parses handshake v10" test_reader_parses_handshake_v10;
    case "reader parses error packet" test_reader_parses_error_packet;
    case "error packet serializes with serde json" test_error_packet_serde_json;
    case "reader parses column definition" test_reader_parses_column_definition;
    case "config parses uri and legacy forms" test_config_parses_uri_and_legacy_forms;
    case "config rejects invalid uri port" test_config_rejects_invalid_uri_port;
    case
      "writer handshake response uses client capabilities"
      test_writer_handshake_response_uses_client_capabilities;
  ]

let main ~args = Test.Cli.main ~name:"mysql_protocol_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
