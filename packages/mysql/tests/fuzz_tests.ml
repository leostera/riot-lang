open Std

module Test = Std.Test
module Protocol = Mysql.Internal.Protocol
module Buffer = Std.StringBuilder
module Value = Sqlx_driver.Value

let add_u8 = fun buffer value -> Buffer.add_char buffer (Char.from_int_unchecked (value land 0xff))

let add_int16_le = fun buffer value ->
  add_u8 buffer value;
  add_u8 buffer (value lsr 8)

let add_int32_le = fun buffer value ->
  add_u8 buffer value;
  add_u8 buffer (value lsr 8);
  add_u8 buffer (value lsr 16);
  add_u8 buffer (value lsr 24)

let add_int64_le = fun buffer value ->
  for shift = 0 to 7 do
    let byte =
      Int64.shift_right_logical value (shift * 8)
      |> Int64.logand 0xffL
      |> Int64.to_int
    in
    add_u8 buffer byte
  done

let add_cstring = fun buffer value ->
  Buffer.add_string buffer value;
  add_u8 buffer 0

let add_lenenc_string = fun buffer value ->
  add_u8 buffer (String.length value);
  Buffer.add_string buffer value

let accept_parse = fun _ -> Ok ()

let packet_frame = fun payload ->
  match Protocol.Writer.packet ~sequence:0 ~payload with
  | frame :: _ -> frame
  | [] -> ""

let handshake_seed = fun () ->
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
  Buffer.contents payload

let column_definition_seed = fun () ->
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
  Buffer.contents payload

let prepare_ok_seed = fun () ->
  let payload = Buffer.create ~size:16 in
  add_u8 payload 0x00;
  add_int32_le payload 99;
  add_int16_le payload 2;
  add_int16_le payload 3;
  add_u8 payload 0x00;
  add_int16_le payload 0;
  Buffer.contents payload

let text_row_seed = fun () ->
  let payload = Buffer.create ~size:160 in
  add_lenenc_string payload "42";
  add_lenenc_string payload "Ada";
  add_lenenc_string payload "2026-05-10";
  add_lenenc_string payload "12:34:56.123456";
  add_lenenc_string payload "2026-05-10 12:34:56";
  add_lenenc_string payload "{\"ok\":true}";
  add_lenenc_string payload "123.456";
  add_u8 payload 0xfb;
  Buffer.contents payload

let binary_row_seed = fun () ->
  let payload = Buffer.create ~size:160 in
  add_u8 payload 0x00;
  add_u8 payload 0x00;
  add_u8 payload 0x00;
  add_u8 payload 42;
  add_int16_le payload 7;
  add_int32_le payload 123_456;
  add_int64_le payload 9_876_543_210L;
  add_lenenc_string payload "Ada";
  add_u8 payload 4;
  add_int16_le payload 2_026;
  add_u8 payload 5;
  add_u8 payload 10;
  add_u8 payload 12;
  add_u8 payload 0;
  add_int32_le payload 1;
  add_u8 payload 2;
  add_u8 payload 3;
  add_u8 payload 4;
  add_lenenc_string payload "123.456";
  add_lenenc_string payload "{\"ok\":true}";
  Buffer.contents payload

let binary_null_row_seed = "\x00\xfc\x0f"

let oversized_lenenc_seed = "\xfe\xff\xff\xff\xff\xff\xff\xff\xff"

let column = fun ?(flags = 0) name column_type ->
  Protocol.{
    schema = "app";
    table = "t";
    org_table = "t";
    name;
    org_name = name;
    character_set = 45;
    column_length = 255;
    column_type;
    flags;
    decimals = 0;
  }

let text_columns = [
  column "id" Protocol.ColumnType.Long;
  column "name" Protocol.ColumnType.VarString;
  column "d" Protocol.ColumnType.Date;
  column "t" Protocol.ColumnType.Time;
  column "ts" Protocol.ColumnType.DateTime;
  column "meta" Protocol.ColumnType.Json;
  column "amount" Protocol.ColumnType.NewDecimal;
  column "missing" Protocol.ColumnType.String;
]

let binary_columns = [
  column "tiny" Protocol.ColumnType.Tiny;
  column "short" Protocol.ColumnType.Short;
  column "long" Protocol.ColumnType.Long;
  column "longlong" Protocol.ColumnType.LongLong;
  column "name" Protocol.ColumnType.VarString;
  column "d" Protocol.ColumnType.Date;
  column "tm" Protocol.ColumnType.Time;
  column "amount" Protocol.ColumnType.NewDecimal;
  column "meta" Protocol.ColumnType.Json;
  column "blob" Protocol.ColumnType.Blob;
]

let byte_mutator =
  Test.Fuzz.Mutator.(bytes
  |> with_max_len 2_048
  |> with_dictionary
    [
      "\x00";
      "\xff";
      "\xfb";
      "\xfc";
      "\xfd";
      "\xfe";
      oversized_lenenc_seed;
      "mysql_native_password";
      "caching_sha2_password";
      "SELECT ?";
    ])

let text_mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 1_024
  |> with_dictionary
    [
      "mysql://";
      "mysql://user:pass@localhost:3306/db";
      "localhost:3306:db:user:pass";
      "@";
      ":";
      "[::1]";
      "%2F";
      "?ssl-mode=require";
    ])

let test_packet_decoder_fuzz = fun _ctx input ->
  Protocol.Packet.decode_one input
  |> accept_parse

let test_handshake_parser_fuzz = fun _ctx input ->
  Protocol.Reader.parse_handshake input
  |> accept_parse

let test_ok_packet_parser_fuzz = fun _ctx input ->
  Protocol.Reader.parse_ok_packet input
  |> accept_parse

let test_error_packet_parser_fuzz = fun _ctx input ->
  Protocol.Reader.parse_error_packet input
  |> accept_parse

let test_column_definition_parser_fuzz = fun _ctx input ->
  Protocol.Reader.parse_column_definition input
  |> accept_parse

let test_prepare_ok_parser_fuzz = fun _ctx input ->
  Protocol.Reader.parse_prepare_ok input
  |> accept_parse

let test_text_row_parser_fuzz = fun _ctx input ->
  Protocol.Reader.parse_text_row text_columns input
  |> accept_parse

let test_binary_row_parser_fuzz = fun _ctx input ->
  Protocol.Reader.parse_binary_row binary_columns input
  |> accept_parse

let test_statement_execute_encoder_fuzz = fun _ctx input ->
  let _payload =
    Protocol.Writer.com_stmt_execute
      ~statement_id:42
      ~params:[
        Value.string input;
        Value.bytes (IO.Bytes.from_string input);
        Value.json input;
        Value.numeric input;
      ]
  in
  Ok ()

let test_connection_string_parser_fuzz = fun _ctx input ->
  Mysql.Config.from_string input
  |> accept_parse

let tests =
  Test.[
    fuzz
      "packet decoder accepts arbitrary frames"
      ~seeds:[ ""; "\x00\x00\x00\x00"; "\x01\x00\x00\xffx"; packet_frame ""; packet_frame "hello"; ]
      ~mutator:byte_mutator
      test_packet_decoder_fuzz;
    fuzz
      "handshake parser accepts arbitrary payloads"
      ~seeds:[ ""; "\x0a"; handshake_seed (); "10.0.0\x00"; ]
      ~mutator:byte_mutator
      test_handshake_parser_fuzz;
    fuzz
      "ok packet parser accepts arbitrary payloads"
      ~seeds:[
        "";
        "\x00\x00\x00\x02\x00\x00\x00";
        "\xfe\x00\x00\x02\x00\x00\x00";
        oversized_lenenc_seed;
      ]
      ~mutator:byte_mutator
      test_ok_packet_parser_fuzz;
    fuzz
      "error packet parser accepts arbitrary payloads"
      ~seeds:[ ""; "\xff\x28\x04#42000syntax error"; "\x00not error"; ]
      ~mutator:byte_mutator
      test_error_packet_parser_fuzz;
    fuzz
      "column definition parser accepts arbitrary payloads"
      ~seeds:[ ""; column_definition_seed (); oversized_lenenc_seed; ]
      ~mutator:byte_mutator
      test_column_definition_parser_fuzz;
    fuzz
      "prepare ok parser accepts arbitrary payloads"
      ~seeds:[ ""; prepare_ok_seed (); "\x00\x01"; "\xff"; ]
      ~mutator:byte_mutator
      test_prepare_ok_parser_fuzz;
    fuzz
      "text row parser accepts arbitrary payloads"
      ~seeds:[ ""; text_row_seed (); oversized_lenenc_seed; "\xfb\xfb\xfb\xfb\xfb\xfb\xfb\xfb"; ]
      ~mutator:byte_mutator
      test_text_row_parser_fuzz;
    fuzz
      "binary row parser accepts arbitrary payloads"
      ~seeds:[
        "";
        binary_row_seed ();
        binary_null_row_seed;
        "\x00";
        "\xff";
        oversized_lenenc_seed;
      ]
      ~mutator:byte_mutator
      test_binary_row_parser_fuzz;
    fuzz
      "statement execute encoder accepts arbitrary parameter text"
      ~seeds:[ ""; "Ada"; "\x00\xff"; String.make ~len:512 ~char:'x'; ]
      ~mutator:text_mutator
      test_statement_execute_encoder_fuzz;
    fuzz
      "connection string parser accepts arbitrary text"
      ~seeds:[
        "";
        "mysql://riot:riot@localhost:3306/riot_test";
        "localhost:3306:riot_test:riot:riot";
        "mysql://user:pass@[::1]:3306/db";
        "mysql://user:pass@localhost:notaport/db";
      ]
      ~mutator:text_mutator
      test_connection_string_parser_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"mysql_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
