open Std

module Test = Std.Test
module Protocol = Mysql.Internal.Protocol
module Row = Sqlx_driver.Row
module Value = Sqlx_driver.Value
module Buffer = Std.StringBuilder

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

let add_lenenc_string = fun buffer value ->
  add_u8 buffer (String.length value);
  Buffer.add_string buffer value

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

let test_text_row_decodes_common_types = fun _ctx ->
  let columns = [
    column "id" Protocol.ColumnType.Long;
    column "email" Protocol.ColumnType.VarString;
    column "created_at" Protocol.ColumnType.DateTime;
    column "meta" Protocol.ColumnType.Json;
    column "missing" Protocol.ColumnType.VarString;
  ]
  in
  let payload = Buffer.create ~size:128 in
  add_lenenc_string payload "42";
  add_lenenc_string payload "ada@example.com";
  add_lenenc_string payload "2026-05-10 12:34:56.123456";
  add_lenenc_string payload "{\"ok\":true}";
  add_u8 payload 0xfb;
  match Protocol.Reader.parse_text_row columns (Buffer.contents payload) with
  | Error error -> Error (Protocol.parse_error_to_string error)
  | Ok row ->
      Test.assert_equal ~expected:(Some 42) ~actual:(Row.int "id" row);
      Test.assert_equal ~expected:(Some "ada@example.com") ~actual:(Row.string "email" row);
      Test.assert_equal
        ~expected:(Some "{\"ok\":true}")
        ~actual:(
          match Row.get "meta" row with
          | Some value -> Value.to_json value
          | None -> None
        );
      Test.assert_true
        (
          match Row.get "created_at" row with
          | Some (Value.Timestamp value) ->
              value.year = 2_026
              && value.month = 5
              && value.day = 10
              && value.hour = 12
              && value.minute = 34
              && value.second = 56
          | _ -> false
        );
      Test.assert_true
        (
          match Row.get "missing" row with
          | Some Value.Null -> true
          | _ -> false
        );
      Ok ()

let test_binary_row_decodes_values_and_null_bitmap = fun _ctx ->
  let columns = [
    column "id" Protocol.ColumnType.LongLong;
    column "name" Protocol.ColumnType.VarString;
    column "active" Protocol.ColumnType.Tiny;
    column "note" Protocol.ColumnType.VarString;
  ]
  in
  let payload = Buffer.create ~size:64 in
  add_u8 payload 0x00;
  add_u8 payload 0x20;
  add_int64_le payload 7L;
  add_lenenc_string payload "Ada";
  add_u8 payload 1;
  match Protocol.Reader.parse_binary_row columns (Buffer.contents payload) with
  | Error error -> Error (Protocol.parse_error_to_string error)
  | Ok row ->
      Test.assert_equal
        ~expected:(Some 7L)
        ~actual:(
          match Row.get "id" row with
          | Some value -> Value.to_int64 value
          | None -> None
        );
      Test.assert_equal ~expected:(Some "Ada") ~actual:(Row.string "name" row);
      Test.assert_equal ~expected:(Some 1) ~actual:(Row.int "active" row);
      Test.assert_true
        (
          match Row.get "note" row with
          | Some Value.Null -> true
          | _ -> false
        );
      Ok ()

let test_binary_row_decodes_date_and_time = fun _ctx ->
  let columns = [ column "d" Protocol.ColumnType.Date; column "t" Protocol.ColumnType.Time; ] in
  let payload = Buffer.create ~size:64 in
  add_u8 payload 0x00;
  add_u8 payload 0x00;
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
  add_int32_le payload 500_000;
  match Protocol.Reader.parse_binary_row columns (Buffer.contents payload) with
  | Error error -> Error (Protocol.parse_error_to_string error)
  | Ok row ->
      Test.assert_equal
        ~expected:(Some (2_026, 5, 10))
        ~actual:(
          match Row.get "d" row with
          | Some value -> Value.to_date value
          | None -> None
        );
      Test.assert_equal
        ~expected:(Some (26, 3, 4, 500_000))
        ~actual:(
          match Row.get "t" row with
          | Some value -> Value.to_time value
          | None -> None
        );
      Ok ()

let test_binary_row_decodes_signed_integer_values = fun _ctx ->
  let columns = [
    column "tiny" Protocol.ColumnType.Tiny;
    column "short" Protocol.ColumnType.Short;
    column "long" Protocol.ColumnType.Long;
    column "longlong" Protocol.ColumnType.LongLong;
  ]
  in
  let payload = Buffer.create ~size:64 in
  add_u8 payload 0x00;
  add_u8 payload 0x00;
  add_u8 payload 0xff;
  add_int16_le payload 0xffff;
  add_int32_le payload 0xffff_ffff;
  add_int64_le payload (-1L);
  match Protocol.Reader.parse_binary_row columns (Buffer.contents payload) with
  | Error error -> Error (Protocol.parse_error_to_string error)
  | Ok row ->
      Test.assert_equal ~expected:(Some (-1)) ~actual:(Row.int "tiny" row);
      Test.assert_equal ~expected:(Some (-1)) ~actual:(Row.int "short" row);
      Test.assert_equal ~expected:(Some (-1)) ~actual:(Row.int "long" row);
      Test.assert_equal
        ~expected:(Some (-1L))
        ~actual:(
          match Row.get "longlong" row with
          | Some value -> Value.to_int64 value
          | None -> None
        );
      Ok ()

let test_binary_row_decodes_unsigned_integer_values = fun _ctx ->
  let unsigned_flag = 0x20 in
  let columns = [
    column ~flags:unsigned_flag "tiny" Protocol.ColumnType.Tiny;
    column ~flags:unsigned_flag "short" Protocol.ColumnType.Short;
    column ~flags:unsigned_flag "long" Protocol.ColumnType.Long;
    column ~flags:unsigned_flag "longlong" Protocol.ColumnType.LongLong;
  ]
  in
  let payload = Buffer.create ~size:64 in
  add_u8 payload 0x00;
  add_u8 payload 0x00;
  add_u8 payload 0xff;
  add_int16_le payload 0xffff;
  add_int32_le payload 0xffff_ffff;
  add_int64_le payload (-1L);
  match Protocol.Reader.parse_binary_row columns (Buffer.contents payload) with
  | Error error -> Error (Protocol.parse_error_to_string error)
  | Ok row ->
      Test.assert_equal ~expected:(Some 255) ~actual:(Row.int "tiny" row);
      Test.assert_equal ~expected:(Some 65_535) ~actual:(Row.int "short" row);
      Test.assert_equal ~expected:(Some 4_294_967_295) ~actual:(Row.int "long" row);
      Test.assert_equal
        ~expected:(Some "18446744073709551615")
        ~actual:(
          match Row.get "longlong" row with
          | Some value -> Value.to_numeric value
          | None -> None
        );
      Ok ()

let test_ok_packet_decodes_affected_rows = fun _ctx ->
  let payload = "\x00\x02\x01\x02\x00\x00\x00ok" in
  match Protocol.Reader.parse_ok_packet payload with
  | Error error -> Error (Protocol.parse_error_to_string error)
  | Ok ok ->
      Test.assert_equal ~expected:2L ~actual:ok.affected_rows;
      Test.assert_equal ~expected:1L ~actual:ok.last_insert_id;
      Test.assert_true ok.status.autocommit;
      Test.assert_equal ~expected:"ok" ~actual:ok.info;
      Ok ()

let tests =
  Test.[
    case "text row decodes common types" test_text_row_decodes_common_types;
    case "binary row decodes values and null bitmap" test_binary_row_decodes_values_and_null_bitmap;
    case "binary row decodes date and time" test_binary_row_decodes_date_and_time;
    case "binary row decodes signed integer values" test_binary_row_decodes_signed_integer_values;
    case
      "binary row decodes unsigned integer values"
      test_binary_row_decodes_unsigned_integer_values;
    case "ok packet decodes affected rows" test_ok_packet_decodes_affected_rows;
  ]

let main ~args = Test.Cli.main ~name:"mysql_result_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
