open Std

module Test = Std.Test
module Protocol = Mysql.Internal.Protocol
module Value = Sqlx_driver.Value

let byte_at = fun text index -> Char.code (String.get_unchecked text ~at:index)

let int32_le_at = fun text offset ->
  byte_at text offset
  lor (byte_at text (offset + 1) lsl 8)
  lor (byte_at text (offset + 2) lsl 16)
  lor (byte_at text (offset + 3) lsl 24)

let int64_le_at = fun text offset ->
  let value = ref 0L in
  for index = 0 to 7 do
    value := Int64.logor
      !value
      (Int64.shift_left (Int64.from_int (byte_at text (offset + index))) (index * 8))
  done;
  !value

let test_com_query_encodes_command_byte = fun _ctx ->
  let payload = Protocol.Writer.com_query "SELECT 1" in
  Test.assert_equal ~expected:0x03 ~actual:(byte_at payload 0);
  Test.assert_equal
    ~expected:"SELECT 1"
    ~actual:(String.sub payload ~offset:1 ~len:(String.length payload - 1));
  Ok ()

let test_stmt_prepare_encodes_sql = fun _ctx ->
  let payload = Protocol.Writer.com_stmt_prepare "SELECT ?" in
  Test.assert_equal ~expected:0x16 ~actual:(byte_at payload 0);
  Test.assert_equal
    ~expected:"SELECT ?"
    ~actual:(String.sub payload ~offset:1 ~len:(String.length payload - 1));
  Ok ()

let test_stmt_execute_encodes_types_nulls_and_values = fun _ctx ->
  let payload =
    Protocol.Writer.com_stmt_execute
      ~statement_id:99
      ~params:[ Value.int64 7L; Value.string "Ada"; Value.null; Value.bool true; ]
  in
  Test.assert_equal ~expected:0x17 ~actual:(byte_at payload 0);
  Test.assert_equal ~expected:99 ~actual:(int32_le_at payload 1);
  Test.assert_equal ~expected:0 ~actual:(byte_at payload 5);
  Test.assert_equal ~expected:1 ~actual:(int32_le_at payload 6);
  Test.assert_equal ~expected:0x04 ~actual:(byte_at payload 10);
  Test.assert_equal ~expected:1 ~actual:(byte_at payload 11);
  Test.assert_equal
    ~expected:(Protocol.ColumnType.to_int Protocol.ColumnType.LongLong)
    ~actual:(byte_at payload 12);
  Test.assert_equal
    ~expected:(Protocol.ColumnType.to_int Protocol.ColumnType.VarString)
    ~actual:(byte_at payload 14);
  Test.assert_equal
    ~expected:(Protocol.ColumnType.to_int Protocol.ColumnType.Null)
    ~actual:(byte_at payload 16);
  Test.assert_equal
    ~expected:(Protocol.ColumnType.to_int Protocol.ColumnType.Tiny)
    ~actual:(byte_at payload 18);
  Test.assert_equal ~expected:7L ~actual:(int64_le_at payload 20);
  Test.assert_equal ~expected:3 ~actual:(byte_at payload 28);
  Test.assert_equal ~expected:"Ada" ~actual:(String.sub payload ~offset:29 ~len:3);
  Test.assert_equal ~expected:1 ~actual:(byte_at payload 32);
  Ok ()

let test_stmt_execute_without_params_omits_param_section = fun _ctx ->
  let payload = Protocol.Writer.com_stmt_execute ~statement_id:42 ~params:[] in
  Test.assert_equal ~expected:10 ~actual:(String.length payload);
  Test.assert_equal ~expected:0x17 ~actual:(byte_at payload 0);
  Test.assert_equal ~expected:42 ~actual:(int32_le_at payload 1);
  Ok ()

let test_stmt_close_encodes_statement_id = fun _ctx ->
  let payload = Protocol.Writer.com_stmt_close 42 in
  Test.assert_equal ~expected:5 ~actual:(String.length payload);
  Test.assert_equal ~expected:0x19 ~actual:(byte_at payload 0);
  Test.assert_equal ~expected:42 ~actual:(int32_le_at payload 1);
  Ok ()

let tests =
  Test.[
    case "com query encodes command byte" test_com_query_encodes_command_byte;
    case "stmt prepare encodes sql" test_stmt_prepare_encodes_sql;
    case
      "stmt execute encodes types nulls and values"
      test_stmt_execute_encodes_types_nulls_and_values;
    case
      "stmt execute without params omits param section"
      test_stmt_execute_without_params_omits_param_section;
    case "stmt close encodes statement id" test_stmt_close_encodes_statement_id;
  ]

let main ~args = Test.Cli.main ~name:"mysql_statement_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
