open Std
open Sqlx

let test_value_conversions = fun _ctx ->
  let int_val = Value.int 42 in
  Test.assert_equal ~expected:(Some 42) ~actual:(Value.to_int int_val);
  Test.assert_equal ~expected:None ~actual:(Value.to_string_value int_val);
  Test.assert_false (Value.is_null int_val);
  let str_val = Value.string "hello" in
  Test.assert_equal ~expected:(Some "hello") ~actual:(Value.to_string_value str_val);
  Test.assert_equal ~expected:None ~actual:(Value.to_int str_val);
  let null_val = Value.null in
  Test.assert_true (Value.is_null null_val);
  Test.assert_equal ~expected:None ~actual:(Value.to_int null_val);
  Test.assert_equal ~expected:None ~actual:(Value.to_string_value null_val);
  Ok ()

let test_row_access = fun _ctx ->
  let row = [ ("id", Value.int 1); ("name", Value.string "Alice"); ("active", Value.bool true) ] in
  Test.assert_equal ~expected:(Some (Value.int 1)) ~actual:(Row.get "id" row);
  Test.assert_equal ~expected:(Some 1) ~actual:(Row.int "id" row);
  Test.assert_equal ~expected:(Some "Alice") ~actual:(Row.string "name" row);
  Test.assert_equal ~expected:(Some true) ~actual:(Row.bool "active" row);
  Test.assert_equal ~expected:None ~actual:(Row.get "missing" row);
  let fields = Row.fields row in
  Test.assert_equal ~expected:3 ~actual:(List.length fields);
  Test.assert_true (List.contains fields ~value:"id");
  Test.assert_true (List.contains fields ~value:"name");
  Test.assert_true (List.contains fields ~value:"active");
  Ok ()

let test_value_equality = fun _ctx ->
  Test.assert_true (Value.equal (Value.int 42) (Value.int 42));
  Test.assert_false (Value.equal (Value.int 42) (Value.int 43));
  Test.assert_true (Value.equal (Value.string "test") (Value.string "test"));
  Test.assert_true (Value.equal Value.null Value.null);
  Test.assert_false (Value.equal Value.null (Value.int 0));
  Ok ()

let test_value_comparison = fun _ctx ->
  Test.assert_equal ~expected:Order.LT ~actual:(Value.compare (Value.int 1) (Value.int 2));
  Test.assert_equal ~expected:Order.GT ~actual:(Value.compare (Value.int 2) (Value.int 1));
  Test.assert_equal ~expected:Order.EQ ~actual:(Value.compare (Value.int 1) (Value.int 1));
  Test.assert_equal ~expected:Order.LT ~actual:(Value.compare Value.null (Value.int 1));
  Test.assert_equal ~expected:Order.GT ~actual:(Value.compare (Value.int 1) Value.null);
  Ok ()

let tests =
  Test.[
    case "value conversions" test_value_conversions;
    case "row access" test_row_access;
    case "value equality" test_value_equality;
    case "value comparison" test_value_comparison;
  ]

let main ~args = Test.Cli.main ~name:"sqlx_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
