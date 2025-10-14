open Std
open Sqlx

let test_value_conversions () =
  Log.info "Testing value conversions...";

  let int_val = Value.int 42 in
  assert (Value.to_int int_val = Some 42);
  assert (Value.to_string int_val = None);
  assert (not (Value.is_null int_val));

  let str_val = Value.string "hello" in
  assert (Value.to_string str_val = Some "hello");
  assert (Value.to_int str_val = None);

  let null_val = Value.null in
  assert (Value.is_null null_val);
  assert (Value.to_int null_val = None);
  assert (Value.to_string null_val = None);

  Log.info "Value conversions: OK"

let test_row_access () =
  Log.info "Testing row access...";

  let row =
    [
      ("id", Value.int 1);
      ("name", Value.string "Alice");
      ("active", Value.bool true);
    ]
  in

  assert (Row.get "id" row = Some (Value.int 1));
  assert (Row.int "id" row = Some 1);
  assert (Row.string "name" row = Some "Alice");
  assert (Row.bool "active" row = Some true);
  assert (Row.get "missing" row = None);

  let fields = Row.fields row in
  assert (List.length fields = 3);
  assert (List.mem "id" fields);
  assert (List.mem "name" fields);
  assert (List.mem "active" fields);

  Log.info "Row access: OK"

let test_value_equality () =
  Log.info "Testing value equality...";

  assert (Value.equal (Value.int 42) (Value.int 42));
  assert (not (Value.equal (Value.int 42) (Value.int 43)));
  assert (Value.equal (Value.string "test") (Value.string "test"));
  assert (Value.equal Value.null Value.null);
  assert (not (Value.equal Value.null (Value.int 0)));

  Log.info "Value equality: OK"

let test_value_comparison () =
  Log.info "Testing value comparison...";

  assert (Value.compare (Value.int 1) (Value.int 2) < 0);
  assert (Value.compare (Value.int 2) (Value.int 1) > 0);
  assert (Value.compare (Value.int 1) (Value.int 1) = 0);
  assert (Value.compare Value.null (Value.int 1) < 0);
  assert (Value.compare (Value.int 1) Value.null > 0);

  Log.info "Value comparison: OK"

let main () =
  Log.set_level Log.Debug;
  Log.info "Starting SQLx tests...";

  test_value_conversions ();
  test_row_access ();
  test_value_equality ();
  test_value_comparison ();

  Log.info "All tests passed!"

let () = main ()
