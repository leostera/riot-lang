open Std

let test_simple_int () =
  let source = "let x = 42" in
  match Checker.typecheck source with
  | Ok result ->
      Log.info "✓ Type checked successfully!";
      Log.info "Expression type: %s"
        (Types.type_expr_to_string result.tree.exp_type)
  | Error msg -> Log.error "✗ Type check failed: %s" msg

let test_simple_string () =
  let source = "let x = \"hello\"" in
  match Checker.typecheck source with
  | Ok result ->
      Log.info "✓ Type checked successfully!";
      Log.info "Expression type: %s"
        (Types.type_expr_to_string result.tree.exp_type)
  | Error msg -> Log.error "✗ Type check failed: %s" msg

let test_tuple () =
  let source = "let x = (1, 2, 3)" in
  match Checker.typecheck source with
  | Ok result ->
      Log.info "✓ Type checked successfully!";
      Log.info "Expression type: %s"
        (Types.type_expr_to_string result.tree.exp_type)
  | Error msg -> Log.error "✗ Type check failed: %s" msg

let () =
  Log.set_level Log.Debug;
  Log.info "=== RAML Type Checker Tests ===";
  Log.info "";
  Log.info "Test 1: Simple integer";
  test_simple_int ();
  Log.info "";
  Log.info "Test 2: Simple string";
  test_simple_string ();
  Log.info "";
  Log.info "Test 3: Tuple";
  test_tuple ();
  Log.info "";
  Log.info "=== Tests Complete ==="
