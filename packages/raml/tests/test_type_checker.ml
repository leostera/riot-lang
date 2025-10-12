open Std

(** Integration tests for the type checker.

    These tests verify end-to-end type checking of expressions. *)

module TC = Raml.TypeChecker
module TT = Raml.TypedTree
module T = Raml.Types
module I = Raml.Identifier
module MP = Raml.ModulePath
module L = Raml.Location

(** {2 Test Helpers} *)

let create_test_context () = TC.create_context ()

let dummy_loc =
  Some (L.make ~start_line:1 ~start_col:0 ~end_line:1 ~end_col:0 ~offset:0)

let make_const_int n =
  TT.make_expression ~desc:(TT.ExpressionConstant (TT.ConstantInt n))
    ~typ:(T.Variable { contents = None; id = -1; level = 0 })
    ~loc:dummy_loc

let make_const_string s =
  TT.make_expression ~desc:(TT.ExpressionConstant (TT.ConstantString s))
    ~typ:(T.Variable { contents = None; id = -1; level = 0 })
    ~loc:dummy_loc

let make_const_unit () =
  TT.make_expression ~desc:(TT.ExpressionConstant TT.ConstantUnit)
    ~typ:(T.Variable { contents = None; id = -1; level = 0 })
    ~loc:dummy_loc

let print_type_expr ty = T.type_expr_to_string ty

let assert_type_equals expected actual =
  let expected_str = print_type_expr expected in
  let actual_str = print_type_expr actual in
  if expected_str <> actual_str then
    failwith
      (format "Type mismatch: expected %s, got %s" expected_str actual_str)

(** {2 Test: Constants} *)

let test_constant_int () =
  Log.info "Test: type checking integer constant";
  let ctx = create_test_context () in
  let expr = make_const_int 42 in

  match TC.type_check_expression ~ctx expr with
  | Ok (typed_expr, ctx) ->
      let expected_ty, _ = Raml.Environment.type_int ~ctx:ctx.types_ctx in
      assert_type_equals expected_ty typed_expr.TT.exp_type;
      Log.info "✓ Integer constant has type int"
  | Error err -> failwith (format "Failed: %s" (TC.error_to_string err))

let test_constant_string () =
  Log.info "Test: type checking string constant";
  let ctx = create_test_context () in
  let expr = make_const_string "hello" in

  match TC.type_check_expression ~ctx expr with
  | Ok (typed_expr, ctx) ->
      let expected_ty, _ = Raml.Environment.type_string ~ctx:ctx.types_ctx in
      assert_type_equals expected_ty typed_expr.TT.exp_type;
      Log.info "✓ String constant has type string"
  | Error err -> failwith (format "Failed: %s" (TC.error_to_string err))

let test_constant_unit () =
  Log.info "Test: type checking unit constant";
  let ctx = create_test_context () in
  let expr = make_const_unit () in

  match TC.type_check_expression ~ctx expr with
  | Ok (typed_expr, ctx) ->
      let expected_ty, _ = Raml.Environment.type_unit ~ctx:ctx.types_ctx in
      assert_type_equals expected_ty typed_expr.TT.exp_type;
      Log.info "✓ Unit constant has type unit"
  | Error err -> failwith (format "Failed: %s" (TC.error_to_string err))

(** {2 Test: Tuples} *)

let test_tuple_pair () =
  Log.info "Test: type checking tuple (int * string)";
  let ctx = create_test_context () in

  let int_expr = make_const_int 42 in
  let str_expr = make_const_string "hello" in
  let tuple =
    TT.make_expression
      ~desc:(TT.ExpressionTuple [ int_expr; str_expr ])
      ~typ:(T.Variable { contents = None; id = -1; level = 0 })
      ~loc:dummy_loc
  in

  match TC.type_check_expression ~ctx tuple with
  | Ok (typed_expr, _ctx) -> (
      match typed_expr.TT.exp_type with
      | T.Tuple [ ty1; ty2 ] ->
          Log.info "✓ Tuple has type (int * string)";
          Log.debug "  Type: %s" (print_type_expr typed_expr.exp_type)
      | _ -> failwith "Expected Tuple type")
  | Error err -> failwith (format "Failed: %s" (TC.error_to_string err))

let test_tuple_empty () =
  Log.info "Test: type checking empty tuple ()";
  let ctx = create_test_context () in

  let tuple =
    TT.make_expression ~desc:(TT.ExpressionTuple [])
      ~typ:(T.Variable { contents = None; id = -1; level = 0 })
      ~loc:dummy_loc
  in

  match TC.type_check_expression ~ctx tuple with
  | Ok (typed_expr, ctx) ->
      let expected_ty, _ = Raml.Environment.type_unit ~ctx:ctx.types_ctx in
      assert_type_equals expected_ty typed_expr.TT.exp_type;
      Log.info "✓ Empty tuple has type unit"
  | Error err -> failwith (format "Failed: %s" (TC.error_to_string err))

(** {2 Test: Functions} *)

let test_function_identity () =
  Log.info "Test: type checking identity function (fun x -> x)";
  let ctx = create_test_context () in

  (* Create: fun x -> x *)
  let x_ident, ctx = I.create_local ~ctx:ctx.types_ctx "x" in
  let x_path = MP.Identifier x_ident in

  let x_expr =
    TT.make_expression ~desc:(TT.ExpressionIdentifier x_path)
      ~typ:(T.Variable { contents = None; id = -1; level = 0 })
      ~loc:dummy_loc
  in

  let pat =
    TT.make_pattern ~desc:(TT.PatternVar x_ident)
      ~typ:(T.Variable { contents = None; id = -1; level = 0 })
      ~loc:dummy_loc
  in

  let case = { TT.case_pattern = pat; case_body = x_expr } in

  let func =
    TT.make_expression
      ~desc:(TT.ExpressionFunction { cases = [ case ] })
      ~typ:(T.Variable { contents = None; id = -1; level = 0 })
      ~loc:dummy_loc
  in

  match TC.type_check_expression ~ctx func with
  | Ok (typed_expr, _ctx) -> (
      match typed_expr.TT.exp_type with
      | T.Arrow (_arg_ty, _ret_ty) ->
          Log.info "✓ Identity function has arrow type";
          Log.debug "  Type: %s" (print_type_expr typed_expr.exp_type)
      | _ -> failwith "Expected Arrow type")
  | Error err -> failwith (format "Failed: %s" (TC.error_to_string err))

let test_function_const () =
  Log.info "Test: type checking const function (fun x -> 42)";
  let ctx = create_test_context () in

  (* Create: fun x -> 42 *)
  let x_ident, ctx = I.create_local ~ctx:ctx.types_ctx "x" in

  let const_expr = make_const_int 42 in

  let pat =
    TT.make_pattern ~desc:(TT.PatternVar x_ident)
      ~typ:(T.Variable { contents = None; id = -1; level = 0 })
      ~loc:dummy_loc
  in

  let case = { TT.case_pattern = pat; case_body = const_expr } in

  let func =
    TT.make_expression
      ~desc:(TT.ExpressionFunction { cases = [ case ] })
      ~typ:(T.Variable { contents = None; id = -1; level = 0 })
      ~loc:dummy_loc
  in

  match TC.type_check_expression ~ctx func with
  | Ok (typed_expr, ctx) -> (
      match typed_expr.TT.exp_type with
      | T.Arrow (_arg_ty, ret_ty) ->
          let expected_ret, _ = Raml.Environment.type_int ~ctx:ctx.types_ctx in
          assert_type_equals expected_ret ret_ty;
          Log.info "✓ Const function has type 'a -> int"
      | _ -> failwith "Expected Arrow type")
  | Error err -> failwith (format "Failed: %s" (TC.error_to_string err))

(** {2 Test: If/Then/Else} *)

let test_if_then_else_with_else () =
  Log.info "Test: type checking if/then/else (with else)";
  let ctx = create_test_context () in

  (* Create a dummy bool condition (we'll use unit for now since we don't have bool literals yet) *)
  let bool_ty, types_ctx = Raml.Environment.type_bool ~ctx:ctx.types_ctx in
  let ctx = { ctx with types_ctx } in

  (* Create condition expression with bool type *)
  let condition =
    TT.make_expression ~desc:(TT.ExpressionConstant TT.ConstantUnit)
      ~typ:bool_ty ~loc:dummy_loc
  in

  let then_branch = make_const_int 1 in
  let else_branch = make_const_int 2 in

  let if_expr =
    TT.make_expression
      ~desc:
        (TT.ExpressionIfThenElse
           { condition; then_branch; else_branch = Some else_branch })
      ~typ:(T.Variable { contents = None; id = -1; level = 0 })
      ~loc:dummy_loc
  in

  match TC.type_check_expression ~ctx if_expr with
  | Ok (typed_expr, ctx) ->
      let expected_ty, _ = Raml.Environment.type_int ~ctx:ctx.types_ctx in
      assert_type_equals expected_ty typed_expr.TT.exp_type;
      Log.info "✓ If/then/else has type int"
  | Error err -> failwith (format "Failed: %s" (TC.error_to_string err))

let test_if_then_else_without_else () =
  Log.info "Test: type checking if/then (without else)";
  let ctx = create_test_context () in

  (* Create a bool condition *)
  let bool_ty, types_ctx = Raml.Environment.type_bool ~ctx:ctx.types_ctx in
  let ctx = { ctx with types_ctx } in

  let condition =
    TT.make_expression ~desc:(TT.ExpressionConstant TT.ConstantUnit)
      ~typ:bool_ty ~loc:dummy_loc
  in

  let then_branch = make_const_unit () in

  let if_expr =
    TT.make_expression
      ~desc:
        (TT.ExpressionIfThenElse { condition; then_branch; else_branch = None })
      ~typ:(T.Variable { contents = None; id = -1; level = 0 })
      ~loc:dummy_loc
  in

  match TC.type_check_expression ~ctx if_expr with
  | Ok (typed_expr, ctx) ->
      let expected_ty, _ = Raml.Environment.type_unit ~ctx:ctx.types_ctx in
      assert_type_equals expected_ty typed_expr.TT.exp_type;
      Log.info "✓ If/then (no else) has type unit"
  | Error err -> failwith (format "Failed: %s" (TC.error_to_string err))

(** {2 Main Test Runner} *)

let run_tests () =
  Log.set_level Log.Info;

  Log.info "=== Type Checker Integration Tests ===\n";

  (* Constants *)
  test_constant_int ();
  test_constant_string ();
  test_constant_unit ();

  (* Tuples *)
  test_tuple_pair ();
  test_tuple_empty ();

  (* Functions *)
  test_function_identity ();
  test_function_const ();

  (* If/then/else *)
  test_if_then_else_with_else ();
  test_if_then_else_without_else ();

  Log.info "\n✅ All tests passed!"

let () = run_tests ()
