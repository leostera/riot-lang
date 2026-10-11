// Typecheck the supported native subset and produce a restricted core.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use String.{equal}
use Check.Names.{check_source_module_names}
use Syntax.Ast.{
  Expr, Program, ValueType, I32, BoolType, StringType, UnitType, DynamicType,
  FunctionType, ValueFunctionType, Integer, Boolean, StringLiteral, Unit,
  VariableExpr, TupleExpr, RecordExpr, RecordExpression, Binary,
  BinaryExpression, Addition, Subtraction, Multiplication, Division, Remainder,
  Equal, NotEqual, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual,
  Negate, Function, FunctionExpression, Apply, ApplicationExpression, Let,
  LetExpression, Match, MatchExpression, MatchArm, Variable, Wildcard,
  PatternUnit, PatternInteger, PatternBoolean, PatternString, TuplePattern,
  ConstructorPattern, ConstructorPatternData,
  RecordPattern, RecordPatternData, RecordPatternField, AscribedPattern,
  AscribedPatternData, ConstructorExpr,
  ConstructorExpression, RecordExpressionField, Ascription,
  AscriptionExpression, Block, Main, IntegerLiteral, ValueName, NamedType,
  NamedTypeExpression, ArrowType, ArrowTypeExpression, TypeName, SourceModule,
  ValueDeclaration, ValueDeclarationData, TypeDeclaration, Import,
  ImportedName, MemberName
}

type NativeOperator =
  | NativeAddition
  | NativeSubtraction
  | NativeMultiplication
  | NativeDivision
  | NativeRemainder
  | NativeEqual
  | NativeNotEqual
  | NativeLessThan
  | NativeLessThanOrEqual
  | NativeGreaterThan
  | NativeGreaterThanOrEqual

type NativeBinaryExpression = NativeBinaryExpression {
  operator: NativeOperator,
  left: NativeExpr,
  right: NativeExpr
}

type NativeExpr =
  | NativeInteger(IntegerLiteral)
  | NativeBoolean(bool)
  | NativeBinary(NativeBinaryExpression)
  | NativeNegation(NativeExpr)
  | NativeSource(Expr)

type CheckedExpression = CheckedExpression {
  inferred_type: ValueType,
  native_expression: NativeExpr
}

type ValueBinding = ValueBinding {
  name: ValueName,
  value_type: ValueType
}

let source_expression = fn inferred_type -> fn expression -> CheckedExpression {
  inferred_type: inferred_type,
  native_expression: NativeSource(expression)
}

let annotation_value_type = fn type_expression ->
  match type_expression {
    | NamedType(NamedTypeExpression { name: TypeName(name), arguments: _ }) ->
      match (equal name "i32") {
        | true -> I32
        | false -> match (equal name "bool") {
          | true -> BoolType
          | false -> match (equal name "string") {
            | true -> StringType
            | false -> match (equal name "unit") {
              | true -> UnitType
              | false -> DynamicType
            }
          }
        }
      }
    | ArrowType(ArrowTypeExpression { argument, result }) -> FunctionType(ValueFunctionType {
      argument: annotation_value_type argument,
      result: annotation_value_type result
    })
    | _ -> DynamicType
  }

let compatible_value_types = fn expected -> fn actual ->
  match expected {
    | DynamicType -> true
    | I32 -> match actual { | I32 -> true | DynamicType -> true | _ -> false }
    | BoolType -> match actual { | BoolType -> true | DynamicType -> true | _ -> false }
    | StringType -> match actual { | StringType -> true | DynamicType -> true | _ -> false }
    | UnitType -> match actual { | UnitType -> true | DynamicType -> true | _ -> false }
    | FunctionType(ValueFunctionType { argument: expected_argument, result: expected_result }) ->
      match actual {
        | FunctionType(ValueFunctionType { argument: actual_argument, result: actual_result }) ->
          match (compatible_value_types expected_argument actual_argument) {
            | false -> false
            | true -> compatible_value_types expected_result actual_result
          }
        | DynamicType -> true
        | _ -> false
      }
  }

let binary_operand_constraint = fn operator -> fn other_operand ->
  match operator {
    | Addition -> match other_operand {
      | Integer(_) -> Some(I32)
      | StringLiteral(_) -> Some(StringType)
      | _ -> None
    }
    | Subtraction -> match other_operand { | Integer(_) -> Some(I32) | _ -> None }
    | Multiplication -> match other_operand { | Integer(_) -> Some(I32) | _ -> None }
    | Division -> match other_operand { | Integer(_) -> Some(I32) | _ -> None }
    | Remainder -> match other_operand { | Integer(_) -> Some(I32) | _ -> None }
    | Equal -> match other_operand { | Integer(_) -> Some(I32) | _ -> None }
    | NotEqual -> match other_operand { | Integer(_) -> Some(I32) | _ -> None }
    | LessThan -> match other_operand { | Integer(_) -> Some(I32) | _ -> None }
    | LessThanOrEqual -> match other_operand { | Integer(_) -> Some(I32) | _ -> None }
    | GreaterThan -> match other_operand { | Integer(_) -> Some(I32) | _ -> None }
    | GreaterThanOrEqual -> match other_operand { | Integer(_) -> Some(I32) | _ -> None }
  }

let parameter_binary_constraint = fn parameter_name -> fn (BinaryExpression {
  operator,
  left,
  right
}) ->
  match left {
    | VariableExpr(ValueName(name)) -> match (equal parameter_name name) {
      | true -> binary_operand_constraint operator right
      | false -> match right {
        | VariableExpr(ValueName(right_name)) -> match (equal parameter_name right_name) {
          | true -> binary_operand_constraint operator left
          | false -> None
        }
        | _ -> None
      }
    }
    | _ -> match right {
      | VariableExpr(ValueName(name)) -> match (equal parameter_name name) {
        | true -> binary_operand_constraint operator left
        | false -> None
      }
      | _ -> None
    }
  }

let same_inferred_type = fn first -> fn second ->
  match first {
    | I32 -> match second { | I32 -> true | _ -> false }
    | BoolType -> match second { | BoolType -> true | _ -> false }
    | StringType -> match second { | StringType -> true | _ -> false }
    | UnitType -> match second { | UnitType -> true | _ -> false }
    | FunctionType(ValueFunctionType { argument: first_argument, result: first_result }) ->
      match second {
        | FunctionType(ValueFunctionType { argument: second_argument, result: second_result }) ->
          match (same_inferred_type first_argument second_argument) {
            | false -> false
            | true -> same_inferred_type first_result second_result
          }
        | _ -> false
      }
    | _ -> false
  }

let merge_parameter_constraints = fn previous -> fn next ->
  match previous {
    | None -> Ok(next)
    | Some(previous_type) -> match next {
      | None -> Ok(Some(previous_type))
      | Some(next_type) -> match (same_inferred_type previous_type next_type) {
        | true -> Ok(Some(previous_type))
        | false -> Err("function parameter is used with incompatible primitive types")
      }
    }
  }

let combine_parameter_constraints = fn first -> fn second ->
  match first {
    | Err(message) -> Err(message)
    | Ok(first_constraint) -> match second {
      | Err(message) -> Err(message)
      | Ok(second_constraint) ->
        merge_parameter_constraints first_constraint second_constraint
    }
  }

let pattern_binds_name = fn pattern -> fn wanted_name ->
  match pattern {
    | Variable(ValueName(name)) -> equal wanted_name name
    | TuplePattern(patterns) -> pattern_list_binds_name patterns wanted_name
    | ConstructorPattern(ConstructorPatternData { constructor_name: _, arguments }) ->
      pattern_list_binds_name arguments wanted_name
    | RecordPattern(RecordPatternData { constructor_name: _, fields }) ->
      record_pattern_binds_name fields wanted_name
    | AscribedPattern(AscribedPatternData { pattern: inner, annotation: _ }) ->
      pattern_binds_name inner wanted_name
    | _ -> false
  }

let pattern_list_binds_name = fn patterns -> fn wanted_name ->
  match patterns {
    | Nil -> false
    | Cons(pattern, remaining) -> match (pattern_binds_name pattern wanted_name) {
      | true -> true
      | false -> pattern_list_binds_name remaining wanted_name
    }
  }

let record_pattern_binds_name = fn fields -> fn wanted_name ->
  match fields {
    | Nil -> false
    | Cons(RecordPatternField { name: _, pattern }, remaining) ->
      match (pattern_binds_name pattern wanted_name) {
        | true -> true
        | false -> record_pattern_binds_name remaining wanted_name
      }
  }

let pattern_type_constraint = fn pattern ->
  match pattern {
    | PatternInteger(_) -> Some(I32)
    | PatternBoolean(_) -> Some(BoolType)
    | PatternString(_) -> Some(StringType)
    | PatternUnit -> Some(UnitType)
    | AscribedPattern(AscribedPatternData { pattern: _, annotation }) ->
      Some(annotation_value_type annotation)
    | _ -> None
  }

let direct_variable_constraint = fn name -> fn expression -> fn inferred_type ->
  match expression {
    | VariableExpr(ValueName(variable_name)) -> match (equal name variable_name) {
      | true -> Some(inferred_type)
      | false -> None
    }
    | _ -> None
  }

let scan_binary_constraints = fn name -> fn (BinaryExpression {
  operator,
  left,
  right
}) -> {
  let direct_constraint = match (parameter_binary_constraint name (BinaryExpression {
    operator: operator,
    left: left,
    right: right
  })) {
    | None -> Ok(None)
    | Some(inferred_type) -> Ok(Some(inferred_type))
  };
  let nested_constraints = match (combine_parameter_constraints
    (scan_expression_constraints name left)
    (scan_expression_constraints name right)) {
    | Err(message) -> Err(message)
    | Ok(constraint) -> Ok(constraint)
  };
  combine_parameter_constraints direct_constraint nested_constraints
}

let scan_expression_list_constraints = fn name -> fn expressions ->
  match expressions {
    | Nil -> Ok(None)
    | Cons(expression, remaining) ->
      match (combine_parameter_constraints
        (scan_expression_constraints name expression)
        (scan_expression_list_constraints name remaining)) {
        | Err(message) -> Err(message)
        | Ok(constraint) -> Ok(constraint)
      }
  }

let scan_record_field_constraints = fn name -> fn fields ->
  match fields {
    | Nil -> Ok(None)
    | Cons(RecordExpressionField { name: _, value }, remaining) ->
      match (combine_parameter_constraints
        (scan_expression_constraints name value)
        (scan_record_field_constraints name remaining)) {
        | Err(message) -> Err(message)
        | Ok(constraint) -> Ok(constraint)
      }
  }

let scan_match_arm_constraints = fn name -> fn scrutinee_name -> fn arms ->
  match arms {
    | Nil -> Ok(None)
    | Cons(MatchArm { pattern, body }, remaining) -> {
      let pattern_constraint = match scrutinee_name {
        | None -> None
        | Some(scrutinee_variable) -> match (equal name scrutinee_variable) {
          | true -> pattern_type_constraint pattern
          | false -> None
        }
      };
      let body_constraint = match (pattern_binds_name pattern name) {
        | true -> Ok(None)
        | false -> scan_expression_constraints name body
      };
      match (combine_parameter_constraints
        (Ok(pattern_constraint)) body_constraint) {
        | Err(message) -> Err(message)
        | Ok(arm_constraint) -> match (combine_parameter_constraints
            (Ok(arm_constraint))
            (scan_match_arm_constraints name scrutinee_name remaining)) {
          | Err(message) -> Err(message)
          | Ok(all_constraints) -> Ok(all_constraints)
        }
      }
    }
  }

let scan_expression_constraints = fn name -> fn expression ->
  match expression {
    | Integer(_) -> Ok(None)
    | Boolean(_) -> Ok(None)
    | StringLiteral(_) -> Ok(None)
    | Unit -> Ok(None)
    | VariableExpr(_) -> Ok(None)
    | TupleExpr(items) -> scan_expression_list_constraints name items
    | RecordExpr(RecordExpression { constructor_name: _, fields }) ->
      scan_record_field_constraints name fields
    | Binary(binary) -> scan_binary_constraints name binary
    | Negate(operand) -> match (combine_parameter_constraints
        (Ok(direct_variable_constraint name operand I32))
        (scan_expression_constraints name operand)) {
      | Err(message) -> Err(message)
      | Ok(constraint) -> Ok(constraint)
    }
    | Function(FunctionExpression { parameter, body }) -> match (pattern_binds_name parameter name) {
      | true -> Ok(None)
      | false -> scan_expression_constraints name body
    }
    | Apply(ApplicationExpression { callee, argument }) ->
      match (combine_parameter_constraints
        (scan_expression_constraints name callee)
        (scan_expression_constraints name argument)) {
        | Err(message) -> Err(message)
        | Ok(constraint) -> Ok(constraint)
      }
    | Let(LetExpression { pattern, annotation: _, value, body }) -> {
      let value_constraint = scan_expression_constraints name value;
      let body_constraint = match (pattern_binds_name pattern name) {
        | true -> Ok(None)
        | false -> scan_expression_constraints name body
      };
      combine_parameter_constraints value_constraint body_constraint
    }
    | Match(MatchExpression { scrutinee, arms }) -> {
      let scrutinee_name = match scrutinee {
        | VariableExpr(ValueName(variable_name)) -> Some(variable_name)
        | _ -> None
      };
      match (combine_parameter_constraints
        (scan_expression_constraints name scrutinee)
        (scan_match_arm_constraints name scrutinee_name arms)) {
        | Err(message) -> Err(message)
        | Ok(constraint) -> Ok(constraint)
      }
    }
    | ConstructorExpr(ConstructorExpression { constructor_name: _, arguments }) ->
      scan_expression_list_constraints name arguments
    | Ascription(AscriptionExpression { expression: inner, annotation }) ->
      match (combine_parameter_constraints
        (Ok(direct_variable_constraint name inner (annotation_value_type annotation)))
        (scan_expression_constraints name inner)) {
        | Err(message) -> Err(message)
        | Ok(constraint) -> Ok(constraint)
      }
    | Block(items) -> scan_expression_list_constraints name items
  }

let function_parameter_type = fn pattern -> fn body ->
  match pattern {
    | AscribedPattern(AscribedPatternData { pattern: _, annotation }) ->
      Ok(annotation_value_type annotation)
    | Variable(ValueName(parameter_name)) -> match (scan_expression_constraints parameter_name body) {
      | Err(message) -> Err(message)
      | Ok(None) -> Ok(DynamicType)
      | Ok(Some(inferred_type)) -> Ok(inferred_type)
    }
    | _ -> Ok(DynamicType)
  }

let application_result_type = fn callee_type -> fn argument_type ->
  match callee_type {
    | FunctionType(ValueFunctionType { argument, result }) ->
      match (compatible_value_types argument argument_type) {
        | true -> Ok(result)
        | false -> Err("function argument has an incompatible type")
      }
    | DynamicType -> Ok(DynamicType)
    | _ -> Err("attempted to apply a non-function value")
  }

type CheckedProgram = CheckedProgram(NativeExpr)

let native_operator = fn operator ->
  match operator {
    | Addition -> NativeAddition
    | Subtraction -> NativeSubtraction
    | Multiplication -> NativeMultiplication
    | Division -> NativeDivision
    | Remainder -> NativeRemainder
    | Equal -> NativeEqual
    | NotEqual -> NativeNotEqual
    | LessThan -> NativeLessThan
    | LessThanOrEqual -> NativeLessThanOrEqual
    | GreaterThan -> NativeGreaterThan
    | GreaterThanOrEqual -> NativeGreaterThanOrEqual
  }

let checked_binary_result = fn operator -> fn left_expression -> fn right_expression ->
  match operator {
    | NativeAddition -> make_i32_binary operator left_expression right_expression
    | NativeSubtraction -> make_i32_binary operator left_expression right_expression
    | NativeMultiplication -> make_i32_binary operator left_expression right_expression
    | NativeDivision -> make_i32_binary operator left_expression right_expression
    | NativeRemainder -> make_i32_binary operator left_expression right_expression
    | NativeEqual -> make_i32_comparison operator left_expression right_expression
    | NativeNotEqual -> make_i32_comparison operator left_expression right_expression
    | NativeLessThan -> make_i32_comparison operator left_expression right_expression
    | NativeLessThanOrEqual -> make_i32_comparison operator left_expression right_expression
    | NativeGreaterThan -> make_i32_comparison operator left_expression right_expression
    | NativeGreaterThanOrEqual -> make_i32_comparison operator left_expression right_expression
  }

let checked_expression_type = fn (CheckedExpression {
  inferred_type,
  native_expression: _
}) -> inferred_type

let checked_expression_native = fn (CheckedExpression {
  inferred_type: _,
  native_expression
}) -> native_expression

let is_i32_or_dynamic = fn value_type ->
  match value_type {
    | I32 -> true
    | DynamicType -> true
    | _ -> false
  }

let both_i32_or_dynamic = fn left_type -> fn right_type ->
  match (is_i32_or_dynamic left_type) {
    | true -> is_i32_or_dynamic right_type
    | false -> false
  }

let make_i32_binary = fn operator -> fn left_expression -> fn right_expression ->
  match (both_i32_or_dynamic
    (checked_expression_type left_expression)
    (checked_expression_type right_expression)) {
    | true -> Ok(CheckedExpression {
      inferred_type: I32,
      native_expression: make_native_binary operator left_expression right_expression
    })
    | false -> Err("integer arithmetic requires two i32 operands")
  }

let make_i32_comparison = fn operator -> fn left_expression -> fn right_expression ->
  match (both_i32_or_dynamic
    (checked_expression_type left_expression)
    (checked_expression_type right_expression)) {
    | true -> Ok(CheckedExpression {
      inferred_type: BoolType,
      native_expression: make_native_binary operator left_expression right_expression
    })
    | false -> Err("integer comparisons require two i32 operands")
  }

let make_native_binary = fn operator -> fn left_expression -> fn right_expression ->
  NativeBinary(NativeBinaryExpression {
    operator: operator,
    left: checked_expression_native left_expression,
    right: checked_expression_native right_expression
  })

let dynamic_addition = fn result_type -> fn operator -> fn left -> fn right ->
  Ok(CheckedExpression {
    inferred_type: result_type,
    native_expression: NativeSource(Binary(BinaryExpression {
      operator: operator,
      left: left,
      right: right
    }))
  })

let check_addition = fn operator -> fn left -> fn right -> fn checked_left -> fn checked_right ->
  match (checked_expression_type checked_left) {
    | StringType -> match (checked_expression_type checked_right) {
      | StringType -> dynamic_addition StringType operator left right
      | DynamicType -> dynamic_addition DynamicType operator left right
      | _ -> Err("string concatenation requires two string operands")
    }
    | I32 -> match (checked_expression_type checked_right) {
      | I32 -> checked_binary_result NativeAddition checked_left checked_right
      | DynamicType -> dynamic_addition DynamicType operator left right
      | _ -> Err("integer addition requires two i32 operands")
    }
    | DynamicType -> match (checked_expression_type checked_right) {
      | I32 -> dynamic_addition DynamicType operator left right
      | StringType -> dynamic_addition DynamicType operator left right
      | DynamicType -> dynamic_addition DynamicType operator left right
      | _ -> Err("addition requires two i32 operands or two strings")
    }
    | _ -> Err("addition requires two i32 operands or two strings")
  }

let lookup_value_type = fn name -> fn bindings ->
  match bindings {
    | Nil -> None
    | Cons(ValueBinding { name: ValueName(binding_name), value_type }, remaining) ->
      match (equal name binding_name) {
        | true -> Some(value_type)
        | false -> lookup_value_type name remaining
      }
  }

let bind_pattern_type = fn pattern -> fn value_type -> fn bindings ->
  match pattern {
    | Variable(name) -> Cons(ValueBinding {
      name: name,
      value_type: value_type
    }, bindings)
    | AscribedPattern(AscribedPatternData { pattern: inner, annotation }) ->
      bind_pattern_type inner (annotation_value_type annotation) bindings
    | TuplePattern(patterns) -> bind_pattern_list_type patterns DynamicType bindings
    | ConstructorPattern(ConstructorPatternData { constructor_name: _, arguments }) ->
      bind_pattern_list_type arguments DynamicType bindings
    | RecordPattern(RecordPatternData { constructor_name: _, fields }) ->
      bind_record_pattern_type fields DynamicType bindings
    | _ -> bindings
  }

let bind_pattern_list_type = fn patterns -> fn value_type -> fn bindings ->
  match patterns {
    | Nil -> bindings
    | Cons(pattern, remaining) -> bind_pattern_list_type remaining value_type
      (bind_pattern_type pattern value_type bindings)
  }

let bind_record_pattern_type = fn fields -> fn value_type -> fn bindings ->
  match fields {
    | Nil -> bindings
    | Cons(RecordPatternField { name: _, pattern }, remaining) ->
      bind_record_pattern_type remaining value_type (bind_pattern_type pattern value_type bindings)
  }

let check_expression_list_in = fn expressions -> fn bindings ->
  match expressions {
    | Nil -> Ok(())
    | Cons(expression, remaining) -> match (check_expression_in expression bindings) {
      | Err(message) -> Err(message)
      | Ok(_) -> check_expression_list_in remaining bindings
    }
  }

let check_record_fields_in = fn fields -> fn bindings ->
  match fields {
    | Nil -> Ok(())
    | Cons(RecordExpressionField { name: _, value }, remaining) ->
      match (check_expression_in value bindings) {
        | Err(message) -> Err(message)
        | Ok(_) -> check_record_fields_in remaining bindings
      }
  }

let check_expression_sequence_in = fn expressions -> fn bindings ->
  match expressions {
    | Nil -> Ok(UnitType)
    | Cons(expression, Nil) -> match (check_expression_in expression bindings) {
      | Err(message) -> Err(message)
      | Ok(checked_expression) -> Ok(checked_expression_type checked_expression)
    }
    | Cons(expression, remaining) -> match (check_expression_in expression bindings) {
      | Err(message) -> Err(message)
      | Ok(_) -> check_expression_sequence_in remaining bindings
    }
  }

let pattern_matches_type = fn pattern -> fn scrutinee_type ->
  match pattern {
    | PatternInteger(_) -> match scrutinee_type {
      | I32 -> true
      | DynamicType -> true
      | _ -> false
    }
    | PatternBoolean(_) -> match scrutinee_type {
      | BoolType -> true
      | DynamicType -> true
      | _ -> false
    }
    | PatternString(_) -> match scrutinee_type {
      | StringType -> true
      | DynamicType -> true
      | _ -> false
    }
    | PatternUnit -> match scrutinee_type {
      | UnitType -> true
      | DynamicType -> true
      | _ -> false
    }
    | AscribedPattern(AscribedPatternData { pattern: _, annotation }) ->
      compatible_value_types (annotation_value_type annotation) scrutinee_type
    | _ -> true
  }

let check_match_arm_bodies_in = fn arms -> fn scrutinee_type -> fn bindings ->
  match arms {
    | Nil -> Ok(DynamicType)
    | Cons(MatchArm { pattern, body }, remaining) -> match (pattern_matches_type pattern scrutinee_type) {
      | false -> Err("match pattern does not match the scrutinee type")
      | true -> match (check_expression_in body
          (bind_pattern_type pattern DynamicType bindings)) {
        | Err(message) -> Err(message)
        | Ok(checked_body) -> match (check_match_arm_bodies_in remaining scrutinee_type bindings) {
          | Err(message) -> Err(message)
          | Ok(remaining_type) -> match (compatible_value_types
              (checked_expression_type checked_body) remaining_type) {
            | true -> Ok(checked_expression_type checked_body)
            | false -> Err("match arms must return compatible types")
          }
        }
      }
    }
  }

let merge_boolean_branch_type = fn previous -> fn current ->
  match previous {
    | None -> Ok(Some(current))
    | Some(previous_type) -> match (compatible_value_types previous_type current) {
      | true -> Ok(Some(previous_type))
      | false -> Err("boolean match arms must return compatible types")
    }
  }

let check_boolean_arms_in = fn saw_true -> fn saw_false -> fn branch_type ->
  fn arms -> fn bindings ->
  match arms {
    | Nil -> match saw_true {
      | false -> Err("boolean match is missing a `true` arm")
      | true -> match saw_false {
        | false -> Err("boolean match is missing a `false` arm")
        | true -> match branch_type {
          | None -> Err("boolean match must have at least one arm")
          | Some(inferred_type) -> Ok(inferred_type)
        }
      }
    }
    | Cons(MatchArm { pattern, body }, remaining) -> match pattern {
      | PatternBoolean(true) -> match saw_true {
        | true -> Err("duplicate `true` match arm")
        | false -> match (check_expression_in body bindings) {
          | Err(message) -> Err(message)
          | Ok(checked_body) -> match (merge_boolean_branch_type branch_type
              (checked_expression_type checked_body)) {
            | Err(message) -> Err(message)
            | Ok(next_type) -> check_boolean_arms_in true saw_false next_type
              remaining bindings
          }
        }
      }
      | PatternBoolean(false) -> match saw_false {
        | true -> Err("duplicate `false` match arm")
        | false -> match (check_expression_in body bindings) {
          | Err(message) -> Err(message)
          | Ok(checked_body) -> match (merge_boolean_branch_type branch_type
              (checked_expression_type checked_body)) {
            | Err(message) -> Err(message)
            | Ok(next_type) -> check_boolean_arms_in saw_true true next_type
              remaining bindings
          }
        }
      }
      | _ -> Err("boolean match accepts only `true` and `false` arms")
    }
  }

let check_boolean_match_in = fn expression -> fn arms -> fn bindings ->
  match (check_boolean_arms_in false false None arms bindings) {
    | Err(message) -> Err(message)
    | Ok(inferred_type) -> Ok(source_expression inferred_type expression)
  }

let check_expression_in = fn expression -> fn bindings ->
  match expression {
    | Integer(literal) -> Ok(CheckedExpression {
      inferred_type: I32,
      native_expression: NativeInteger(literal)
    })
    | Boolean(value) -> Ok(CheckedExpression {
      inferred_type: BoolType,
      native_expression: NativeBoolean(value)
    })
    | StringLiteral(_) -> Ok(source_expression StringType expression)
    | Unit -> Ok(source_expression UnitType expression)
    | VariableExpr(ValueName(name)) -> match (lookup_value_type name bindings) {
      | None -> Ok(source_expression DynamicType expression)
      | Some(value_type) -> Ok(source_expression value_type expression)
    }
    | Binary(BinaryExpression { operator, left, right }) ->
      match (check_expression_in left bindings) {
        | Err(message) -> Err(message)
        | Ok(checked_left) -> match (check_expression_in right bindings) {
          | Err(message) -> Err(message)
          | Ok(checked_right) -> match operator {
            | Addition -> check_addition operator left right checked_left checked_right
            | _ -> checked_binary_result (native_operator operator) checked_left checked_right
          }
        }
      }
    | Negate(operand) -> match (check_expression_in operand bindings) {
      | Err(message) -> Err(message)
      | Ok(CheckedExpression { inferred_type: I32, native_expression }) ->
        Ok(CheckedExpression {
          inferred_type: I32,
          native_expression: NativeNegation(native_expression)
        })
      | Ok(CheckedExpression { inferred_type: DynamicType, native_expression }) ->
        Ok(CheckedExpression {
          inferred_type: I32,
          native_expression: NativeNegation(native_expression)
        })
      | Ok(_) -> Err("unary `-` requires an i32 operand")
    }
    | Function(FunctionExpression { parameter, body }) ->
      match (function_parameter_type parameter body) {
        | Err(message) -> Err(message)
        | Ok(argument_type) -> {
          let body_bindings = bind_pattern_type parameter argument_type bindings;
          match (check_expression_in body body_bindings) {
            | Err(message) -> Err(message)
            | Ok(checked_body) -> Ok(source_expression
              (FunctionType(ValueFunctionType {
                argument: argument_type,
                result: checked_expression_type checked_body
              })) expression)
          }
        }
      }
    | Apply(ApplicationExpression { callee, argument }) ->
      match (check_expression_in callee bindings) {
        | Err(message) -> Err(message)
        | Ok(checked_callee) -> match (check_expression_in argument bindings) {
          | Err(message) -> Err(message)
          | Ok(checked_argument) -> match (application_result_type
              (checked_expression_type checked_callee)
              (checked_expression_type checked_argument)) {
            | Err(message) -> Err(message)
            | Ok(result_type) -> Ok(source_expression result_type expression)
          }
        }
      }
    | Let(LetExpression { pattern, annotation, value, body }) ->
      match (check_expression_in value bindings) {
        | Err(message) -> Err(message)
        | Ok(checked_value) -> match annotation {
          | None -> check_let_body_in expression pattern (checked_expression_type checked_value)
            body bindings
          | Some(type_expression) -> {
            let declared_type = annotation_value_type type_expression;
            match (compatible_value_types declared_type (checked_expression_type checked_value)) {
              | false -> Err("let value does not match its type annotation")
              | true -> check_let_body_in expression pattern declared_type body bindings
            }
          }
        }
      }
    | TupleExpr(items) -> match (check_expression_list_in items bindings) {
      | Err(message) -> Err(message)
      | Ok(_) -> Ok(source_expression DynamicType expression)
    }
    | RecordExpr(RecordExpression { constructor_name: _, fields }) ->
      match (check_record_fields_in fields bindings) {
        | Err(message) -> Err(message)
        | Ok(_) -> Ok(source_expression DynamicType expression)
      }
    | ConstructorExpr(ConstructorExpression { constructor_name: _, arguments }) ->
      match (check_expression_list_in arguments bindings) {
        | Err(message) -> Err(message)
        | Ok(_) -> Ok(source_expression DynamicType expression)
      }
    | Ascription(AscriptionExpression { expression: inner, annotation }) ->
      match (check_expression_in inner bindings) {
        | Err(message) -> Err(message)
        | Ok(checked_inner) -> {
          let declared_type = annotation_value_type annotation;
          match (compatible_value_types declared_type (checked_expression_type checked_inner)) {
            | true -> Ok(source_expression declared_type expression)
            | false -> Err("expression does not match its type annotation")
          }
        }
      }
    | Block(items) -> match (check_expression_sequence_in items bindings) {
      | Err(message) -> Err(message)
      | Ok(result_type) -> Ok(source_expression result_type expression)
    }
    | Match(MatchExpression { scrutinee, arms }) ->
      match (check_expression_in scrutinee bindings) {
        | Err(message) -> Err(message)
        | Ok(checked_scrutinee) -> match (checked_expression_type checked_scrutinee) {
          | BoolType -> check_boolean_match_in expression arms bindings
          | scrutinee_type -> match (check_match_arm_bodies_in arms scrutinee_type bindings) {
            | Err(message) -> Err(message)
            | Ok(result_type) -> Ok(source_expression result_type expression)
          }
        }
      }
  }

let check_let_body_in = fn expression -> fn pattern -> fn value_type -> fn body -> fn bindings -> {
  let body_bindings = bind_pattern_type pattern value_type bindings;
  match (check_expression_in body body_bindings) {
    | Err(message) -> Err(message)
    | Ok(checked_body) -> Ok(source_expression (checked_expression_type checked_body) expression)
  }
}

let check_expression = fn expression -> check_expression_in expression Nil

let append_value_bindings = fn first -> fn second ->
  match first {
    | Nil -> second
    | Cons(binding, remaining) -> Cons(binding, append_value_bindings remaining second)
  }

let declared_value_bindings = fn declarations ->
  match declarations {
    | Nil -> Nil
    | Cons(ValueDeclaration(ValueDeclarationData { name, body: _ }), remaining) ->
      Cons(ValueBinding { name: name, value_type: DynamicType },
        declared_value_bindings remaining)
    | Cons(TypeDeclaration(_), remaining) -> declared_value_bindings remaining
  }

let imported_value_bindings = fn imports ->
  match imports {
    | Nil -> Nil
    | Cons(Import { module_path: _, selected_names }, remaining) ->
      append_value_bindings (selected_member_bindings selected_names)
        (imported_value_bindings remaining)
  }

let selected_member_bindings = fn selected_names ->
  match selected_names {
    | Nil -> Nil
    | Cons(ImportedName(MemberName(name)), remaining) ->
      Cons(ValueBinding {
        name: ValueName(name),
        value_type: DynamicType
      }, selected_member_bindings remaining)
  }

let module_value_bindings = fn imports -> fn declarations ->
  append_value_bindings (declared_value_bindings declarations)
    (imported_value_bindings imports)

let replace_value_binding = fn name -> fn new_value_type -> fn bindings ->
  match bindings {
    | Nil -> Cons(ValueBinding { name: name, value_type: new_value_type }, Nil)
    | Cons(ValueBinding { name: ValueName(existing_name), value_type: existing_type }, remaining) ->
      match name {
        | ValueName(wanted_name) -> match (equal wanted_name existing_name) {
          | true -> Cons(ValueBinding { name: name, value_type: new_value_type }, remaining)
          | false -> Cons(ValueBinding {
            name: ValueName(existing_name),
            value_type: existing_type
          }, replace_value_binding name new_value_type remaining)
        }
      }
  }

let infer_source_declaration_bindings = fn declarations -> fn bindings ->
  match declarations {
    | Nil -> Ok(bindings)
    | Cons(ValueDeclaration(ValueDeclarationData { name, body }), remaining) ->
      match (check_expression_in body bindings) {
        | Err(message) -> Err(message)
        | Ok(checked_body) -> infer_source_declaration_bindings remaining
          (replace_value_binding name (checked_expression_type checked_body) bindings)
      }
    | Cons(TypeDeclaration(_), remaining) ->
      infer_source_declaration_bindings remaining bindings
  }

let check_source_declarations = fn declarations -> fn bindings ->
  match declarations {
    | Nil -> Ok(())
    | Cons(ValueDeclaration(ValueDeclarationData { name: _, body }), remaining) ->
      match (check_expression_in body bindings) {
        | Err(message) -> Err(message)
        | Ok(_) -> check_source_declarations remaining bindings
      }
    | Cons(TypeDeclaration(_), remaining) -> check_source_declarations remaining bindings
  }

let check_source_module_bindings = fn (SourceModule { imports, declarations }) ->
  match (check_source_module_names (SourceModule {
    imports: imports,
    declarations: declarations
  })) {
    | "" -> {
      let initial_bindings = module_value_bindings imports declarations;
      match (infer_source_declaration_bindings declarations initial_bindings) {
        | Err(message) -> Err(message)
        | Ok(inferred_bindings) -> match (check_source_declarations declarations inferred_bindings) {
          | Err(message) -> Err(message)
          | Ok(_) -> Ok(inferred_bindings)
        }
      }
    }
    | message -> Err(message)
  }

let check_source_module = fn source_module ->
  match (check_source_module_bindings source_module) {
    | Err(message) -> Err(message)
    | Ok(_) -> Ok(())
  }

let find_native_entry = fn declarations ->
  match declarations {
    | Nil -> Err("native entry module must define `main`")
    | Cons(ValueDeclaration(ValueDeclarationData {
        name: ValueName("main"),
        body: Function(FunctionExpression { parameter: PatternUnit, body })
      }), _) -> Ok(body)
    | Cons(ValueDeclaration(ValueDeclarationData { name: _, body: _ }), remaining) ->
      find_native_entry remaining
    | Cons(TypeDeclaration(_), remaining) -> find_native_entry remaining
  }

let validate_native_entry_module = fn (SourceModule { imports, declarations }) -> {
  let source_module = SourceModule {
    imports: imports,
    declarations: declarations
  };
  match (find_native_entry declarations) {
    | Err(message) -> Err(message)
    | Ok(body) -> match (check_source_module_bindings source_module) {
      | Err(message) -> Err(message)
      | Ok(bindings) -> match (check_expression_in body bindings) {
        | Err(message) -> Err(message)
        | Ok(checked_body) -> match (checked_expression_type checked_body) {
          | I32 -> Ok(())
          | DynamicType -> Ok(())
          | _ -> Err("native entry point must return i32")
        }
      }
    }
  }
}

let check_program = fn program ->
  match program {
    | Main(expression) -> match (check_expression expression) {
      | Err(message) -> Err(message)
      | Ok(CheckedExpression {
          inferred_type: I32,
          native_expression: native_expression
        }) -> Ok(CheckedProgram(native_expression))
      | Ok(CheckedExpression {
          inferred_type: DynamicType,
          native_expression: native_expression
        }) -> Ok(CheckedProgram(native_expression))
      | Ok(_) -> Err("native entry point must return i32")
    }
  }
