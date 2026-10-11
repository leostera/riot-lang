// Typecheck the supported native subset and produce a restricted core.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Syntax.Ast.{
  Expr, Program, ValueType, I32, BoolType, Integer, Boolean, Binary,
  BinaryExpression, BinaryOperator, Addition, Subtraction, Multiplication,
  Division, Remainder, Equal, NotEqual, LessThan, LessThanOrEqual,
  GreaterThan, GreaterThanOrEqual, Negate, Match, MatchExpression,
  MatchArm, Pattern, PatternBoolean, Main, IntegerLiteral
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

type NativeConditionalExpression = NativeConditionalExpression {
  condition: NativeExpr,
  when_true: NativeExpr,
  when_false: NativeExpr
}

type NativeExpr =
  | NativeInteger(IntegerLiteral)
  | NativeBoolean(bool)
  | NativeBinary(NativeBinaryExpression)
  | NativeConditional(NativeConditionalExpression)
  | NativeNegation(NativeExpr)

type BooleanMatchCases = BooleanMatchCases {
  true_expression: Option<NativeExpr>,
  false_expression: Option<NativeExpr>
}

type CheckedExpression = CheckedExpression {
  inferred_type: ValueType,
  native_expression: NativeExpr
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

let is_i32 = fn value_type ->
  match value_type {
    | I32 -> true
    | _ -> false
  }

let make_i32_binary = fn operator -> fn left_expression -> fn right_expression ->
  match (is_i32 (checked_expression_type left_expression)) {
    | false -> Err("integer arithmetic requires two i32 operands")
    | true -> match (is_i32 (checked_expression_type right_expression)) {
      | false -> Err("integer arithmetic requires two i32 operands")
      | true -> Ok(CheckedExpression {
        inferred_type: I32,
        native_expression: make_native_binary operator left_expression right_expression
      })
    }
  }

let make_i32_comparison = fn operator -> fn left_expression -> fn right_expression ->
  match (is_i32 (checked_expression_type left_expression)) {
    | false -> Err("integer comparisons require two i32 operands")
    | true -> match (is_i32 (checked_expression_type right_expression)) {
      | false -> Err("integer comparisons require two i32 operands")
      | true -> Ok(CheckedExpression {
        inferred_type: BoolType,
        native_expression: make_native_binary operator left_expression right_expression
      })
    }
  }

let make_native_binary = fn operator -> fn left_expression -> fn right_expression ->
  NativeBinary(NativeBinaryExpression {
    operator: operator,
    left: checked_expression_native left_expression,
    right: checked_expression_native right_expression
  })

let check_binary = fn (BinaryExpression { operator, left, right }) ->
  match (check_expression left) {
    | Err(message) -> Err(message)
    | Ok(checked_left) -> match (check_expression right) {
      | Err(message) -> Err(message)
      | Ok(checked_right) -> checked_binary_result
        (native_operator operator) checked_left checked_right
    }
  }

let require_i32 = fn error_message -> fn expression ->
  match (check_expression expression) {
    | Err(message) -> Err(message)
    | Ok(CheckedExpression {
        inferred_type: I32,
        native_expression
      }) -> Ok(native_expression)
    | Ok(_) -> Err(error_message)
  }

let check_boolean_match = fn (MatchExpression { scrutinee, arms }) ->
  match (check_expression scrutinee) {
    | Err(message) -> Err(message)
    | Ok(CheckedExpression {
        inferred_type: BoolType,
        native_expression: condition
      }) -> match (check_boolean_arms (BooleanMatchCases {
        true_expression: None,
        false_expression: None
      }) arms) {
      | Err(message) -> Err(message)
      | Ok(cases) -> finish_boolean_match condition cases
    }
    | Ok(_) -> Err("boolean match requires a bool scrutinee")
  }

let check_boolean_arms = fn (BooleanMatchCases {
  true_expression,
  false_expression
}) -> fn arms ->
  match arms {
    | Nil -> Ok(BooleanMatchCases {
      true_expression: true_expression,
      false_expression: false_expression
    })
    | Cons(arm, remaining) -> check_boolean_arm
      (BooleanMatchCases {
        true_expression: true_expression,
        false_expression: false_expression
      }) arm remaining
  }

let check_boolean_arm = fn cases -> fn (MatchArm { pattern, body }) -> fn remaining ->
  match pattern {
    | PatternBoolean(true) -> check_true_arm cases body remaining
    | PatternBoolean(false) -> check_false_arm cases body remaining
    | _ -> Err("native boolean match accepts only `true` and `false` arms")
  }

let check_true_arm = fn (BooleanMatchCases {
  true_expression,
  false_expression
}) -> fn body -> fn remaining ->
  match true_expression {
    | Some(_) -> Err("duplicate `true` match arm")
    | None -> match (require_i32 "native match branches must return i32" body) {
      | Err(message) -> Err(message)
      | Ok(checked_body) -> check_boolean_arms (BooleanMatchCases {
        true_expression: Some(checked_body),
        false_expression: false_expression
      }) remaining
    }
  }

let check_false_arm = fn (BooleanMatchCases {
  true_expression,
  false_expression
}) -> fn body -> fn remaining ->
  match false_expression {
    | Some(_) -> Err("duplicate `false` match arm")
    | None -> match (require_i32 "native match branches must return i32" body) {
      | Err(message) -> Err(message)
      | Ok(checked_body) -> check_boolean_arms (BooleanMatchCases {
        true_expression: true_expression,
        false_expression: Some(checked_body)
      }) remaining
    }
  }

let finish_boolean_match = fn condition -> fn (BooleanMatchCases {
  true_expression,
  false_expression
}) ->
  match true_expression {
    | None -> Err("native boolean match is missing a `true` arm")
    | Some(when_true) -> match false_expression {
      | None -> Err("native boolean match is missing a `false` arm")
      | Some(when_false) -> Ok(CheckedExpression {
        inferred_type: I32,
        native_expression: NativeConditional(NativeConditionalExpression {
          condition: condition,
          when_true: when_true,
          when_false: when_false
        })
      })
    }
  }

let check_expression = fn expression ->
  match expression {
    | Integer(literal) -> Ok(CheckedExpression {
      inferred_type: I32,
      native_expression: NativeInteger(literal)
    })
    | Boolean(value) -> Ok(CheckedExpression {
      inferred_type: BoolType,
      native_expression: NativeBoolean(value)
    })
    | Binary(binary) -> check_binary binary
    | Match(match_expression) -> check_boolean_match match_expression
    | Negate(operand) -> match (require_i32 "unary `-` requires an i32 operand" operand) {
      | Err(message) -> Err(message)
      | Ok(checked_operand) -> Ok(CheckedExpression {
        inferred_type: I32,
        native_expression: NativeNegation(checked_operand)
      })
    }
    | _ -> Err("expression is not supported by the native compiler slice")
  }

let check_program = fn program ->
  match program {
    | Main(expression) -> match (check_expression expression) {
      | Err(message) -> Err(message)
      | Ok(CheckedExpression {
          inferred_type: I32,
          native_expression: native_expression
        }) -> Ok(CheckedProgram(native_expression))
      | Ok(_) -> Err("native entry point must return i32")
    }
  }
