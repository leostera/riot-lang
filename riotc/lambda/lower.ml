// Lower the checker's restricted native core into Lambda IR.
use Check.Typer.{
  NativeExpr, NativeOperator, NativeBinaryExpression, CheckedProgram,
  NativeInteger, NativeBoolean, NativeBinary, NativeNegation,
  NativeAddition, NativeSubtraction, NativeMultiplication, NativeDivision,
  NativeRemainder, NativeEqual, NativeNotEqual, NativeLessThan, NativeSource,
  NativeLessThanOrEqual, NativeGreaterThan, NativeGreaterThanOrEqual
}
use Lambda.Ir.{
  IntegerOperator, LambdaBinaryExpression, LambdaInteger, LambdaBoolean,
  LambdaBinary,
  LambdaNegation, AddI32, SubtractI32, MultiplyI32, DivideI32,
  RemainderI32, EqualI32, NotEqualI32, LessThanI32, LessThanOrEqualI32,
  GreaterThanI32, GreaterThanOrEqualI32, Entry, LambdaSource
}

let lower_operator = fn operator ->
  match operator {
    | NativeAddition -> AddI32
    | NativeSubtraction -> SubtractI32
    | NativeMultiplication -> MultiplyI32
    | NativeDivision -> DivideI32
    | NativeRemainder -> RemainderI32
    | NativeEqual -> EqualI32
    | NativeNotEqual -> NotEqualI32
    | NativeLessThan -> LessThanI32
    | NativeLessThanOrEqual -> LessThanOrEqualI32
    | NativeGreaterThan -> GreaterThanI32
    | NativeGreaterThanOrEqual -> GreaterThanOrEqualI32
  }

let lower_binary = fn lower_child -> fn (NativeBinaryExpression {
  operator,
  left,
  right
}) ->
  LambdaBinary(LambdaBinaryExpression {
    operator: lower_operator operator,
    left: lower_child left,
    right: lower_child right
  })

let lower_expression = fn expression ->
  match expression {
    | NativeInteger(value) -> LambdaInteger(value)
    | NativeBoolean(value) -> LambdaBoolean(value)
    | NativeBinary(binary) -> lower_binary lower_expression binary
    | NativeNegation(operand) -> LambdaNegation(lower_expression operand)
    | NativeSource(expression) -> LambdaSource(expression)
  }

let lower_program = fn checked_program ->
  match checked_program {
    | CheckedProgram(expression) -> Entry(lower_expression expression)
  }
