// Lower checked RiotML syntax into the native Lambda IR.
use Syntax.Ast.{Program, Integer, Boolean, Add, Subtract, Multiply, Divide, Negate, Main}
use Lambda.Ir.{LambdaInteger, LambdaBoolean, LambdaAdd, LambdaSubtract, LambdaMultiply, LambdaDivide, LambdaNegate, Entry}

let lower_expr = fn expression ->
  match expression {
    | Integer(value) -> LambdaInteger(value)
    | Boolean(value) -> LambdaBoolean(value)
    | Add(left, right) -> LambdaAdd(lower_expr left, lower_expr right)
    | Subtract(left, right) -> LambdaSubtract(lower_expr left, lower_expr right)
    | Multiply(left, right) -> LambdaMultiply(lower_expr left, lower_expr right)
    | Divide(left, right) -> LambdaDivide(lower_expr left, lower_expr right)
    | Negate(operand) -> LambdaNegate(lower_expr operand)
  }

let lower_program = fn program ->
  match program {
    | Main(expression) -> Entry(lower_expr expression)
  }
