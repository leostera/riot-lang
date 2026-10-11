// Typed Lambda IR for the native C backend.
use Syntax.Ast.{IntegerLiteral, Expr}

type IntegerOperator =
  | AddI32
  | SubtractI32
  | MultiplyI32
  | DivideI32
  | RemainderI32
  | EqualI32
  | NotEqualI32
  | LessThanI32
  | LessThanOrEqualI32
  | GreaterThanI32
  | GreaterThanOrEqualI32

type LambdaBinaryExpression = LambdaBinaryExpression {
  operator: IntegerOperator,
  left: LambdaExpr,
  right: LambdaExpr
}

type LambdaExpr =
  | LambdaInteger(IntegerLiteral)
  | LambdaBoolean(bool)
  | LambdaBinary(LambdaBinaryExpression)
  | LambdaNegation(LambdaExpr)
  | LambdaSource(Expr)

type LambdaModule =
  | Entry(LambdaExpr)
