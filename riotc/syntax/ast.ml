// Syntax nodes for the first native compiler slice.

type Expr =
  | Integer(string)
  | Boolean(bool)
  | Add(Expr, Expr)
  | Subtract(Expr, Expr)
  | Multiply(Expr, Expr)
  | Divide(Expr, Expr)
  | Negate(Expr)

type Program =
  | Main(Expr)

type ValueType =
  | I32
  | BoolType
