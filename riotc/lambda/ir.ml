// Typed first-order Lambda IR consumed by native code generation.

type LambdaExpr =
  | LambdaInteger(string)
  | LambdaBoolean(bool)
  | LambdaAdd(LambdaExpr, LambdaExpr)
  | LambdaSubtract(LambdaExpr, LambdaExpr)
  | LambdaMultiply(LambdaExpr, LambdaExpr)
  | LambdaDivide(LambdaExpr, LambdaExpr)
  | LambdaNegate(LambdaExpr)

type LambdaModule =
  | Entry(LambdaExpr)
