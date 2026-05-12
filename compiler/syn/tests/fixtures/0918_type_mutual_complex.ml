type expr =
  Value of int
  | BinOp of binop

and binop = {
  op: operator;
  left: expr;
  right: expr;
}

and operator =
  Add
  | Mul
