type expr =
  Num of int
  | Op of operator * expr * expr

and operator =
  Add
  | Sub
  | Mul
  | Div
