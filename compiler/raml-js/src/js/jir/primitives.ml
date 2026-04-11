open Std
module Core = Raml_core.Core_ir
module Jir = Types
module Intrinsics = Intrinsics

let lower_runtime_call = fun primitive arguments ->
  let callee = Jir.Expr.Runtime_helper (Jir.Runtime.call_primitive ()) in
  let arguments = Jir.Expr.Literal (Jir.Literal.String (Core.Primitive.to_string primitive)) :: arguments in
  Intrinsics.call callee arguments

let lower = fun primitive arguments ->
  match (primitive, arguments) with
  | (Core.Primitive.Tuple_make, arguments) -> Intrinsics.array arguments
  | (Core.Primitive.Tuple_get, [tuple;index]) -> Intrinsics.index tuple index
  | (Core.Primitive.Add_float, [left;right])
  | (Core.Primitive.Add_int, [left;right]) -> Intrinsics.binary Jir.Operator.Add left right
  | (Core.Primitive.Subtract_float, [left;right])
  | (Core.Primitive.Subtract_int, [left;right]) -> Intrinsics.binary Jir.Operator.Subtract left right
  | (Core.Primitive.Multiply_float, [left;right])
  | (Core.Primitive.Multiply_int, [left;right]) -> Intrinsics.binary Jir.Operator.Multiply left right
  | (Core.Primitive.Divide_float, [left;right])
  | (Core.Primitive.Divide_int, [left;right]) -> Intrinsics.binary Jir.Operator.Divide left right
  | (Core.Primitive.Modulo_int, [left;right]) -> Intrinsics.binary Jir.Operator.Modulo left right
  | (Core.Primitive.Equal, [left;right]) -> Intrinsics.binary Jir.Operator.Equal left right
  | (Core.Primitive.Not_equal, [left;right]) -> Intrinsics.binary Jir.Operator.Not_equal left right
  | (Core.Primitive.Less_than, [left;right]) -> Intrinsics.binary Jir.Operator.Less_than left right
  | (Core.Primitive.Less_or_equal, [left;right]) -> Intrinsics.binary
    Jir.Operator.Less_or_equal
    left
    right
  | (Core.Primitive.Greater_than, [left;right]) -> Intrinsics.binary Jir.Operator.Greater_than left right
  | (Core.Primitive.Greater_or_equal, [left;right]) -> Intrinsics.binary
    Jir.Operator.Greater_or_equal
    left
    right
  | (Core.Primitive.Concatenate_string, [left;right]) -> Intrinsics.binary
    Jir.Operator.Add
    (Intrinsics.string_constructor left)
    (Intrinsics.string_constructor right)
  | (Core.Primitive.Int_to_string, [ value ])
  | (Core.Primitive.Float_to_string, [ value ]) -> Intrinsics.string_constructor value
  | (Core.Primitive.Float_sqrt, [ value ]) -> Intrinsics.math_sqrt value
  | (Core.Primitive.Trace, [ value ]) -> Intrinsics.console_log [ value ]
  | _ -> lower_runtime_call primitive arguments
