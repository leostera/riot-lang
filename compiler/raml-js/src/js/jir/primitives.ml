open Std

module Core = Raml_core.Core_ir
module Jir = Types

let lower_global = fun name -> Jir.Expr.Global Jir.Expr.{ name }

let lower_member = fun object_ property -> Jir.Expr.Member Jir.Expr.{ object_; property }

let lower_index = fun object_ index -> Jir.Expr.Index Jir.Expr.{ object_; index }

let lower_call = fun callee arguments -> Jir.Expr.Call Jir.Expr.{ callee; arguments }

let lower_binary = fun operator left right -> Jir.Expr.Binary Jir.Expr.{ operator; left; right }

let lower_array = fun elements -> Jir.Expr.Array (List.map (fun expr -> Jir.Expr.Item expr) elements)

let lower_string_constructor = fun value -> lower_call (lower_global "String") [ value ]

let lower_console_log = fun arguments -> lower_call (lower_member (lower_global "console") "log") arguments

let lower_runtime_call = fun primitive arguments ->
  let callee = Jir.Expr.Runtime_helper (Jir.Runtime.call_primitive ()) in
  let arguments =
    Jir.Expr.Literal (Jir.Literal.String (Core.Primitive.to_string primitive)) :: arguments
  in
  lower_call callee arguments

let lower = fun primitive arguments ->
  match (primitive, arguments) with
  | (Core.Primitive.Tuple_make, arguments) -> lower_array arguments
  | (Core.Primitive.Tuple_get, [tuple;index]) -> lower_index tuple index
  | (Core.Primitive.Add_float, [left;right])
  | (Core.Primitive.Add_int, [left;right]) -> lower_binary Jir.Operator.Add left right
  | (Core.Primitive.Subtract_float, [left;right])
  | (Core.Primitive.Subtract_int, [left;right]) -> lower_binary Jir.Operator.Subtract left right
  | (Core.Primitive.Multiply_float, [left;right])
  | (Core.Primitive.Multiply_int, [left;right]) -> lower_binary Jir.Operator.Multiply left right
  | (Core.Primitive.Divide_float, [left;right])
  | (Core.Primitive.Divide_int, [left;right]) -> lower_binary Jir.Operator.Divide left right
  | (Core.Primitive.Modulo_int, [left;right]) -> lower_binary Jir.Operator.Modulo left right
  | (Core.Primitive.Equal, [left;right]) -> lower_binary Jir.Operator.Equal left right
  | (Core.Primitive.Not_equal, [left;right]) -> lower_binary Jir.Operator.Not_equal left right
  | (Core.Primitive.Less_than, [left;right]) -> lower_binary Jir.Operator.Less_than left right
  | (Core.Primitive.Less_or_equal, [left;right]) -> lower_binary Jir.Operator.Less_or_equal left right
  | (Core.Primitive.Greater_than, [left;right]) -> lower_binary Jir.Operator.Greater_than left right
  | (Core.Primitive.Greater_or_equal, [left;right]) -> lower_binary Jir.Operator.Greater_or_equal left right
  | (Core.Primitive.Concatenate_string, [left;right]) -> lower_binary
    Jir.Operator.Add
    (lower_string_constructor left)
    (lower_string_constructor right)
  | (Core.Primitive.Int_to_string, [ value ])
  | (Core.Primitive.Float_to_string, [ value ]) -> lower_string_constructor value
  | (Core.Primitive.Float_sqrt, [ value ]) ->
      lower_call (lower_member (lower_global "Math") "sqrt") [ value ]
  | (Core.Primitive.Trace, [ value ]) -> lower_console_log [ value ]
  | _ -> lower_runtime_call primitive arguments
