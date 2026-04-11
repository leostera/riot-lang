module Jir = Types
module Intrinsics = Intrinsics
module Syntax = Syntax

let string_literal = fun value -> Jir.Expr.Literal (Jir.Literal.String value)

let field = fun name value -> Jir.Expr.{ name; value }

let literal = fun fields -> Jir.Expr.Object fields

let named_access = fun object_ property ->
  if Syntax.can_use_dot_property property then
    Intrinsics.member object_ property
  else
    Intrinsics.index object_ (string_literal property)
