module Jir = Types

let rec is_pure_expr = fun expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Identifier _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _
  | Jir.Expr.Function _ ->
      true
  | Jir.Expr.Member member ->
      is_pure_expr member.object_
  | Jir.Expr.Call _
  | Jir.Expr.Assignment _ ->
      false
  | Jir.Expr.Conditional conditional ->
      if is_pure_expr conditional.condition then
        if is_pure_expr conditional.then_ then
          is_pure_expr conditional.else_
        else
          false
      else
        false
