open Std
module Jir = Types
module Analysis = Analysis

let block = fun statements ->
  match statements with
  | [] -> []
  | statements -> [ Jir.Statement.Block statements ]

let effect_expression = fun expr ->
  if Analysis.is_pure_expr expr then
    []
  else
    [ Jir.Statement.Expression expr ]

let conditional = fun ~condition ~then_ ~else_ ->
  if List.is_empty then_ then
    if List.is_empty else_ then
      effect_expression condition
    else
      [ Jir.Statement.If Jir.Statement.{ condition; then_; else_ } ]
  else
    [ Jir.Statement.If Jir.Statement.{ condition; then_; else_ } ]

let rec function_body = fun statements ->
  match List.rev statements with
  | [] -> []
  | tail :: prefix_rev -> List.rev prefix_rev @ simplify_function_tail tail

and simplify_function_tail = fun statement ->
  match statement with
  | Jir.Statement.Return (Jir.Expr.Literal Jir.Literal.Undefined) ->
      []
  | Jir.Statement.Block statements ->
      function_body statements |> block
  | Jir.Statement.If if_ ->
      let then_ = function_body if_.then_ in
      let else_ = function_body if_.else_ in
      conditional ~condition:if_.condition ~then_ ~else_
  | statement ->
      [ statement ]
