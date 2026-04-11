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
