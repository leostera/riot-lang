(** This pass trims obviously redundant structure from [NIR] before instruction
    selection. It walks the expression tree, folds boolean-literal conditionals,
    removes dead pure [let] bindings, collapses [let x = expr in x], and drops
    pure entry-side [Eval] items. The effect is intentionally conservative:
    keep the native-runtime shape, but stop carrying tree structure that every
    later stage would just delete again. *)
open Std
module Nir = Types

let rec expr_uses_name = fun name expr ->
  match expr with
  | Nir.Expr.Literal _ -> false
  | Nir.Expr.Symbol symbol -> String.equal symbol name
  | Nir.Expr.Symbol_address _ -> false
  | Nir.Expr.Call { callee; arguments } -> callee_uses_name name callee
  || List.exists (expr_uses_name name) arguments
  | Nir.Expr.If_then_else if_then_else -> expr_uses_name name if_then_else.condition
  || expr_uses_name name if_then_else.then_
  || expr_uses_name name if_then_else.else_
  | Nir.Expr.Let let_ -> List.exists
    (fun (binding: Nir.Expr.binding) -> expr_uses_name name binding.expr)
    let_.bindings
  || expr_uses_name name let_.body

and callee_uses_name = fun name callee ->
  match callee with
  | Nir.Expr.Direct _ -> false
  | Nir.Expr.Indirect expr -> expr_uses_name name expr

let rec expr_is_pure = fun expr ->
  match expr with
  | Nir.Expr.Literal _ -> true
  | Nir.Expr.Symbol _ -> true
  | Nir.Expr.Symbol_address _ -> true
  | Nir.Expr.Call _ -> false
  | Nir.Expr.If_then_else if_then_else -> expr_is_pure if_then_else.condition
  && expr_is_pure if_then_else.then_
  && expr_is_pure if_then_else.else_
  | Nir.Expr.Let let_ -> List.for_all
    (fun (binding: Nir.Expr.binding) -> expr_is_pure binding.expr)
    let_.bindings
  && expr_is_pure let_.body

let simplify_literal = fun literal -> Nir.Expr.Literal literal

let rec simplify_expr = fun expr ->
  match expr with
  | Nir.Expr.Literal literal ->
      simplify_literal literal
  | Nir.Expr.Symbol _ ->
      expr
  | Nir.Expr.Symbol_address _ ->
      expr
  | Nir.Expr.Call call ->
      Nir.Expr.Call {
        callee = simplify_callee call.callee;
        arguments = List.map call.arguments ~fn:simplify_expr
      }
  | Nir.Expr.If_then_else if_then_else ->
      let condition = simplify_expr if_then_else.condition in
      let then_ = simplify_expr if_then_else.then_ in
      let else_ = simplify_expr if_then_else.else_ in
      (
        match condition with
        | Nir.Expr.Literal (Nir.Literal.Bool true) -> then_
        | Nir.Expr.Literal (Nir.Literal.Bool false) -> else_
        | _ -> Nir.Expr.If_then_else { condition; then_; else_ }
      )
  | Nir.Expr.Let let_ ->
      simplify_let let_

and simplify_callee = fun callee ->
  match callee with
  | Nir.Expr.Direct _ -> callee
  | Nir.Expr.Indirect expr -> Nir.Expr.Indirect (simplify_expr expr)

and simplify_binding = fun (binding: Nir.Expr.binding) ->
  Nir.Expr.{ binding with expr = simplify_expr binding.expr }

and simplify_let = fun (let_: Nir.Expr.let_) ->
  let bindings = List.map let_.bindings ~fn:simplify_binding in
  let body = simplify_expr let_.body in
  let bindings =
    List.filter
      bindings
      ~fn:(fun (binding: Nir.Expr.binding) ->
        if expr_uses_name binding.name body then
          true
        else
          not (expr_is_pure binding.expr))
  in
  match (bindings, body) with
  | ([], body) -> body
  | ([ binding ], Nir.Expr.Symbol symbol) when String.equal binding.name symbol -> binding.expr
  | _ -> Nir.Expr.Let Nir.Expr.{ bindings; body }

let simplify_function = fun (function_: Nir.Function.t) ->
  Nir.Function.{ function_ with body = simplify_expr function_.body }

let simplify_binding_item = fun (binding: Nir.Binding.t) ->
  Nir.Binding.{ binding with expr = simplify_expr binding.expr }

let simplify_entry_item = fun entry_item ->
  match entry_item with
  | Nir.Entry_item.Binding binding -> Some (Nir.Entry_item.Binding (simplify_binding_item binding))
  | Nir.Entry_item.Eval expr ->
      let expr = simplify_expr expr in
      if expr_is_pure expr then
        None
      else
        Some (Nir.Entry_item.Eval expr)

let program = fun (program: Nir.Program.t) ->
  {
    program
    with functions = List.map program.functions ~fn:simplify_function;
    entry = List.filter_map program.entry ~fn:simplify_entry_item
  }
