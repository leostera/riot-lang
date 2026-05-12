open Std
module Types = Types
module Expr = Types.Expr
module Init_item = Types.Init_item

let is_unit_constant = fun expr ->
  match expr with
  | Expr.Constant Raml_core.Core_ir.Constant.Unit -> true
  | _ -> false

let rec normalize_expr = fun expr ->
  match expr with
  | Expr.Constant _
  | Expr.Var _ ->
      expr
  | Expr.Direct_call call ->
      Expr.Direct_call { call with arguments = List.map call.arguments ~fn:normalize_expr }
  | Expr.Indirect_call call ->
      Expr.Indirect_call {
        callee = normalize_expr call.callee;
        arguments = List.map call.arguments ~fn:normalize_expr
      }
  | Expr.Lambda lambda ->
      Expr.Lambda { lambda with body = normalize_expr lambda.body }
  | Expr.Let let_ ->
      Expr.Let {
        let_
        with bindings = List.map let_.bindings ~fn:normalize_binding;
        body = normalize_expr let_.body
      }
  | Expr.Sequence sequence ->
      let first = normalize_expr sequence.first in
      let second = normalize_expr sequence.second in
      if is_unit_constant first then
        second
      else
        Expr.Sequence { first; second }
  | Expr.Tuple elements ->
      Expr.Tuple (List.map elements ~fn:normalize_expr)
  | Expr.Tuple_get tuple_get ->
      Expr.Tuple_get { tuple_get with tuple = normalize_expr tuple_get.tuple }
  | Expr.If_then_else if_then_else ->
      let condition = normalize_expr if_then_else.condition in
      let then_ = normalize_expr if_then_else.then_ in
      let else_ = normalize_expr if_then_else.else_ in
      begin
        match condition with
        | Expr.Constant (Raml_core.Core_ir.Constant.Bool true) -> then_
        | Expr.Constant (Raml_core.Core_ir.Constant.Bool false) -> else_
        | _ -> Expr.If_then_else { condition; then_; else_ }
      end
  | Expr.Primitive primitive ->
      Expr.Primitive { primitive with arguments = List.map primitive.arguments ~fn:normalize_expr }

and normalize_binding = fun (binding: Types.Expr.binding) ->
  { binding with expr = normalize_expr binding.expr }

let normalize_global = fun (global: Types.Global.t) ->
  { global with expr = normalize_expr global.expr }

let normalize_function = fun (function_: Types.Function.t) ->
  { function_ with body = normalize_expr function_.body }

let normalize_init_item = fun item ->
  match item with
  | Init_item.Global global -> Some (Init_item.Global (normalize_global global))
  | Init_item.Eval expr ->
      let expr = normalize_expr expr in
      if is_unit_constant expr then
        None
      else
        Some (Init_item.Eval expr)

let program = fun (program: Types.Compilation_unit.t) ->
  {
    program
    with globals = List.map program.globals ~fn:normalize_global;
    functions = List.map program.functions ~fn:normalize_function;
    init = List.filter_map program.init ~fn:normalize_init_item
  }
