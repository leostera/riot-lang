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
      Expr.Direct_call { call with arguments = List.map normalize_expr call.arguments }
  | Expr.Indirect_call call ->
      Expr.Indirect_call {
        callee = normalize_expr call.callee;
        arguments = List.map normalize_expr call.arguments
      }
  | Expr.Lambda lambda ->
      Expr.Lambda { lambda with body = normalize_expr lambda.body }
  | Expr.Let let_ ->
      Expr.Let {
        let_
        with bindings = List.map normalize_binding let_.bindings;
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
      Expr.Tuple (List.map normalize_expr elements)
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
      Expr.Primitive { primitive with arguments = List.map normalize_expr primitive.arguments }

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
    with globals = List.map normalize_global program.globals;
    functions = List.map normalize_function program.functions;
    init = List.filter_map normalize_init_item program.init
  }
