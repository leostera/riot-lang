(** [NIR] normalization makes the tree easier for later native passes to work
    with. It lifts nested [let] bindings out of call positions and conditions,
    flattens adjacent [let] chains, and generally pushes the IR toward a more
    ANF-like shape. The point is not to change runtime meaning; it is to make
    later simplification and lowering passes reason about one regular shape
    instead of many incidental ones. *)
open Std
module Program = Types.Program
module Expr = Types.Expr
module Function = Types.Function
module Binding = Types.Binding
module Entry_item = Types.Entry_item

let make_let = fun bindings body ->
  if List.is_empty bindings then
    body
  else
    Expr.Let Expr.{ bindings; body }

let rec lift_let = fun expr ->
  match expr with
  | Expr.Let let_ -> (let_.bindings, let_.body)
  | expr -> ([], expr)

and normalize_call = fun (call: Expr.call) ->
  let callee_bindings, callee =
    match normalize_callee call.callee with
    | Expr.Direct name -> ([], Expr.Direct name)
    | Expr.Indirect expr ->
        let bindings, expr = lift_let expr in
        (bindings, Expr.Indirect expr)
  in
  let argument_bindings, arguments =
    List.fold_left call.arguments ~init:([], [])
      ~fn:(fun (bindings_acc, arguments_acc) argument ->
        let bindings, argument = normalize_expr argument |> lift_let in
        (bindings_acc @ bindings, arguments_acc @ [ argument ]))
  in
  make_let (callee_bindings @ argument_bindings) (Expr.Call Expr.{ callee; arguments })

and normalize_callee = fun callee ->
  match callee with
  | Expr.Direct _ -> callee
  | Expr.Indirect expr -> Expr.Indirect (normalize_expr expr)

and normalize_if_then_else = fun (if_then_else: Expr.if_then_else) ->
  let condition_bindings, condition = normalize_expr if_then_else.condition |> lift_let in
  let then_ = normalize_expr if_then_else.then_ in
  let else_ = normalize_expr if_then_else.else_ in
  make_let condition_bindings (Expr.If_then_else Expr.{ condition; then_; else_ })

and normalize_binding = fun (binding: Expr.binding) ->
  let bindings, expr = normalize_expr binding.expr |> lift_let in
  let binding = Expr.{ name = binding.name; expr } in
  (bindings @ [ binding ])

and normalize_let = fun (let_: Expr.let_) ->
  let bindings = List.flat_map let_.bindings ~fn:normalize_binding in
  let body = normalize_expr let_.body in
  let body_bindings, body = lift_let body in
  make_let (bindings @ body_bindings) body

and normalize_expr = fun expr ->
  match expr with
  | Expr.Literal _
  | Expr.Symbol _
  | Expr.Symbol_address _ -> expr
  | Expr.Call call -> normalize_call call
  | Expr.If_then_else if_then_else -> normalize_if_then_else if_then_else
  | Expr.Let let_ -> normalize_let let_

let normalize_function = fun (function_: Function.t) ->
  { function_ with body = normalize_expr function_.body }

let normalize_binding_item = fun (binding: Binding.t) ->
  { binding with expr = normalize_expr binding.expr }

let normalize_entry_item = fun entry_item ->
  match entry_item with
  | Entry_item.Binding binding -> Entry_item.Binding (normalize_binding_item binding)
  | Entry_item.Eval expr -> Entry_item.Eval (normalize_expr expr)

let program = fun (program: Program.t) ->
  {
    program
    with functions = List.map program.functions ~fn:normalize_function;
    entry = List.map program.entry ~fn:normalize_entry_item
  }
