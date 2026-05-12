open Std
module Core = Raml_core.Core_ir
module Jir = Types
module Analysis = Analysis
module Simplify = Simplify

module Binding_map = Collections.Map.Make (struct
  type t = Core.Binding_id.t

  let compare = Core.Binding_id.compare
end)

module Entity_set = Analysis.Entity_set

type env = {
  aliases: Core.Entity_id.t Binding_map.t;
  assigned: Entity_set.t;
  exported: Entity_set.t;
}

let resolve_alias = fun env entity ->
  let rec loop seen entity =
    if Entity_set.mem entity seen then
      entity
    else
      match Core.Entity_id.binding_id entity with
      | None -> entity
      | Some binding_id -> (
          match Binding_map.get env.aliases ~key:binding_id with
          | Some target -> loop (Entity_set.add entity seen) target
          | None -> entity
        )
  in
  loop Entity_set.empty entity

let bind_alias = fun env alias target ->
  { env with aliases = Binding_map.insert env.aliases ~key:alias ~value:target }

let rec lower_array_element = fun env element ->
  match element with
  | Jir.Expr.Item expr -> Jir.Expr.Item (lower_expr env expr)
  | Jir.Expr.Spread expr -> Jir.Expr.Spread (lower_expr env expr)

and lower_object_field = fun env (field: Jir.Expr.object_field) ->
  Jir.Expr.{ field with value = lower_expr env field.value }

and lower_expr = fun env expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Global _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ -> expr
  | Jir.Expr.Identifier entity -> Jir.Expr.Identifier (resolve_alias env entity)
  | Jir.Expr.Unary unary -> Jir.Expr.Unary Jir.Expr.{
    unary
    with operand = lower_expr env unary.operand
  }
  | Jir.Expr.Binary binary -> Jir.Expr.Binary Jir.Expr.{
    binary
    with left = lower_expr env binary.left;
    right = lower_expr env binary.right
  }
  | Jir.Expr.Array elements -> Jir.Expr.Array (List.map elements ~fn:(lower_array_element env))
  | Jir.Expr.Object fields -> Jir.Expr.Object (List.map fields ~fn:(lower_object_field env))
  | Jir.Expr.Function function_ -> Jir.Expr.Function Jir.Expr.{
    function_
    with body = lower_scoped_block env function_.body
  }
  | Jir.Expr.Member member -> Jir.Expr.Member Jir.Expr.{
    member
    with object_ = lower_expr env member.object_
  }
  | Jir.Expr.Index index -> Jir.Expr.Index Jir.Expr.{
    object_ = lower_expr env index.object_;
    index = lower_expr env index.index
  }
  | Jir.Expr.Call call -> Jir.Expr.Call Jir.Expr.{
    callee = lower_expr env call.callee;
    arguments = List.map call.arguments ~fn:(lower_expr env)
  }
  | Jir.Expr.Conditional conditional -> Jir.Expr.Conditional Jir.Expr.{
    condition = lower_expr env conditional.condition;
    then_ = lower_expr env conditional.then_;
    else_ = lower_expr env conditional.else_
  }
  | Jir.Expr.Assignment assignment -> Jir.Expr.Assignment Jir.Expr.{
    assignment
    with value = lower_expr env assignment.value
  }

and lower_statement = fun env statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      lower_declaration env declaration
  | Jir.Statement.Block statements ->
      (lower_scoped_block env statements |> Simplify.block, env)
  | Jir.Statement.Expression expr ->
      let expr = lower_expr env expr in
      (Simplify.effect_expression expr, env)
  | Jir.Statement.Return expr ->
      ([ Jir.Statement.Return (lower_expr env expr) ], env)
  | Jir.Statement.If if_ ->
      let condition = lower_expr env if_.condition in
      let then_ = lower_scoped_block env if_.then_ in
      let else_ = lower_scoped_block env if_.else_ in
      (Simplify.conditional ~condition ~then_ ~else_, env)

and lower_declaration = fun env (declaration: Jir.Declaration.t) ->
  let init = Option.map declaration.init ~fn:(lower_expr env) in
  let binder_entity = Jir.Binder.entity_id declaration.binder in
  match (declaration.kind, init) with
  | (Jir.Declaration.Const, Some (Jir.Expr.Identifier target)) when not
    (Core.Entity_id.equal binder_entity target)
  && not (Entity_set.mem binder_entity env.exported)
  && not (Entity_set.mem target env.assigned) -> (
    [],
    bind_alias env declaration.binder.binding_id target
  )
  | _ -> ([ Jir.Statement.Declaration Jir.Declaration.{ declaration with init } ], env)

and lower_block = fun env statements ->
  match statements with
  | [] -> ([], env)
  | statement :: rest ->
      let (statement, env) = lower_statement env statement in
      let (rest, env) = lower_block env rest in
      (statement @ rest, env)

and lower_scoped_block = fun env statements ->
  let lowered, _ = lower_block env statements in
  lowered

let program = fun ~context:_ (program: Jir.Program.t) ->
  let env = {
    aliases = Binding_map.empty;
    assigned = Analysis.program_assigned_entities program;
    exported =
      List.fold_left program.exports ~init:Entity_set.empty
        ~fn:(fun set (export: Jir.Export.t) ->
          Entity_set.add export.local set);
  }
  in
  let (body, _) = lower_block env program.body in
  { program with body }
