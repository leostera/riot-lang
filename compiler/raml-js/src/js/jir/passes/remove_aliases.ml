open Std
module Core = Raml_core.Core_ir
module Jir = Types
module Analysis = Analysis

module Binding_map = Collections.Map.Make (struct
  type t = Core.Binding_id.t
  let compare = Core.Binding_id.compare
end)

module Entity_set = struct
  module Storage = Collections.Map.Make (struct
    type t = Core.Entity_id.t
    let compare = Core.Entity_id.compare
  end)

  type t = unit Storage.t

  let empty = Storage.empty

  let add = fun entity set -> Storage.add entity () set

  let mem = Storage.mem
end

type env = {
  aliases: Core.Entity_id.t Binding_map.t;
  assigned: Entity_set.t;
  exported: Entity_set.t;
}

let rec collect_expr_assigned_entities = fun entities expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Identifier _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      entities
  | Jir.Expr.Function function_ ->
      collect_statement_assigned_entities entities function_.body
  | Jir.Expr.Member member ->
      collect_expr_assigned_entities entities member.object_
  | Jir.Expr.Call call ->
      let entities = collect_expr_assigned_entities entities call.callee in
      collect_expr_list_assigned_entities entities call.arguments
  | Jir.Expr.Conditional conditional ->
      let entities = collect_expr_assigned_entities entities conditional.condition in
      let entities = collect_expr_assigned_entities entities conditional.then_ in
      collect_expr_assigned_entities entities conditional.else_
  | Jir.Expr.Assignment assignment ->
      collect_expr_assigned_entities (Entity_set.add assignment.target entities) assignment.value

and collect_expr_list_assigned_entities = fun entities exprs ->
  match exprs with
  | [] -> entities
  | expr :: rest ->
      let entities = collect_expr_assigned_entities entities expr in
      collect_expr_list_assigned_entities entities rest

and collect_statement_assigned_entities = fun entities statements ->
  match statements with
  | [] -> entities
  | statement :: rest ->
      let entities = collect_one_statement_assigned_entities entities statement in
      collect_statement_assigned_entities entities rest

and collect_one_statement_assigned_entities = fun entities statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      Option.map (collect_expr_assigned_entities entities) declaration.init
      |> Option.unwrap_or ~default:entities
  | Jir.Statement.Block statements ->
      collect_statement_assigned_entities entities statements
  | Jir.Statement.Expression expr
  | Jir.Statement.Return expr ->
      collect_expr_assigned_entities entities expr
  | Jir.Statement.If if_ ->
      let entities = collect_expr_assigned_entities entities if_.condition in
      let entities = collect_statement_assigned_entities entities if_.then_ in
      collect_statement_assigned_entities entities if_.else_

let collect_program_assigned_entities = fun (program: Jir.Program.t) ->
  collect_statement_assigned_entities Entity_set.empty program.body

let resolve_alias = fun env entity ->
  let rec loop seen entity =
    if Entity_set.mem entity seen then
      entity
    else
      match Core.Entity_id.binding_id entity with
      | None -> entity
      | Some binding_id -> (
          match Binding_map.find_opt binding_id env.aliases with
          | Some target -> loop (Entity_set.add entity seen) target
          | None -> entity
        )
  in
  loop Entity_set.empty entity

let bind_alias = fun env alias target -> { env with aliases = Binding_map.add alias target env.aliases }

let rec lower_expr = fun env expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ -> expr
  | Jir.Expr.Identifier entity -> Jir.Expr.Identifier (resolve_alias env entity)
  | Jir.Expr.Function function_ -> Jir.Expr.Function Jir.Expr.{
    function_
    with body = lower_scoped_block env function_.body;
  }
  | Jir.Expr.Member member -> Jir.Expr.Member Jir.Expr.{
    member
    with object_ = lower_expr env member.object_;
  }
  | Jir.Expr.Call call -> Jir.Expr.Call Jir.Expr.{
    callee = lower_expr env call.callee;
    arguments = List.map (lower_expr env) call.arguments;
  }
  | Jir.Expr.Conditional conditional -> Jir.Expr.Conditional Jir.Expr.{
    condition = lower_expr env conditional.condition;
    then_ = lower_expr env conditional.then_;
    else_ = lower_expr env conditional.else_;
  }
  | Jir.Expr.Assignment assignment -> Jir.Expr.Assignment Jir.Expr.{
    assignment
    with value = lower_expr env assignment.value;
  }

and lower_statement = fun env statement ->
  match statement with
  | Jir.Statement.Declaration declaration -> lower_declaration env declaration
  | Jir.Statement.Block statements -> (
    match lower_scoped_block env statements with
    | [] -> ([], env)
    | statements -> ([ Jir.Statement.Block statements ], env)
  )
  | Jir.Statement.Expression expr -> ([ Jir.Statement.Expression (lower_expr env expr) ], env)
  | Jir.Statement.Return expr -> ([ Jir.Statement.Return (lower_expr env expr) ], env)
  | Jir.Statement.If if_ ->
      let condition = lower_expr env if_.condition in
      let then_ = lower_scoped_block env if_.then_ in
      let else_ = lower_scoped_block env if_.else_ in
      if List.is_empty then_ && List.is_empty else_ then
        if Analysis.is_pure_expr condition then
          ([], env)
        else
          ([ Jir.Statement.Expression condition ], env)
      else
        ([
          Jir.Statement.If Jir.Statement.{ condition; then_; else_ }
        ], env)

and lower_declaration = fun env (declaration: Jir.Declaration.t) ->
  let init = Option.map (lower_expr env) declaration.init in
  let binder_entity = Jir.Binder.entity_id declaration.binder in
  match (declaration.kind, init) with
  | (Jir.Declaration.Const, Some (Jir.Expr.Identifier target)) when not
    (Core.Entity_id.equal binder_entity target)
  && not (Entity_set.mem binder_entity env.exported)
  && not (Entity_set.mem target env.assigned) -> ([], bind_alias env declaration.binder.binding_id target)
  | _ -> ([ Jir.Statement.Declaration Jir.Declaration.{ declaration with init } ], env)

and lower_block = fun env statements ->
  match statements with
  | [] -> ([], env)
  | statement :: rest ->
      let (statement, env) = lower_statement env statement in
      let (rest, env) = lower_block env rest in
      (statement @ rest, env)

and lower_scoped_block = fun env statements -> lower_block env statements |> fst

let program = fun (program: Jir.Program.t) ->
  let env = {
    aliases = Binding_map.empty;
    assigned = collect_program_assigned_entities program;
    exported =
      List.fold_left
        (fun set (export: Jir.Export.t) -> Entity_set.add export.local set)
        Entity_set.empty
        program.exports;
  } in
  let (body, _) = lower_block env program.body in
  { program with body }
