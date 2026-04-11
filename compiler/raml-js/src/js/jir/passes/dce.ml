open Std
module Core = Raml_core.Core_ir
module Jir = Types
module Analysis = Analysis

module Entity_set = struct
  module Storage = Collections.Map.Make (struct
    type t = Core.Entity_id.t
    let compare = Core.Entity_id.compare
  end)

  type t = unit Storage.t

  let empty = Storage.empty

  let add = fun entity set -> Storage.add entity () set

  let singleton = fun entity -> Storage.singleton entity ()

  let mem = Storage.mem

  let union = fun left right ->
    Storage.union (fun _ () () -> Some ()) left right

  let filter = fun predicate set ->
    Storage.filter (fun entity () -> predicate entity) set
end

type lowered_block = {
  statements: Jir.Statement.t list;
  used: Entity_set.t;
}

let is_dead_local_store = fun ~protected ~used_after target ->
  match Core.Entity_id.binding_id target with
  | None -> false
  | Some _ -> not (Entity_set.mem target protected) && not (Entity_set.mem target used_after)

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

let forget_binding = fun entities binding_id ->
  Entity_set.filter
    (fun entity ->
      match Core.Entity_id.binding_id entity with
      | Some entity_binding_id -> not (Core.Binding_id.equal entity_binding_id binding_id)
      | None -> true)
    entities

let rec lower_expr = fun ~assigned expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      (expr, Entity_set.empty)
  | Jir.Expr.Identifier entity ->
      (Jir.Expr.Identifier entity, Entity_set.singleton entity)
  | Jir.Expr.Function function_ ->
      let lowered_body = lower_block ~protected:Entity_set.empty ~assigned function_.body in
      let used =
        List.fold_left
          (fun used (binder: Jir.Binder.t) -> forget_binding used binder.binding_id)
          lowered_body.used
          function_.params
      in
      (Jir.Expr.Function Jir.Expr.{ function_ with body = lowered_body.statements }, used)
  | Jir.Expr.Member member ->
      let (object_, used) = lower_expr ~assigned member.object_ in
      (Jir.Expr.Member Jir.Expr.{ member with object_ }, used)
  | Jir.Expr.Call call ->
      let (callee, used) = lower_expr ~assigned call.callee in
      let (arguments, argument_used) = lower_expr_list ~assigned call.arguments in
      (Jir.Expr.Call Jir.Expr.{ callee; arguments }, Entity_set.union used argument_used)
  | Jir.Expr.Conditional conditional ->
      let (condition, condition_used) = lower_expr ~assigned conditional.condition in
      let (then_, then_used) = lower_expr ~assigned conditional.then_ in
      let (else_, else_used) = lower_expr ~assigned conditional.else_ in
      (
        Jir.Expr.Conditional Jir.Expr.{ condition; then_; else_ },
        Entity_set.union (Entity_set.union condition_used then_used) else_used
      )
  | Jir.Expr.Assignment assignment ->
      let (value, used) = lower_expr ~assigned assignment.value in
      (Jir.Expr.Assignment Jir.Expr.{ assignment with value }, used)

and lower_expr_list = fun ~assigned exprs ->
  match exprs with
  | [] -> ([], Entity_set.empty)
  | expr :: rest ->
      let (expr, used) = lower_expr ~assigned expr in
      let (rest, rest_used) = lower_expr_list ~assigned rest in
      (expr :: rest, Entity_set.union used rest_used)

and lower_statement = fun ~protected ~assigned used_after statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      lower_declaration ~protected ~assigned used_after declaration
  | Jir.Statement.Block statements ->
      let lowered_block = lower_block ~protected:Entity_set.empty ~assigned statements in
      let used = Entity_set.union used_after lowered_block.used in
      if List.is_empty lowered_block.statements then
        { statements = []; used }
      else
        { statements = [ Jir.Statement.Block lowered_block.statements ]; used }
  | Jir.Statement.Expression expr ->
      let (expr, used) = lower_expr ~assigned expr in
      lower_effect_expr_statement ~protected used_after expr used
  | Jir.Statement.Return expr ->
      let (expr, used) = lower_expr ~assigned expr in
      { statements = [ Jir.Statement.Return expr ]; used = Entity_set.union used_after used }
  | Jir.Statement.If if_ ->
      let (condition, condition_used) = lower_expr ~assigned if_.condition in
      let then_ = lower_block ~protected:Entity_set.empty ~assigned if_.then_ in
      let else_ = lower_block ~protected:Entity_set.empty ~assigned if_.else_ in
      if List.is_empty then_.statements && List.is_empty else_.statements then
        lower_effect_expr_statement ~protected used_after condition condition_used
      else
        {
          statements = [
            Jir.Statement.If Jir.Statement.{
              condition;
              then_ = then_.statements;
              else_ = else_.statements;
            }
          ];
          used =
            Entity_set.union used_after condition_used
            |> Entity_set.union then_.used
            |> Entity_set.union else_.used;
        }

and lower_declaration = fun ~protected ~assigned used_after (declaration: Jir.Declaration.t) ->
  let binder_entity = Jir.Binder.entity_id declaration.binder in
  let (init, init_used) =
    match declaration.init with
    | None -> (None, Entity_set.empty)
    | Some init ->
        let (init, used) = lower_expr ~assigned init in
        (Some init, used)
  in
  match (declaration.kind, init) with
  | (Jir.Declaration.Const, Some init)
    when not (Entity_set.mem binder_entity protected)
      && not (Entity_set.mem binder_entity used_after)
      && Analysis.is_pure_expr init ->
      { statements = []; used = used_after }
  | (Jir.Declaration.Let, None)
    when not (Entity_set.mem binder_entity protected)
      && not (Entity_set.mem binder_entity used_after)
      && not (Entity_set.mem binder_entity assigned) ->
      { statements = []; used = used_after }
  | (Jir.Declaration.Let, Some init)
    when not (Entity_set.mem binder_entity protected)
      && not (Entity_set.mem binder_entity used_after)
      && not (Entity_set.mem binder_entity assigned)
      && Analysis.is_pure_expr init ->
      { statements = []; used = used_after }
  | _ -> {
    statements = [ Jir.Statement.Declaration Jir.Declaration.{ declaration with init } ];
    used = Entity_set.union (forget_binding used_after declaration.binder.binding_id) init_used;
  }

and lower_effect_expr_statement = fun ~protected used_after expr used ->
  match expr with
  | Jir.Expr.Assignment assignment when is_dead_local_store ~protected ~used_after assignment.target ->
      if Analysis.is_pure_expr assignment.value then
        { statements = []; used = Entity_set.union used_after used }
      else
        {
          statements = [ Jir.Statement.Expression assignment.value ];
          used = Entity_set.union used_after used;
        }
  | _ ->
      if Analysis.is_pure_expr expr then
        { statements = []; used = used_after }
      else
        { statements = [ Jir.Statement.Expression expr ]; used = Entity_set.union used_after used }

and lower_block = fun ~protected ~assigned statements ->
  match statements with
  | [] -> { statements = []; used = Entity_set.empty }
  | statement :: rest ->
      let lowered_rest = lower_block ~protected ~assigned rest in
      let lowered_statement = lower_statement ~protected ~assigned lowered_rest.used statement in
      {
        statements = lowered_statement.statements @ lowered_rest.statements;
        used = lowered_statement.used;
      }

let program = fun (program: Jir.Program.t) ->
  let protected =
    List.fold_left
      (fun set (export: Jir.Export.t) -> Entity_set.add export.local set)
      Entity_set.empty
      program.exports
  in
  let assigned = collect_program_assigned_entities program in
  let lowered_body = lower_block ~protected ~assigned program.body in
  { program with body = lowered_body.statements }
