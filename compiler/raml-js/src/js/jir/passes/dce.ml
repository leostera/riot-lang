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

let forget_binding = fun entities binding_id ->
  Entity_set.filter
    (fun entity ->
      match Core.Entity_id.binding_id entity with
      | Some entity_binding_id -> not (Core.Binding_id.equal entity_binding_id binding_id)
      | None -> true)
    entities

let rec lower_expr = fun expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      (expr, Entity_set.empty)
  | Jir.Expr.Identifier entity ->
      (Jir.Expr.Identifier entity, Entity_set.singleton entity)
  | Jir.Expr.Function function_ ->
      let lowered_body = lower_block ~protected:Entity_set.empty function_.body in
      let used =
        List.fold_left
          (fun used (binder: Jir.Binder.t) -> forget_binding used binder.binding_id)
          lowered_body.used
          function_.params
      in
      (Jir.Expr.Function Jir.Expr.{ function_ with body = lowered_body.statements }, used)
  | Jir.Expr.Member member ->
      let (object_, used) = lower_expr member.object_ in
      (Jir.Expr.Member Jir.Expr.{ member with object_ }, used)
  | Jir.Expr.Call call ->
      let (callee, used) = lower_expr call.callee in
      let (arguments, argument_used) = lower_expr_list call.arguments in
      (Jir.Expr.Call Jir.Expr.{ callee; arguments }, Entity_set.union used argument_used)
  | Jir.Expr.Conditional conditional ->
      let (condition, condition_used) = lower_expr conditional.condition in
      let (then_, then_used) = lower_expr conditional.then_ in
      let (else_, else_used) = lower_expr conditional.else_ in
      (
        Jir.Expr.Conditional Jir.Expr.{ condition; then_; else_ },
        Entity_set.union (Entity_set.union condition_used then_used) else_used
      )
  | Jir.Expr.Assignment assignment ->
      let (value, used) = lower_expr assignment.value in
      (Jir.Expr.Assignment Jir.Expr.{ assignment with value }, Entity_set.add assignment.target used)

and lower_expr_list = fun exprs ->
  match exprs with
  | [] -> ([], Entity_set.empty)
  | expr :: rest ->
      let (expr, used) = lower_expr expr in
      let (rest, rest_used) = lower_expr_list rest in
      (expr :: rest, Entity_set.union used rest_used)

and lower_statement = fun ~protected used_after statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      lower_declaration ~protected used_after declaration
  | Jir.Statement.Block statements ->
      let lowered_block = lower_block ~protected:Entity_set.empty statements in
      let used = Entity_set.union used_after lowered_block.used in
      if List.is_empty lowered_block.statements then
        { statements = []; used }
      else
        { statements = [ Jir.Statement.Block lowered_block.statements ]; used }
  | Jir.Statement.Expression expr ->
      let (expr, used) = lower_expr expr in
      if Analysis.is_pure_expr expr then
        { statements = []; used = used_after }
      else
        { statements = [ Jir.Statement.Expression expr ]; used = Entity_set.union used_after used }
  | Jir.Statement.Return expr ->
      let (expr, used) = lower_expr expr in
      { statements = [ Jir.Statement.Return expr ]; used = Entity_set.union used_after used }
  | Jir.Statement.If if_ ->
      let (condition, condition_used) = lower_expr if_.condition in
      let then_ = lower_block ~protected:Entity_set.empty if_.then_ in
      let else_ = lower_block ~protected:Entity_set.empty if_.else_ in
      {
        statements = [
          Jir.Statement.If Jir.Statement.{
            condition;
            then_ = then_.statements;
            else_ = else_.statements;
          }
        ];
        used = Entity_set.union used_after condition_used |> Entity_set.union then_.used |> Entity_set.union else_.used;
      }

and lower_declaration = fun ~protected used_after (declaration: Jir.Declaration.t) ->
  let binder_entity = Jir.Binder.entity_id declaration.binder in
  let (init, init_used) =
    match declaration.init with
    | None -> (None, Entity_set.empty)
    | Some init ->
        let (init, used) = lower_expr init in
        (Some init, used)
  in
  match init with
  | Some init when declaration.kind = Jir.Declaration.Const
  && not (Entity_set.mem binder_entity protected)
  && not (Entity_set.mem binder_entity used_after)
  && Analysis.is_pure_expr init -> { statements = []; used = used_after }
  | _ -> {
    statements = [ Jir.Statement.Declaration Jir.Declaration.{ declaration with init } ];
    used = Entity_set.union (forget_binding used_after declaration.binder.binding_id) init_used;
  }

and lower_block = fun ~protected statements ->
  match statements with
  | [] -> { statements = []; used = Entity_set.empty }
  | statement :: rest ->
      let lowered_rest = lower_block ~protected rest in
      let lowered_statement = lower_statement ~protected lowered_rest.used statement in
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
  let lowered_body = lower_block ~protected program.body in
  { program with body = lowered_body.statements }
