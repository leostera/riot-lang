open Std
module Core = Raml.CoreIR
module Jir = Types

type lowered_block = {
  statements: Jir.Statement.t list;
  used: Core.Entity_id.t list;
}

let remember_entity = fun entities entity ->
  if List.exists (Core.Entity_id.equal entity) entities then
    entities
  else
    entity :: entities

let forget_binding = fun entities binding_id ->
  List.filter
    (fun entity ->
      match Core.Entity_id.binding_id entity with
      | Some entity_binding_id -> not (Core.Binding_id.equal entity_binding_id binding_id)
      | None -> true)
    entities

let merge_entities = fun left right ->
  List.fold_left remember_entity left right

let is_entity = fun entities entity ->
  List.exists (Core.Entity_id.equal entity) entities

let rec is_pure_expr = fun expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Identifier _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _
  | Jir.Expr.Function _ -> true
  | Jir.Expr.Member member -> is_pure_expr member.object_
  | Jir.Expr.Conditional conditional -> is_pure_expr conditional.condition
  && is_pure_expr conditional.then_
  && is_pure_expr conditional.else_
  | Jir.Expr.Call _
  | Jir.Expr.Assignment _ -> false

let rec lower_expr = fun expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      (expr, [])
  | Jir.Expr.Identifier entity ->
      (Jir.Expr.Identifier entity, [ entity ])
  | Jir.Expr.Function function_ ->
      let lowered_body = lower_block ~protected:[] function_.body in
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
      (Jir.Expr.Call Jir.Expr.{ callee; arguments }, merge_entities used argument_used)
  | Jir.Expr.Conditional conditional ->
      let (condition, condition_used) = lower_expr conditional.condition in
      let (then_, then_used) = lower_expr conditional.then_ in
      let (else_, else_used) = lower_expr conditional.else_ in
      (
        Jir.Expr.Conditional Jir.Expr.{ condition; then_; else_ },
        merge_entities (merge_entities condition_used then_used) else_used
      )
  | Jir.Expr.Assignment assignment ->
      let (value, used) = lower_expr assignment.value in
      (Jir.Expr.Assignment Jir.Expr.{ assignment with value }, remember_entity used assignment.target)

and lower_expr_list = fun exprs ->
  match exprs with
  | [] -> ([], [])
  | expr :: rest ->
      let (expr, used) = lower_expr expr in
      let (rest, rest_used) = lower_expr_list rest in
      (expr :: rest, merge_entities used rest_used)

and lower_statement = fun ~protected used_after statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      lower_declaration ~protected used_after declaration
  | Jir.Statement.Block statements ->
      let lowered_block = lower_block ~protected:[] statements in
      let used = merge_entities used_after lowered_block.used in
      if List.is_empty lowered_block.statements then
        { statements = []; used }
      else
        { statements = [ Jir.Statement.Block lowered_block.statements ]; used }
  | Jir.Statement.Expression expr ->
      let (expr, used) = lower_expr expr in
      { statements = [ Jir.Statement.Expression expr ]; used = merge_entities used_after used }
  | Jir.Statement.Return expr ->
      let (expr, used) = lower_expr expr in
      { statements = [ Jir.Statement.Return expr ]; used = merge_entities used_after used }
  | Jir.Statement.If if_ ->
      let (condition, condition_used) = lower_expr if_.condition in
      let then_ = lower_block ~protected:[] if_.then_ in
      let else_ = lower_block ~protected:[] if_.else_ in
      {
        statements = [
          Jir.Statement.If Jir.Statement.{
            condition;
            then_ = then_.statements;
            else_ = else_.statements;
          }
        ];
        used = used_after
        |> merge_entities condition_used
        |> merge_entities then_.used
        |> merge_entities else_.used;
      }

and lower_declaration = fun ~protected used_after (declaration: Jir.Declaration.t) ->
  let binder_entity = Jir.Binder.entity_id declaration.binder in
  let (init, init_used) =
    match declaration.init with
    | None -> (None, [])
    | Some init ->
        let (init, used) = lower_expr init in
        (Some init, used)
  in
  match init with
  | Some init when declaration.kind = Jir.Declaration.Const
  && not (is_entity protected binder_entity)
  && not (is_entity used_after binder_entity)
  && is_pure_expr init -> { statements = []; used = used_after }
  | _ -> {
    statements = [ Jir.Statement.Declaration Jir.Declaration.{ declaration with init } ];
    used = merge_entities (forget_binding used_after declaration.binder.binding_id) init_used;
  }

and lower_block = fun ~protected statements ->
  match statements with
  | [] -> { statements = []; used = [] }
  | statement :: rest ->
      let lowered_rest = lower_block ~protected rest in
      let lowered_statement = lower_statement ~protected lowered_rest.used statement in
      {
        statements = lowered_statement.statements @ lowered_rest.statements;
        used = lowered_statement.used;
      }

let program = fun (program: Jir.Program.t) ->
  let protected = List.map (fun (export: Jir.Export.t) -> export.local) program.exports in
  let lowered_body = lower_block ~protected program.body in
  { program with body = lowered_body.statements }
