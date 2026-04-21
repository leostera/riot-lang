open Std
module Core = Raml_core.Core_ir
module Jir = Types
module Analysis = Analysis
module Entity_set = Analysis.Entity_set

type lowered_block = {
  statements: Jir.Statement.t list;
  used: Entity_set.t;
}

let is_dead_local_store = fun ~protected ~used_after target ->
  match Core.Entity_id.binding_id target with
  | None -> false
  | Some _ ->
      if not (Entity_set.mem target protected) then
        not (Entity_set.mem target used_after)
      else
        false

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
  | Jir.Expr.Global _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      (expr, Entity_set.empty)
  | Jir.Expr.Identifier entity ->
      (Jir.Expr.Identifier entity, Entity_set.singleton entity)
  | Jir.Expr.Unary unary ->
      let (operand, used) = lower_expr ~assigned unary.operand in
      (Jir.Expr.Unary Jir.Expr.{ unary with operand }, used)
  | Jir.Expr.Binary binary ->
      let (left, left_used) = lower_expr ~assigned binary.left in
      let (right, right_used) = lower_expr ~assigned binary.right in
      (Jir.Expr.Binary Jir.Expr.{ binary with left; right }, Entity_set.union left_used right_used)
  | Jir.Expr.Array elements ->
      let (elements, used) = lower_array_elements ~assigned elements in
      (Jir.Expr.Array elements, used)
  | Jir.Expr.Object fields ->
      let (fields, used) = lower_object_fields ~assigned fields in
      (Jir.Expr.Object fields, used)
  | Jir.Expr.Function function_ ->
      let lowered_body = lower_block ~protected:Entity_set.empty ~assigned function_.body in
      let used =
        List.fold_left
          function_.params
          ~init:lowered_body.used
          ~fn:(fun used (binder: Jir.Binder.t) -> forget_binding used binder.binding_id)
      in
      (Jir.Expr.Function Jir.Expr.{ function_ with body = lowered_body.statements }, used)
  | Jir.Expr.Member member ->
      let (object_, used) = lower_expr ~assigned member.object_ in
      (Jir.Expr.Member Jir.Expr.{ member with object_ }, used)
  | Jir.Expr.Index index ->
      let (object_, object_used) = lower_expr ~assigned index.object_ in
      let (index, index_used) = lower_expr ~assigned index.index in
      (Jir.Expr.Index Jir.Expr.{ object_; index }, Entity_set.union object_used index_used)
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

and lower_array_element = fun ~assigned element ->
  match element with
  | Jir.Expr.Item expr ->
      let (expr, used) = lower_expr ~assigned expr in
      (Jir.Expr.Item expr, used)
  | Jir.Expr.Spread expr ->
      let (expr, used) = lower_expr ~assigned expr in
      (Jir.Expr.Spread expr, used)

and lower_object_field = fun ~assigned (field: Jir.Expr.object_field) ->
  let (value, used) = lower_expr ~assigned field.value in
  (Jir.Expr.{ field with value }, used)

and lower_array_elements = fun ~assigned elements ->
  match elements with
  | [] -> ([], Entity_set.empty)
  | element :: rest ->
      let (element, used) = lower_array_element ~assigned element in
      let (rest, rest_used) = lower_array_elements ~assigned rest in
      (element :: rest, Entity_set.union used rest_used)

and lower_object_fields = fun ~assigned fields ->
  match fields with
  | [] -> ([], Entity_set.empty)
  | field :: rest ->
      let (field, used) = lower_object_field ~assigned field in
      let (rest, rest_used) = lower_object_fields ~assigned rest in
      (field :: rest, Entity_set.union used rest_used)

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
      if List.is_empty then_.statements then
        if List.is_empty else_.statements then
          lower_effect_expr_statement ~protected used_after condition condition_used
        else
          {
            statements = [
              Jir.Statement.If Jir.Statement.{
                condition;
                then_ = then_.statements;
                else_ = else_.statements
              }
            ];
            used = Entity_set.union used_after condition_used
            |> Entity_set.union then_.used
            |> Entity_set.union else_.used
          }
      else
        {
          statements = [
            Jir.Statement.If Jir.Statement.{
              condition;
              then_ = then_.statements;
              else_ = else_.statements
            }
          ];
          used = Entity_set.union used_after condition_used
          |> Entity_set.union then_.used
          |> Entity_set.union else_.used
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
  | (Jir.Declaration.Const, Some init) when not (Entity_set.mem binder_entity protected)
  && not (Entity_set.mem binder_entity used_after)
  && Analysis.is_pure_expr init -> { statements = []; used = used_after }
  | (Jir.Declaration.Let, None) when not (Entity_set.mem binder_entity protected)
  && not (Entity_set.mem binder_entity used_after)
  && not (Entity_set.mem binder_entity assigned) -> { statements = []; used = used_after }
  | (Jir.Declaration.Let, Some init) when not (Entity_set.mem binder_entity protected)
  && not (Entity_set.mem binder_entity used_after)
  && not (Entity_set.mem binder_entity assigned)
  && Analysis.is_pure_expr init -> { statements = []; used = used_after }
  | _ -> {
    statements = [ Jir.Statement.Declaration Jir.Declaration.{ declaration with init } ];
    used = Entity_set.union (forget_binding used_after declaration.binder.binding_id) init_used
  }

and lower_effect_expr_statement = fun ~protected used_after expr used ->
  match expr with
  | Jir.Expr.Assignment assignment when is_dead_local_store ~protected ~used_after assignment.target ->
      if Analysis.is_pure_expr assignment.value then
        { statements = []; used = Entity_set.union used_after used }
      else
        {
          statements = [ Jir.Statement.Expression assignment.value ];
          used = Entity_set.union used_after used
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
        used = lowered_statement.used
      }

let program = fun ~context:_ (program: Jir.Program.t) ->
  let protected =
    List.fold_left program.exports ~init:Entity_set.empty
      ~fn:(fun set (export: Jir.Export.t) ->
        Entity_set.add export.local set)
  in
  let assigned = Analysis.program_assigned_entities program in
  let lowered_body = lower_block ~protected ~assigned program.body in
  { program with body = lowered_body.statements }
