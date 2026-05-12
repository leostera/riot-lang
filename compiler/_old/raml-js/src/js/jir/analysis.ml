open Std
module Core = Raml_core.Core_ir
module Jir = Types

module Entity_set = struct
  module Storage = Collections.Map.Make (struct
    type t = Core.Entity_id.t

    let compare = Core.Entity_id.compare
  end)

  type t = unit Storage.t

  let empty = Storage.empty

  let add = fun entity set -> Storage.insert set ~key:entity ~value:()

  let singleton = fun entity -> Storage.singleton ~key:entity ~value:()

  let mem = fun entity set -> Storage.has_key set ~key:entity

  let union = fun left right ->
    Storage.union ~left ~right ~fn:(fun ~key:_ ~left:(()) ~right:(()) -> Some ())

  let filter = fun predicate set -> Storage.filter set ~fn:(fun entity () -> predicate entity)
end

let rec is_pure_expr = fun expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Global _
  | Jir.Expr.Identifier _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _
  | Jir.Expr.Function _ ->
      true
  | Jir.Expr.Unary unary ->
      is_pure_expr unary.operand
  | Jir.Expr.Binary binary ->
      if is_pure_expr binary.left then
        is_pure_expr binary.right
      else
        false
  | Jir.Expr.Array elements ->
      List.for_all is_pure_array_element elements
  | Jir.Expr.Object fields ->
      List.for_all is_pure_object_field fields
  | Jir.Expr.Member member ->
      is_pure_expr member.object_
  | Jir.Expr.Index index ->
      if is_pure_expr index.object_ then
        is_pure_expr index.index
      else
        false
  | Jir.Expr.Call _
  | Jir.Expr.Assignment _ ->
      false
  | Jir.Expr.Conditional conditional ->
      if is_pure_expr conditional.condition then
        if is_pure_expr conditional.then_ then
          is_pure_expr conditional.else_
        else
          false
      else
        false

and is_pure_array_element = fun element ->
  match element with
  | Jir.Expr.Item expr
  | Jir.Expr.Spread expr -> is_pure_expr expr

and is_pure_object_field = fun (field: Jir.Expr.object_field) -> is_pure_expr field.value

let rec expr_read_entities = fun expr used ->
  match expr with
  | Jir.Expr.Literal _ ->
      used
  | Jir.Expr.Global _ ->
      used
  | Jir.Expr.Identifier entity ->
      Entity_set.add entity used
  | Jir.Expr.Imported requirement ->
      Entity_set.add (Jir.Binder.entity_id requirement.local) used
  | Jir.Expr.Runtime_helper helper ->
      Entity_set.add (Jir.Binder.entity_id helper.local) used
  | Jir.Expr.Unary unary ->
      expr_read_entities unary.operand used
  | Jir.Expr.Binary binary ->
      let used = expr_read_entities binary.left used in
      expr_read_entities binary.right used
  | Jir.Expr.Array elements ->
      array_elements_read_entities elements used
  | Jir.Expr.Object fields ->
      object_fields_read_entities fields used
  | Jir.Expr.Function function_ ->
      statements_read_entities function_.body used
  | Jir.Expr.Member member ->
      expr_read_entities member.object_ used
  | Jir.Expr.Index index ->
      let used = expr_read_entities index.object_ used in
      expr_read_entities index.index used
  | Jir.Expr.Call call ->
      let used = expr_read_entities call.callee used in
      expr_list_read_entities call.arguments used
  | Jir.Expr.Conditional conditional ->
      let used = expr_read_entities conditional.condition used in
      let used = expr_read_entities conditional.then_ used in
      expr_read_entities conditional.else_ used
  | Jir.Expr.Assignment assignment ->
      expr_read_entities assignment.value used

and expr_list_read_entities = fun exprs used ->
  match exprs with
  | [] -> used
  | expr :: rest ->
      let used = expr_read_entities expr used in
      expr_list_read_entities rest used

and array_elements_read_entities = fun elements used ->
  match elements with
  | [] -> used
  | element :: rest ->
      let used =
        match element with
        | Jir.Expr.Item expr
        | Jir.Expr.Spread expr -> expr_read_entities expr used
      in
      array_elements_read_entities rest used

and object_fields_read_entities = fun fields used ->
  match fields with
  | [] -> used
  | field :: rest ->
      let used = expr_read_entities field.value used in
      object_fields_read_entities rest used

and statement_read_entities = fun statement used ->
  match statement with
  | Jir.Statement.Declaration declaration -> (
      match declaration.init with
      | None -> used
      | Some init -> expr_read_entities init used
    )
  | Jir.Statement.Block statements ->
      statements_read_entities statements used
  | Jir.Statement.Expression expr
  | Jir.Statement.Return expr ->
      expr_read_entities expr used
  | Jir.Statement.If if_ ->
      let used = expr_read_entities if_.condition used in
      let used = statements_read_entities if_.then_ used in
      statements_read_entities if_.else_ used

and statements_read_entities = fun statements used ->
  match statements with
  | [] -> used
  | statement :: rest ->
      let used = statement_read_entities statement used in
      statements_read_entities rest used

let program_read_entities = fun (program: Jir.Program.t) ->
  let used = statements_read_entities program.body Entity_set.empty in
  List.fold_left program.exports ~init:used
    ~fn:(fun used (export: Jir.Export.t) ->
      Entity_set.add export.local used)

let rec expr_assigned_entities = fun expr entities ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Global _
  | Jir.Expr.Identifier _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      entities
  | Jir.Expr.Unary unary ->
      expr_assigned_entities unary.operand entities
  | Jir.Expr.Binary binary ->
      let entities = expr_assigned_entities binary.left entities in
      expr_assigned_entities binary.right entities
  | Jir.Expr.Array elements ->
      array_elements_assigned_entities elements entities
  | Jir.Expr.Object fields ->
      object_fields_assigned_entities fields entities
  | Jir.Expr.Function function_ ->
      statements_assigned_entities function_.body entities
  | Jir.Expr.Member member ->
      expr_assigned_entities member.object_ entities
  | Jir.Expr.Index index ->
      let entities = expr_assigned_entities index.object_ entities in
      expr_assigned_entities index.index entities
  | Jir.Expr.Call call ->
      let entities = expr_assigned_entities call.callee entities in
      expr_list_assigned_entities call.arguments entities
  | Jir.Expr.Conditional conditional ->
      let entities = expr_assigned_entities conditional.condition entities in
      let entities = expr_assigned_entities conditional.then_ entities in
      expr_assigned_entities conditional.else_ entities
  | Jir.Expr.Assignment assignment ->
      expr_assigned_entities assignment.value (Entity_set.add assignment.target entities)

and expr_list_assigned_entities = fun exprs entities ->
  match exprs with
  | [] -> entities
  | expr :: rest ->
      let entities = expr_assigned_entities expr entities in
      expr_list_assigned_entities rest entities

and array_elements_assigned_entities = fun elements entities ->
  match elements with
  | [] -> entities
  | element :: rest ->
      let entities =
        match element with
        | Jir.Expr.Item expr
        | Jir.Expr.Spread expr -> expr_assigned_entities expr entities
      in
      array_elements_assigned_entities rest entities

and object_fields_assigned_entities = fun fields entities ->
  match fields with
  | [] -> entities
  | field :: rest ->
      let entities = expr_assigned_entities field.value entities in
      object_fields_assigned_entities rest entities

and statement_assigned_entities = fun statement entities ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      Option.map declaration.init ~fn:(fun init -> expr_assigned_entities init entities)
      |> Option.unwrap_or ~default:entities
  | Jir.Statement.Block statements ->
      statements_assigned_entities statements entities
  | Jir.Statement.Expression expr
  | Jir.Statement.Return expr ->
      expr_assigned_entities expr entities
  | Jir.Statement.If if_ ->
      let entities = expr_assigned_entities if_.condition entities in
      let entities = statements_assigned_entities if_.then_ entities in
      statements_assigned_entities if_.else_ entities

and statements_assigned_entities = fun statements entities ->
  match statements with
  | [] -> entities
  | statement :: rest ->
      let entities = statement_assigned_entities statement entities in
      statements_assigned_entities rest entities

let program_assigned_entities = fun (program: Jir.Program.t) ->
  statements_assigned_entities program.body Entity_set.empty
