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

  let add = fun entity set -> Storage.add entity () set

  let mem = Storage.mem
end

let rec collect_expr_used_entities = fun expr used ->
  match expr with
  | Jir.Expr.Literal _ ->
      used
  | Jir.Expr.Identifier entity ->
      Entity_set.add entity used
  | Jir.Expr.Imported requirement ->
      Entity_set.add (Jir.Binder.entity_id requirement.local) used
  | Jir.Expr.Runtime_helper helper ->
      Entity_set.add (Jir.Binder.entity_id helper.local) used
  | Jir.Expr.Function function_ ->
      collect_statement_used_entities function_.body used
  | Jir.Expr.Member member ->
      collect_expr_used_entities member.object_ used
  | Jir.Expr.Call call ->
      let used = collect_expr_used_entities call.callee used in
      collect_expr_list_used_entities call.arguments used
  | Jir.Expr.Conditional conditional ->
      let used = collect_expr_used_entities conditional.condition used in
      let used = collect_expr_used_entities conditional.then_ used in
      collect_expr_used_entities conditional.else_ used
  | Jir.Expr.Assignment assignment ->
      collect_expr_used_entities assignment.value (Entity_set.add assignment.target used)

and collect_expr_list_used_entities = fun exprs used ->
  match exprs with
  | [] -> used
  | expr :: rest ->
      let used = collect_expr_used_entities expr used in
      collect_expr_list_used_entities rest used

and collect_statement_used_entities = fun statements used ->
  match statements with
  | [] -> used
  | statement :: rest ->
      let used = collect_one_statement_used_entities statement used in
      collect_statement_used_entities rest used

and collect_one_statement_used_entities = fun statement used ->
  match statement with
  | Jir.Statement.Declaration declaration -> (
      match declaration.init with
      | None -> used
      | Some init -> collect_expr_used_entities init used
    )
  | Jir.Statement.Block statements ->
      collect_statement_used_entities statements used
  | Jir.Statement.Expression expr
  | Jir.Statement.Return expr ->
      collect_expr_used_entities expr used
  | Jir.Statement.If if_ ->
      let used = collect_expr_used_entities if_.condition used in
      let used = collect_statement_used_entities if_.then_ used in
      collect_statement_used_entities if_.else_ used

let collect_program_used_entities = fun (program: Jir.Program.t) ->
  let used = collect_statement_used_entities program.body Entity_set.empty in
  List.fold_left
    (fun used (export: Jir.Export.t) -> Entity_set.add export.local used)
    used
    program.exports

let program = fun (program: Jir.Program.t) ->
  let used = collect_program_used_entities program in
  let imports =
    List.filter
      (fun (import: Jir.Imports.requirement) ->
        Entity_set.mem (Jir.Binder.entity_id import.local) used)
      program.imports
  in
  { program with imports }
