open Std
module Core = Raml_core.Core_ir
module Jir = Types
module Analysis = Analysis
module Simplify = Simplify

module Import_set = struct
  module Storage = Collections.Map.Make (struct
    type t = Jir.Imports.requirement

    let compare_optional_string = fun left right ->
      match (left, right) with
      | (None, None) -> 0
      | (None, Some _) -> (-1)
      | (Some _, None) -> 1
      | (Some left, Some right) -> String.compare left right

    let compare = fun (left: Jir.Imports.requirement) (right: Jir.Imports.requirement) ->
      let by_from = String.compare left.from right.from in
      if by_from != 0 then
        by_from
      else
        let by_namespace = Bool.compare left.namespace right.namespace in
        if by_namespace != 0 then
          by_namespace
        else
          let by_imported = compare_optional_string left.imported right.imported in
          if by_imported != 0 then
            by_imported
          else
            let by_binding = Core.Binding_id.compare left.local.binding_id right.local.binding_id in
            if by_binding != 0 then
              by_binding
            else
              String.compare left.local.name right.local.name
  end)

  type t = unit Storage.t

  let empty = Storage.empty

  let add = fun import set -> Storage.add import () set

  let mem = Storage.mem
end

type import_state = {
  seen: Import_set.t;
  reversed: Jir.Imports.requirement list;
}

let empty_import_state = {
  seen = Import_set.empty;
  reversed = [];
}

let remember_import = fun requirement state ->
  if Import_set.mem requirement state.seen then
    state
  else
    {
      seen = Import_set.add requirement state.seen;
      reversed = requirement :: state.reversed;
    }

let rec collect_expr_imports = fun expr state ->
  match expr with
  | Jir.Expr.Literal _ ->
      state
  | Jir.Expr.Identifier _ ->
      state
  | Jir.Expr.Imported requirement ->
      remember_import requirement state
  | Jir.Expr.Runtime_helper helper ->
      remember_import (Jir.Runtime.to_import helper) state
  | Jir.Expr.Function function_ ->
      collect_statement_import_list function_.body state
  | Jir.Expr.Member member ->
      collect_expr_imports member.object_ state
  | Jir.Expr.Call { callee; arguments } ->
      let state = collect_expr_imports callee state in
      collect_expr_import_list arguments state
  | Jir.Expr.Conditional conditional ->
      let state = collect_expr_imports conditional.condition state in
      let state = collect_expr_imports conditional.then_ state in
      collect_expr_imports conditional.else_ state
  | Jir.Expr.Assignment assignment ->
      collect_expr_imports assignment.value state

and collect_expr_import_list = fun exprs state ->
  match exprs with
  | [] -> state
  | expr :: rest ->
      let state = collect_expr_imports expr state in
      collect_expr_import_list rest state

and collect_statement_import_list = fun statements state ->
  match statements with
  | [] -> state
  | statement :: rest ->
      let state = collect_statement_imports statement state in
      collect_statement_import_list rest state

and collect_statement_imports = fun statement state ->
  match statement with
  | Jir.Statement.Declaration declaration -> (
      match declaration.init with
      | None -> state
      | Some init -> collect_expr_imports init state
    )
  | Jir.Statement.Block statements ->
      collect_statement_import_list statements state
  | Jir.Statement.Expression expr ->
      collect_expr_imports expr state
  | Jir.Statement.Return expr ->
      collect_expr_imports expr state
  | Jir.Statement.If if_ ->
      let state = collect_expr_imports if_.condition state in
      let state = collect_statement_import_list if_.then_ state in
      collect_statement_import_list if_.else_ state

let collect_program_imports = fun program ->
  List.fold_left
    (fun state statement -> collect_statement_imports statement state)
    empty_import_state
    Jir.Program.(program.body)

let rec normalize_expr = fun expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Identifier _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      expr
  | Jir.Expr.Function function_ -> Jir.Expr.Function Jir.Expr.{
    function_
    with body = normalize_statement_list function_.body;
  }
  | Jir.Expr.Member member -> Jir.Expr.Member Jir.Expr.{
    member
    with object_ = normalize_expr member.object_;
  }
  | Jir.Expr.Call call -> Jir.Expr.Call Jir.Expr.{
    callee = normalize_expr call.callee;
    arguments = List.map normalize_expr call.arguments;
  }
  | Jir.Expr.Conditional conditional -> Jir.Expr.Conditional Jir.Expr.{
    condition = normalize_expr conditional.condition;
    then_ = normalize_expr conditional.then_;
    else_ = normalize_expr conditional.else_;
  }
  | Jir.Expr.Assignment assignment -> Jir.Expr.Assignment Jir.Expr.{
    assignment
    with value = normalize_expr assignment.value;
  }

and normalize_statement = fun statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      [ Jir.Statement.Declaration Jir.Declaration.{
          declaration
          with init = Option.map normalize_expr declaration.init;
        } ]
  | Jir.Statement.Block statements -> (
      normalize_statement_list statements |> Simplify.block
    )
  | Jir.Statement.Expression expr ->
      [ Jir.Statement.Expression (normalize_expr expr) ]
  | Jir.Statement.Return expr ->
      [ Jir.Statement.Return (normalize_expr expr) ]
  | Jir.Statement.If if_ ->
      let condition = normalize_expr if_.condition in
      let then_ = normalize_statement_list if_.then_ in
      let else_ = normalize_statement_list if_.else_ in
      Simplify.conditional ~condition ~then_ ~else_

and normalize_statement_list = fun statements ->
  List.concat_map normalize_statement statements

let program = fun (program: Jir.Program.t) ->
  let body = normalize_statement_list program.body in
  let imports =
    collect_program_imports Jir.Program.{ program with body }
    |> fun state -> List.rev state.reversed
  in
  { program with body; imports }
