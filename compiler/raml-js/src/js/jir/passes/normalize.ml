open Std
module Core = Raml_core.Core_ir
module Jir = Types

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

  let to_list = fun set -> Storage.bindings set |> List.map fst
end

let rec collect_expr_imports = fun expr imports ->
  match expr with
  | Jir.Expr.Literal _ ->
      imports
  | Jir.Expr.Identifier _ ->
      imports
  | Jir.Expr.Imported requirement ->
      Import_set.add requirement imports
  | Jir.Expr.Runtime_helper helper ->
      Import_set.add (Jir.Runtime.to_import helper) imports
  | Jir.Expr.Function function_ ->
      collect_statement_import_list function_.body imports
  | Jir.Expr.Member member ->
      collect_expr_imports member.object_ imports
  | Jir.Expr.Call { callee; arguments } ->
      let imports = collect_expr_imports callee imports in
      collect_expr_import_list arguments imports
  | Jir.Expr.Conditional conditional ->
      let imports = collect_expr_imports conditional.condition imports in
      let imports = collect_expr_imports conditional.then_ imports in
      collect_expr_imports conditional.else_ imports
  | Jir.Expr.Assignment assignment ->
      collect_expr_imports assignment.value imports

and collect_expr_import_list = fun exprs imports ->
  match exprs with
  | [] -> imports
  | expr :: rest ->
      let imports = collect_expr_imports expr imports in
      collect_expr_import_list rest imports

and collect_statement_import_list = fun statements imports ->
  match statements with
  | [] -> imports
  | statement :: rest ->
      let imports = collect_statement_imports statement imports in
      collect_statement_import_list rest imports

and collect_statement_imports = fun statement imports ->
  match statement with
  | Jir.Statement.Declaration declaration -> (
      match declaration.init with
      | None -> imports
      | Some init -> collect_expr_imports init imports
    )
  | Jir.Statement.Block statements ->
      collect_statement_import_list statements imports
  | Jir.Statement.Expression expr ->
      collect_expr_imports expr imports
  | Jir.Statement.Return expr ->
      collect_expr_imports expr imports
  | Jir.Statement.If if_ ->
      let imports = collect_expr_imports if_.condition imports in
      let imports = collect_statement_import_list if_.then_ imports in
      collect_statement_import_list if_.else_ imports

let collect_program_imports = fun program ->
  List.fold_right collect_statement_imports Jir.Program.(program.body) Import_set.empty

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
      match normalize_statement_list statements with
      | [] -> []
      | statements -> [ Jir.Statement.Block statements ]
    )
  | Jir.Statement.Expression expr ->
      [ Jir.Statement.Expression (normalize_expr expr) ]
  | Jir.Statement.Return expr ->
      [ Jir.Statement.Return (normalize_expr expr) ]
  | Jir.Statement.If if_ ->
      [ Jir.Statement.If Jir.Statement.{
          condition = normalize_expr if_.condition;
          then_ = normalize_statement_list if_.then_;
          else_ = normalize_statement_list if_.else_;
        } ]

and normalize_statement_list = fun statements ->
  List.concat_map normalize_statement statements

let program = fun (program: Jir.Program.t) ->
  let body = normalize_statement_list program.body in
  let imports = collect_program_imports Jir.Program.{ program with body } |> Import_set.to_list in
  { program with body; imports }
