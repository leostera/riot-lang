open Std
module Core = Raml.CoreIR
module Jir = Types

let rec collect_expr_imports = fun expr imports ->
  match expr with
  | Jir.Expr.Literal _ ->
      imports
  | Jir.Expr.Identifier _ ->
      imports
  | Jir.Expr.Imported requirement ->
      requirement :: imports
  | Jir.Expr.Runtime_helper helper ->
      Jir.Runtime.to_import helper :: imports
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
  List.fold_right collect_statement_imports Jir.Program.(program.body) []

let dedupe_imports = fun imports ->
  let rec loop seen rest =
    match rest with
    | [] -> List.rev seen
    | import :: rest ->
        let seen_already = List.exists (Jir.Imports.equal import) seen in
        if seen_already then
          loop seen rest
        else
          loop (import :: seen) rest
  in
  loop [] imports

let compare_optional_string = fun left right ->
  match (left, right) with
  | (None, None) -> 0
  | (None, Some _) -> (-1)
  | (Some _, None) -> 1
  | (Some left, Some right) -> String.compare left right

let compare_imports = fun left right ->
  let by_from = String.compare Jir.Imports.(left.from) Jir.Imports.(right.from) in
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

let program = fun program ->
  let imports = collect_program_imports program |> dedupe_imports |> List.sort compare_imports in
  { program with imports }
