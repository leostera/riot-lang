open Std
module Source = Jir
module Target = Types

let lower_import = fun (import: Source.Imports.requirement) ->
  if import.namespace then
    Target.Import.{ from = import.from; default = None; namespace = Some import.local; names = [] }
  else
    match import.imported with
    | None -> Target.Import.{
      from = import.from;
      default = Some import.local;
      namespace = None;
      names = []
    }
    | Some imported ->
        let local =
          if String.equal imported import.local then
            None
          else
            Some import.local
        in
        Target.Import.{
          from = import.from;
          default = None;
          namespace = None;
          names = [ { imported; local } ]
        }

let lower_literal = fun literal ->
  match literal with
  | Source.Literal.Undefined -> Target.Literal.Undefined
  | Source.Literal.Null -> Target.Literal.Null
  | Source.Literal.Bool value -> Target.Literal.Bool value
  | Source.Literal.Number (Source.Literal.Int value) -> Target.Literal.Number (Target.Literal.Int value)
  | Source.Literal.Number (Source.Literal.Float value) -> Target.Literal.Number (Target.Literal.Float value)
  | Source.Literal.String value -> Target.Literal.String value

let rec lower_expr = fun expr ->
  match expr with
  | Source.Expr.Literal literal -> Target.Expr.Literal (lower_literal literal)
  | Source.Expr.Identifier name -> Target.Expr.Identifier name
  | Source.Expr.Imported requirement -> Target.Expr.Identifier (Source.Imports.local requirement)
  | Source.Expr.Runtime_helper helper -> Target.Expr.Identifier helper.local
  | Source.Expr.Function function_ -> Target.Expr.Function Target.Expr.{
    params = function_.params;
    body = List.map lower_statement function_.body
  }
  | Source.Expr.Member member -> Target.Expr.Member Target.Expr.{
    object_ = lower_expr member.object_;
    property = member.property
  }
  | Source.Expr.Call { callee; arguments } -> Target.Expr.Call Target.Expr.{
    callee = lower_expr callee;
    arguments = List.map lower_expr arguments
  }
  | Source.Expr.Conditional conditional -> Target.Expr.Conditional Target.Expr.{
    condition = lower_expr conditional.condition;
    then_ = lower_expr conditional.then_;
    else_ = lower_expr conditional.else_
  }
  | Source.Expr.Assignment assignment -> Target.Expr.Assignment Target.Expr.{
    target = assignment.target;
    value = lower_expr assignment.value
  }

and lower_declaration = fun (declaration: Source.Declaration.t) ->
  let kind =
    match declaration.kind with
    | Source.Declaration.Const -> Target.Declaration.Const
    | Source.Declaration.Let -> Target.Declaration.Let
    | Source.Declaration.Var -> Target.Declaration.Var
  in
  let init = Option.map lower_expr declaration.init in
  Target.Declaration.{ kind; name = declaration.name; init }

and lower_statement = fun statement ->
  match statement with
  | Source.Statement.Declaration declaration -> Target.Statement.Declaration (lower_declaration declaration)
  | Source.Statement.Expression expr -> Target.Statement.Expression (lower_expr expr)
  | Source.Statement.Return expr -> Target.Statement.Return (lower_expr expr)

let lower_export = fun (export: Source.Export.t) ->
  Target.Export.{ name = export.name; local = export.local }

let lower_program = fun (program: Source.Program.t) ->
  let program = Source.Passes.Normalize.program program in
  let import_items = program.imports
  |> List.map lower_import
  |> List.map (fun import -> Target.Module_item.Import import) in
  let statement_items = program.body
  |> List.map lower_statement
  |> List.map (fun statement -> Target.Module_item.Statement statement) in
  let export_items =
    match program.exports with
    | [] -> []
    | exports -> [ Target.Module_item.Export (List.map lower_export exports) ]
  in
  Target.Program.{
    module_name = program.module_name;
    items = import_items @ statement_items @ export_items
  }
