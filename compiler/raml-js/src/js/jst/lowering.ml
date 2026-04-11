open Std
module Source = Jir
module Target = Types

let lower_binder = fun (binder: Source.Binder.t) ->
  Target.Binder.make ~name:binder.name binder.binding_id

let same_binder = fun (left: Target.Binder.t) (right: Target.Binder.t) ->
  Raml_core.Core_ir.Binding_id.equal left.binding_id right.binding_id

let lower_import = fun (import: Source.Imports.requirement) ->
  if import.namespace then
    Target.Import.{
      from = import.from;
      default = None;
      namespace = Some (lower_binder import.local);
      names = [];
    }
  else
    match import.imported with
    | None -> Target.Import.{
      from = import.from;
      default = Some (lower_binder import.local);
      namespace = None;
      names = [];
    }
    | Some imported ->
        Target.Import.{
          from = import.from;
          default = None;
          namespace = None;
          names = [ { imported; local = lower_binder import.local } ];
        }

let import_has_bindings = fun (import: Target.Import.t) ->
  match (import.default, import.namespace, import.names) with
  | (None, None, []) -> false
  | _ -> true

let choose_default_import = fun left right ->
  match left with
  | Some _ -> left
  | None -> right

let merge_imports = fun (left: Target.Import.t) (right: Target.Import.t) ->
  if not (String.equal left.from right.from) then
    None
  else
    match (left.namespace, right.namespace) with
    | (Some _, _)
    | (_, Some _) -> None
    | (None, None) ->
        if not (import_has_bindings left && import_has_bindings right) then
          None
        else
          match (left.default, right.default) with
          | (Some left_default, Some right_default) when not (same_binder left_default right_default) -> None
          | _ -> Some Target.Import.{
            from = left.from;
            default = choose_default_import left.default right.default;
            namespace = None;
            names = left.names @ right.names;
          }

let materialize_imports = fun imports ->
  let rec loop current rest =
    match rest with
    | [] -> [ current ]
    | next :: rest -> (
        match merge_imports current next with
        | Some merged -> loop merged rest
        | None -> current :: loop next rest
      )
  in
  match imports with
  | [] -> []
  | import :: rest -> loop import rest

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
  | Source.Expr.Identifier entity_id -> Target.Expr.Identifier entity_id
  | Source.Expr.Imported requirement ->
      Target.Expr.Identifier (Source.Binder.entity_id (Source.Imports.local requirement))
  | Source.Expr.Runtime_helper helper ->
      Target.Expr.Identifier (Source.Binder.entity_id helper.local)
  | Source.Expr.Function function_ -> Target.Expr.Function Target.Expr.{
    params = List.map lower_binder function_.params;
    body = List.map lower_statement function_.body;
  }
  | Source.Expr.Member member -> Target.Expr.Member Target.Expr.{
    object_ = lower_expr member.object_;
    property = member.property;
  }
  | Source.Expr.Call { callee; arguments } -> Target.Expr.Call Target.Expr.{
    callee = lower_expr callee;
    arguments = List.map lower_expr arguments;
  }
  | Source.Expr.Conditional conditional -> Target.Expr.Conditional Target.Expr.{
    condition = lower_expr conditional.condition;
    then_ = lower_expr conditional.then_;
    else_ = lower_expr conditional.else_;
  }
  | Source.Expr.Assignment assignment -> Target.Expr.Assignment Target.Expr.{
    target = assignment.target;
    value = lower_expr assignment.value;
  }

and lower_declaration = fun (declaration: Source.Declaration.t) ->
  let kind =
    match declaration.kind with
    | Source.Declaration.Const -> Target.Declaration.Const
    | Source.Declaration.Let -> Target.Declaration.Let
    | Source.Declaration.Var -> Target.Declaration.Var
  in
  let init = Option.map lower_expr declaration.init in
  Target.Declaration.{ kind; binder = lower_binder declaration.binder; init }

and lower_statement = fun statement ->
  match statement with
  | Source.Statement.Declaration declaration -> Target.Statement.Declaration (lower_declaration declaration)
  | Source.Statement.Block statements -> Target.Statement.Block (List.map lower_statement statements)
  | Source.Statement.Expression expr -> Target.Statement.Expression (lower_expr expr)
  | Source.Statement.Return expr -> Target.Statement.Return (lower_expr expr)
  | Source.Statement.If if_ -> Target.Statement.If Target.Statement.{
    condition = lower_expr if_.condition;
    then_ = List.map lower_statement if_.then_;
    else_ = List.map lower_statement if_.else_;
  }

let lower_export = fun (export: Source.Export.t) ->
  Target.Export.{ name = export.name; local = export.local }

let lower_program = fun (program: Source.Program.t) ->
  let import_items = program.imports
  |> List.map lower_import
  |> materialize_imports
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
    items = import_items @ statement_items @ export_items;
  }
