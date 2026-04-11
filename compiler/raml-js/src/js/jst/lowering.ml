open Std
module Source = Jir
module Target = Types

let lower_binder = fun (binder: Source.Binder.t) ->
  Target.Binder.make ~name:binder.name binder.binding_id

let unresolved_expr = fun kind ->
  raise
    (Invalid_argument
    (format
       Format.[
         str "RamlJs.Js.Jst.Lowering: unresolved JIR expression reached JST lowering (";
         str kind;
         str ")";
       ]))

let lower_import = fun (import: Source.Imports.requirement) ->
  let from = Source.Modules.import_path import.from in
  if import.namespace then
    Target.Import.{
      from;
      default = None;
      namespace = Some (lower_binder import.local);
      names = [];
    }
  else
    match import.imported with
    | None -> Target.Import.{
      from;
      default = Some (lower_binder import.local);
      namespace = None;
      names = [];
    }
    | Some imported ->
        Target.Import.{
          from;
          default = None;
          namespace = None;
          names = [ { imported; local = lower_binder import.local } ];
        }

let lower_literal = fun literal ->
  match literal with
  | Source.Literal.Undefined -> Target.Literal.Undefined
  | Source.Literal.Null -> Target.Literal.Null
  | Source.Literal.Bool value -> Target.Literal.Bool value
  | Source.Literal.Number (Source.Literal.Int value) -> Target.Literal.Number (Target.Literal.Int value)
  | Source.Literal.Number (Source.Literal.Float value) -> Target.Literal.Number (Target.Literal.Float value)
  | Source.Literal.String value -> Target.Literal.String value

let lower_unary_operator = fun operator ->
  match operator with
  | Source.Operator.Not -> Target.Operator.Not
  | Source.Operator.Negate -> Target.Operator.Negate

let lower_binary_operator = fun operator ->
  match operator with
  | Source.Operator.Add -> Target.Operator.Add
  | Source.Operator.Subtract -> Target.Operator.Subtract
  | Source.Operator.Multiply -> Target.Operator.Multiply
  | Source.Operator.Divide -> Target.Operator.Divide
  | Source.Operator.Modulo -> Target.Operator.Modulo
  | Source.Operator.Equal -> Target.Operator.Equal
  | Source.Operator.Not_equal -> Target.Operator.Not_equal
  | Source.Operator.Less_than -> Target.Operator.Less_than
  | Source.Operator.Less_or_equal -> Target.Operator.Less_or_equal
  | Source.Operator.Greater_than -> Target.Operator.Greater_than
  | Source.Operator.Greater_or_equal -> Target.Operator.Greater_or_equal

let rec lower_array_element = fun element ->
  match element with
  | Source.Expr.Item expr -> Target.Expr.Item (lower_expr expr)
  | Source.Expr.Spread expr -> Target.Expr.Spread (lower_expr expr)

let rec lower_expr = fun expr ->
  match expr with
  | Source.Expr.Literal literal -> Target.Expr.Literal (lower_literal literal)
  | Source.Expr.Global global -> Target.Expr.Global Target.Expr.{ name = global.name }
  | Source.Expr.Identifier entity_id -> Target.Expr.Identifier entity_id
  | Source.Expr.Imported _ ->
      unresolved_expr "imported"
  | Source.Expr.Runtime_helper _ ->
      unresolved_expr "runtime_helper"
  | Source.Expr.Unary unary -> Target.Expr.Unary Target.Expr.{
    operator = lower_unary_operator unary.operator;
    operand = lower_expr unary.operand;
  }
  | Source.Expr.Binary binary -> Target.Expr.Binary Target.Expr.{
    operator = lower_binary_operator binary.operator;
    left = lower_expr binary.left;
    right = lower_expr binary.right;
  }
  | Source.Expr.Array array ->
      Target.Expr.Array (List.map lower_array_element array)
  | Source.Expr.Function function_ -> Target.Expr.Function Target.Expr.{
    params = List.map lower_binder function_.params;
    body = List.map lower_statement function_.body;
  }
  | Source.Expr.Member member -> Target.Expr.Member Target.Expr.{
    object_ = lower_expr member.object_;
    property = member.property;
  }
  | Source.Expr.Index index -> Target.Expr.Index Target.Expr.{
    object_ = lower_expr index.object_;
    index = lower_expr index.index;
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
