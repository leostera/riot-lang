open Std
open Std.Data

let indent = fun level ->
  String.make (level * 2) ' '

let emit_number = fun number ->
  match number with
  | Types.Literal.Int value -> string_of_int value
  | Types.Literal.Float value ->
      if Float.is_nan value then
        "NaN"
      else if Float.is_infinite value then
        if value < 0.0 then
          "-Infinity"
        else
          "Infinity"
      else
        string_of_float value

let emit_literal = fun literal ->
  match literal with
  | Types.Literal.Undefined -> "undefined"
  | Types.Literal.Null -> "null"
  | Types.Literal.Bool value ->
      if value then
        "true"
      else
        "false"
  | Types.Literal.Number number -> emit_number number
  | Types.Literal.String value -> Json.to_string (Json.string value)

let rec emit_expr = fun ~level expr ->
  match expr with
  | Types.Expr.Literal literal ->
      emit_literal literal
  | Types.Expr.Identifier name ->
      name
  | Types.Expr.Function function_ ->
      emit_function ~level function_
  | Types.Expr.Member member ->
      format Format.[ str (emit_member_object ~level member.object_); str "."; str member.property ]
  | Types.Expr.Call { callee; arguments } ->
      let arguments = arguments |> List.map (emit_expr ~level) |> String.concat ", " in
      format Format.[ str (emit_call_callee ~level callee); str "("; str arguments; str ")" ]
  | Types.Expr.Conditional conditional ->
      format
        Format.[
          str "(";
          str (emit_expr ~level conditional.condition);
          str " ? ";
          str (emit_expr ~level conditional.then_);
          str " : ";
          str (emit_expr ~level conditional.else_);
          str ")";
        ]
  | Types.Expr.Assignment assignment ->
      format
        Format.[
          str "(";
          str assignment.target;
          str " = ";
          str (emit_expr ~level assignment.value);
          str ")";
        ]

and emit_function = fun ~level function_ ->
  let body =
    match function_.body with
    | [] -> ""
    | statements -> format
      Format.[
        str "\n";
        str (emit_block ~level:(level + 1) statements);
        str "\n";
        str (indent level);
      ]
  in
  format
    Format.[
      str "(function(";
      str (String.concat ", " function_.params);
      str ") {";
      str body;
      str "})";
    ]

and emit_member_object = fun ~level expr ->
  match expr with
  | Types.Expr.Identifier _
  | Types.Expr.Member _
  | Types.Expr.Call _ -> emit_expr ~level expr
  | _ -> format Format.[ str "("; str (emit_expr ~level expr); str ")" ]

and emit_call_callee = fun ~level expr ->
  match expr with
  | Types.Expr.Identifier _
  | Types.Expr.Member _
  | Types.Expr.Call _ -> emit_expr ~level expr
  | _ -> format Format.[ str "("; str (emit_expr ~level expr); str ")" ]

and emit_statement = fun ~level statement ->
  let prefix = indent level in
  match statement with
  | Types.Statement.Declaration declaration ->
      let init =
        match declaration.init with
        | None -> ";"
        | Some init -> format Format.[ str " = "; str (emit_expr ~level init); str ";" ]
      in
      format
        Format.[
          str prefix;
          str (emit_declaration_kind declaration.kind);
          str " ";
          str declaration.name;
          str init;
        ]
  | Types.Statement.Block statements ->
      format Format.[ str prefix; str "{"; str (emit_statement_body ~level statements); str "}"; ]
  | Types.Statement.Expression expr ->
      format Format.[ str prefix; str (emit_expr ~level expr); str ";" ]
  | Types.Statement.Return expr ->
      format Format.[ str prefix; str "return "; str (emit_expr ~level expr); str ";" ]
  | Types.Statement.If if_ ->
      format
        Format.[
          str prefix;
          str "if (";
          str (emit_expr ~level if_.condition);
          str ") {";
          str (emit_statement_body ~level if_.then_);
          str "}";
          str " else {";
          str (emit_statement_body ~level if_.else_);
          str "}";
        ]

and emit_statement_body = fun ~level statements ->
  match statements with
  | [] -> ""
  | statements -> format
    Format.[
      str "\n";
      str (emit_block ~level:(level + 1) statements);
      str "\n";
      str (indent level);
    ]

and emit_block = fun ~level statements ->
  statements |> List.map (emit_statement ~level) |> String.concat "\n"

and emit_declaration_kind = fun kind ->
  match kind with
  | Types.Declaration.Const -> "const"
  | Types.Declaration.Let -> "let"
  | Types.Declaration.Var -> "var"

let emit_named_import = fun (named: Types.Import.named) ->
  match named.local with
  | None -> named.imported
  | Some local when String.equal local named.imported -> named.imported
  | Some local -> format Format.[ str named.imported; str " as "; str local ]

let emit_import = fun (import: Types.Import.t) ->
  match import.namespace with
  | Some namespace -> format
    Format.[
      str "import * as ";
      str namespace;
      str " from ";
      str (Json.to_string (Json.string import.from));
      str ";";
    ]
  | None ->
      let named =
        match import.names with
        | [] -> None
        | names -> Some (format
          Format.[ str "{ "; str (String.concat ", " (List.map emit_named_import names)); str " }" ])
      in
      let bindings =
        match (import.default, named) with
        | (None, None) -> None
        | (Some default, None) -> Some default
        | (None, Some named) -> Some named
        | (Some default, Some named) -> Some (format Format.[ str default; str ", "; str named ])
      in
      match bindings with
      | None -> format
        Format.[ str "import "; str (Json.to_string (Json.string import.from)); str ";" ]
      | Some bindings -> format
        Format.[
          str "import ";
          str bindings;
          str " from ";
          str (Json.to_string (Json.string import.from));
          str ";";
        ]

let emit_export = fun (export: Types.Export.t) ->
  if String.equal export.local export.name then
    format Format.[ str "  "; str export.local ]
  else
    format Format.[ str "  "; str export.local; str " as "; str export.name ]

let emit_exports = fun exports ->
  let lines = exports |> List.map emit_export |> String.concat ",\n" in
  format Format.[ str "export {\n"; str lines; str "\n};" ]

let emit_module_item = fun item ->
  match item with
  | Types.Module_item.Import import -> emit_import import
  | Types.Module_item.Statement statement -> emit_statement ~level:0 statement
  | Types.Module_item.Export exports -> emit_exports exports

let emit_program = fun (program: Types.Program.t) ->
  let sections =
    program.items
    |> List.map emit_module_item
    |> List.filter
      (fun section ->
        if String.equal section "" then
          false
        else
          true)
  in
  match sections with
  | [] -> ""
  | sections -> format Format.[ str (String.concat "\n\n" sections); str "\n" ]
