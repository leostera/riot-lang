open Std
open Std.Data
module Core = Raml_core.Core_ir
module Module_format = Module_format
module Syntax = Syntax

module Binding_map = Collections.Map.Make (struct
  type t = Core.Binding_id.t

  let compare = Core.Binding_id.compare
end)

type env = string Binding_map.t

let indent = fun level -> String.make ~len:(level * 2) ~char:' '

let lookup_binding_name = fun env binding_id ->
  Binding_map.get env ~key:binding_id
  |> Option.unwrap_or ~default:(Syntax.sanitize_binding_identifier (Core.Binding_id.name binding_id))

let emit_entity = fun env entity_id ->
  match Core.Entity_id.binding_id entity_id with
  | Some binding_id -> lookup_binding_name env binding_id
  | None -> Core.Entity_id.to_string entity_id

let remember_binding = fun env (binder: Types.Binder.t) ->
  Binding_map.insert env ~key:binder.binding_id ~value:binder.name

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

let emit_unary_operator = fun operator ->
  match operator with
  | Types.Operator.Not -> "!"
  | Types.Operator.Negate -> "-"

let emit_binary_operator = fun operator ->
  match operator with
  | Types.Operator.Add -> "+"
  | Types.Operator.Subtract -> "-"
  | Types.Operator.Multiply -> "*"
  | Types.Operator.Divide -> "/"
  | Types.Operator.Modulo -> "%"
  | Types.Operator.Equal -> "==="
  | Types.Operator.Not_equal -> "!=="
  | Types.Operator.Less_than -> "<"
  | Types.Operator.Less_or_equal -> "<="
  | Types.Operator.Greater_than -> ">"
  | Types.Operator.Greater_or_equal -> ">="

let rec emit_array_element = fun ~level env element ->
  match element with
  | Types.Expr.Item expr -> emit_expr ~level env expr
  | Types.Expr.Spread expr -> format Format.[ str "..."; str (emit_expr ~level env expr) ]

and emit_object_field = fun ~level env (field: Types.Expr.object_field) ->
  let emitted_value = emit_expr ~level env field.value in
  let key =
    if Syntax.can_use_unquoted_object_key field.name then
      field.name
    else
      Json.to_string (Json.string field.name)
  in
  match field.value with
  | Types.Expr.Identifier entity_id when Syntax.can_use_unquoted_object_key field.name
  && String.equal field.name (emit_entity env entity_id) -> field.name
  | _ -> format Format.[ str key; str ": "; str emitted_value ]

and emit_expr = fun ~level env expr ->
  match expr with
  | Types.Expr.Literal literal ->
      emit_literal literal
  | Types.Expr.Global global ->
      global.name
  | Types.Expr.Identifier entity_id ->
      emit_entity env entity_id
  | Types.Expr.Unary unary ->
      format
        Format.[
          str "(";
          str (emit_unary_operator unary.operator);
          str (emit_expr ~level env unary.operand);
          str ")";
        ]
  | Types.Expr.Binary binary ->
      format
        Format.[
          str "(";
          str (emit_expr ~level env binary.left);
          str " ";
          str (emit_binary_operator binary.operator);
          str " ";
          str (emit_expr ~level env binary.right);
          str ")";
        ]
  | Types.Expr.Array elements ->
      format
        Format.[
          str "[";
          str (String.concat ", " (List.map elements ~fn:(emit_array_element ~level env)));
          str "]";
        ]
  | Types.Expr.Object fields ->
      format
        Format.[
          str "{";
          str (String.concat ", " (List.map fields ~fn:(emit_object_field ~level env)));
          str "}";
        ]
  | Types.Expr.Function function_ ->
      emit_function ~level env function_
  | Types.Expr.Member member ->
      format
        Format.[ str (emit_member_object ~level env member.object_); str "."; str member.property ]
  | Types.Expr.Index index ->
      format
        Format.[
          str (emit_member_object ~level env index.object_);
          str "[";
          str (emit_expr ~level env index.index);
          str "]";
        ]
  | Types.Expr.Call { callee; arguments } ->
      let arguments = arguments |> List.map ~fn:(emit_expr ~level env) |> String.concat ", " in
      format Format.[ str (emit_call_callee ~level env callee); str "("; str arguments; str ")" ]
  | Types.Expr.Conditional conditional ->
      format
        Format.[
          str "(";
          str (emit_expr ~level env conditional.condition);
          str " ? ";
          str (emit_expr ~level env conditional.then_);
          str " : ";
          str (emit_expr ~level env conditional.else_);
          str ")";
        ]
  | Types.Expr.Assignment assignment ->
      format
        Format.[
          str "(";
          str (emit_entity env assignment.target);
          str " = ";
          str (emit_expr ~level env assignment.value);
          str ")";
        ]

and emit_function = fun ~level env function_ ->
  let env = List.fold_left function_.params ~init:env ~fn:remember_binding in
  let body =
    match function_.body with
    | [] -> ""
    | statements ->
        let (body, _) = emit_block ~level:(level + 1) env statements in
        format Format.[ str "\n"; str body; str "\n"; str (indent level); ]
  in
  format
    Format.[
      str "(function(";
      str
        (String.concat
          ", "
          (List.map function_.params ~fn:(fun (binder: Types.Binder.t) -> binder.name)));
      str ") {";
      str body;
      str "})";
    ]

and emit_member_object = fun ~level env expr ->
  match expr with
  | Types.Expr.Global _
  | Types.Expr.Identifier _
  | Types.Expr.Member _
  | Types.Expr.Index _
  | Types.Expr.Call _ -> emit_expr ~level env expr
  | _ -> format Format.[ str "("; str (emit_expr ~level env expr); str ")" ]

and emit_call_callee = fun ~level env expr ->
  match expr with
  | Types.Expr.Global _
  | Types.Expr.Identifier _
  | Types.Expr.Member _
  | Types.Expr.Index _
  | Types.Expr.Call _ -> emit_expr ~level env expr
  | _ -> format Format.[ str "("; str (emit_expr ~level env expr); str ")" ]

and emit_statement = fun ~level env statement ->
  let prefix = indent level in
  match statement with
  | Types.Statement.Declaration declaration ->
      let init =
        match declaration.init with
        | None -> ";"
        | Some init -> format Format.[ str " = "; str (emit_expr ~level env init); str ";" ]
      in
      let line = format
        Format.[
          str prefix;
          str (emit_declaration_kind declaration.kind);
          str " ";
          str declaration.binder.name;
          str init;
        ] in
      (line, remember_binding env declaration.binder)
  | Types.Statement.Block statements ->
      let (body, _) = emit_block ~level:(level + 1) env statements in
      let line =
        if String.equal body "" then
          format Format.[ str prefix; str "{}" ]
        else
          format Format.[ str prefix; str "{\n"; str body; str "\n"; str prefix; str "}"; ]
      in
      (line, env)
  | Types.Statement.Expression expr ->
      (format Format.[ str prefix; str (emit_expr ~level env expr); str ";" ], env)
  | Types.Statement.Return expr ->
      (format Format.[ str prefix; str "return "; str (emit_expr ~level env expr); str ";" ], env)
  | Types.Statement.If if_ ->
      let (then_body, _) = emit_block ~level:(level + 1) env if_.then_ in
      let (else_body, _) = emit_block ~level:(level + 1) env if_.else_ in
      let line = format
        Format.[
          str prefix;
          str "if (";
          str (emit_expr ~level env if_.condition);
          str ") {";
          str (emit_statement_body ~level then_body);
          str "}";
          str " else {";
          str (emit_statement_body ~level else_body);
          str "}";
        ] in
      (line, env)

and emit_statement_body = fun ~level body ->
  if String.equal body "" then
    ""
  else
    format Format.[ str "\n"; str body; str "\n"; str (indent level); ]

and emit_block = fun ~level env statements ->
  match statements with
  | [] -> ("", env)
  | statement :: rest ->
      let (statement, env) = emit_statement ~level env statement in
      let (rest, env) = emit_block ~level env rest in
      if String.equal rest "" then
        (statement, env)
      else
        (format Format.[ str statement; str "\n"; str rest ], env)

and emit_declaration_kind = fun kind ->
  match kind with
  | Types.Declaration.Const -> "const"
  | Types.Declaration.Let -> "let"
  | Types.Declaration.Var -> "var"

let emit_named_import = fun (named: Types.Import.named) ->
  if String.equal named.local.name named.imported then
    named.imported
  else
    format Format.[ str named.imported; str " as "; str named.local.name ]

let emit_import_path = fun (module_ref: Types.module_ref) ->
  Json.to_string (Json.string module_ref.import_path)

let emit_import = fun ~module_format env (import: Types.Import.t) ->
  let env =
    env
    |> (fun env ->
      match import.default with
      | None -> env
      | Some binder -> remember_binding env binder)
    |> (fun env ->
      match import.namespace with
      | None -> env
      | Some binder -> remember_binding env binder)
    |> (fun env ->
      List.fold_left
        import.names
        ~init:env
        ~fn:(fun env (named: Types.Import.named) -> remember_binding env named.local))
  in
  let line =
    match module_format with
    | Module_format.Esm ->
        match import.namespace with
        | Some namespace -> format
          Format.[
            str "import * as ";
            str namespace.name;
            str " from ";
            str (emit_import_path import.from);
            str ";";
          ]
        | None ->
            let named =
              match import.names with
              | [] -> None
              | names -> Some (format
                Format.[
                  str "{ ";
                  str (String.concat ", " (List.map names ~fn:emit_named_import));
                  str " }"
                ])
            in
            let bindings =
              match (import.default, named) with
              | (None, None) -> None
              | (Some default, None) -> Some default.name
              | (None, Some named) -> Some named
              | (Some default, Some named) -> Some (format
                Format.[ str default.name; str ", "; str named ])
            in
            match bindings with
            | None -> format Format.[ str "import "; str (emit_import_path import.from); str ";" ]
            | Some bindings -> format
              Format.[
                str "import ";
                str bindings;
                str " from ";
                str (emit_import_path import.from);
                str ";";
              ]
  in
  (line, env)

let emit_export = fun env (export: Types.Export.t) ->
  let local = emit_entity env export.local in
  if String.equal local export.name then
    format Format.[ str "  "; str local ]
  else
    format Format.[ str "  "; str local; str " as "; str export.name ]

let emit_exports = fun ~module_format env exports ->
  match module_format with
  | Module_format.Esm ->
      let lines = exports |> List.map ~fn:(emit_export env) |> String.concat ",\n" in
      format Format.[ str "export {\n"; str lines; str "\n};" ]

let emit_module_item = fun ~module_format env item ->
  match item with
  | Types.Module_item.Import import -> emit_import ~module_format env import
  | Types.Module_item.Statement statement -> emit_statement ~level:0 env statement
  | Types.Module_item.Export exports -> (emit_exports ~module_format env exports, env)

let emit_program = fun ~context (program: Types.Program.t) ->
  let module_format = Module_format.from_context context in
  let (sections_rev, _) =
    List.fold_left program.items ~init:([], Binding_map.empty)
      ~fn:(fun (sections_rev, env) item ->
        let (section, env) = emit_module_item ~module_format env item in
        if String.equal section "" then
          (sections_rev, env)
        else
          (section :: sections_rev, env))
  in
  match List.rev sections_rev with
  | [] -> ""
  | sections -> format Format.[ str (String.concat "\n\n" sections); str "\n" ]
