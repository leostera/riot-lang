open Std
open Std.Data
module Source_unit = Raml_core.Source_unit
module Core_ir = Raml_core.Core_ir

let ( let* ) = Result.and_then

let missing_field = fun scope field ->
  Error (format Format.[ str scope; str " is missing field `"; str field; str "`" ])

let invalid_field = fun scope field expected ->
  Error (format Format.[ str scope; str "."; str field; str " must be "; str expected ])

let map_results = fun items f ->
  List.fold_right
    (fun item acc ->
      let* item = f item in
      let* acc = acc in
      Ok (item :: acc))
    items
    (Ok [])

let field = fun scope name json ->
  match Json.get_field name json with
  | Some value -> Ok value
  | None -> missing_field scope name

let string_field = fun scope name json ->
  let* value = field scope name json in
  match Json.get_string value with
  | Some value -> Ok value
  | None -> invalid_field scope name "a string"

let array_field = fun scope name json ->
  let* value = field scope name json in
  match Json.get_array value with
  | Some value -> Ok value
  | None -> invalid_field scope name "an array"

let float_of_json = fun json ->
  match json with
  | Json.Float value -> Some value
  | Json.Int value -> Some (float_of_int value)
  | _ -> None

let parse_source_kind = fun scope json ->
  let* kind = string_field scope "kind" json in
  match kind with
  | "implementation" -> Ok Source_unit.Implementation
  | "interface" -> Ok Source_unit.Interface
  | _ -> invalid_field scope "kind" "`implementation` or `interface`"

let parse_rec_flag = fun scope json ->
  let* rec_flag = string_field scope "rec_flag" json in
  match rec_flag with
  | "nonrecursive" -> Ok Core_ir.Rec_flag.Nonrecursive
  | "recursive" -> Ok Core_ir.Rec_flag.Recursive
  | _ -> invalid_field scope "rec_flag" "`nonrecursive` or `recursive`"

let parse_unit_id = fun json ->
  let scope = "unit_id" in
  let* relpath = string_field scope "relpath" json in
  let* relpath =
    Result.map_error
      (fun _ -> format Format.[ str scope; str ".relpath must be a valid path" ])
      (Path.from_string relpath)
  in
  let* unit_name = string_field scope "unit_name" json in
  let* kind = parse_source_kind scope json in
  Ok Core_ir.Unit_id.{ relpath; unit_name; kind }

let parse_constant = fun json ->
  let scope = "constant" in
  let* kind = string_field scope "kind" json in
  match kind with
  | "unit" ->
      Ok Core_ir.Constant.Unit
  | "bool" ->
      let* value = field scope "value" json in
      let value =
        match Json.get_bool value with
        | Some value -> Ok value
        | None -> invalid_field scope "value" "a boolean"
      in
      Result.map (fun value -> Core_ir.Constant.Bool value) value
  | "int" ->
      let* value = field scope "value" json in
      let value =
        match Json.get_int value with
        | Some value -> Ok value
        | None -> invalid_field scope "value" "an integer"
      in
      Result.map (fun value -> Core_ir.Constant.Int value) value
  | "float" ->
      let* value = field scope "value" json in
      let value =
        match float_of_json value with
        | Some value -> Ok value
        | None -> invalid_field scope "value" "a number"
      in
      Result.map (fun value -> Core_ir.Constant.Float value) value
  | "char" ->
      let* value = string_field scope "value" json in
      Ok (Core_ir.Constant.Char value)
  | "string" ->
      let* value = string_field scope "value" json in
      Ok (Core_ir.Constant.String value)
  | _ ->
      invalid_field scope "kind" "`unit`, `bool`, `int`, `float`, `char`, or `string`"

let parse_surface_path = fun scope json ->
  match Json.get_string json with
  | Some value -> Ok (Core_ir.Surface_path.from_string value)
  | None -> invalid_field scope "surface_path" "a string"

let parse_binding_id = fun json ->
  let scope = "binding_id" in
  let* kind = string_field scope "kind" json in
  match kind with
  | "local" ->
      let* name = string_field scope "name" json in
      let* stamp_json = field scope "stamp" json in
      let* stamp =
        match Json.get_int stamp_json with
        | Some stamp -> Ok stamp
        | None -> invalid_field scope "stamp" "an integer"
      in
      Ok (Core_ir.Binding_id.local ~stamp ~name)
  | "predef" ->
      let* name = string_field scope "name" json in
      let* stamp_json = field scope "stamp" json in
      let* stamp =
        match Json.get_int stamp_json with
        | Some stamp -> Ok stamp
        | None -> invalid_field scope "stamp" "an integer"
      in
      Ok (Core_ir.Binding_id.predef ~stamp ~name)
  | "persistent" ->
      let* surface_path_json = field scope "surface_path" json in
      let* surface_path = parse_surface_path scope surface_path_json in
      Ok (Core_ir.Binding_id.persistent surface_path)
  | _ ->
      invalid_field scope "kind" "`local`, `predef`, or `persistent`"

let parse_entity_id = fun json ->
  match Json.get_string json with
  | Some value -> Ok (Core_ir.Entity_id.from_string value)
  | None ->
      let scope = "entity_id" in
      let* kind = string_field scope "kind" json in
      let* surface_path_json = field scope "surface_path" json in
      let* surface_path = parse_surface_path scope surface_path_json in
      match kind with
      | "unresolved" ->
          Ok (Core_ir.Entity_id.from_surface_path surface_path)
      | "resolved" ->
          let* binding_id_json = field scope "binding_id" json in
          let* binding_id = parse_binding_id binding_id_json in
          Ok (Core_ir.Entity_id.resolved ~binding_id ~surface_path)
      | _ ->
          invalid_field scope "kind" "`unresolved` or `resolved`"

let rec parse_expr = fun json ->
  let scope = "expr" in
  let* kind = string_field scope "kind" json in
  match kind with
  | "constant" ->
      let* constant_json = field scope "constant" json in
      let* constant = parse_constant constant_json in
      Ok (Core_ir.Expr.Constant constant)
  | "var" ->
      let* name_json = field scope "name" json in
      let* name = parse_entity_id name_json in
      Ok (Core_ir.Expr.Var name)
  | "apply" ->
      let* apply_json = field scope "apply" json in
      let* apply = parse_apply apply_json in
      Ok (Core_ir.Expr.Apply apply)
  | "lambda" ->
      let* lambda_json = field scope "lambda" json in
      let* lambda = parse_lambda lambda_json in
      Ok (Core_ir.Expr.Lambda lambda)
  | "let" ->
      let* let_json = field scope "let" json in
      let* let_ = parse_let let_json in
      Ok (Core_ir.Expr.Let let_)
  | "sequence" ->
      let* sequence_json = field scope "sequence" json in
      let* sequence = parse_sequence sequence_json in
      Ok (Core_ir.Expr.Sequence sequence)
  | "if_then_else" ->
      let* if_json = field scope "if_then_else" json in
      let* if_then_else = parse_if_then_else if_json in
      Ok (Core_ir.Expr.If_then_else if_then_else)
  | "primitive" ->
      let* primitive_json = field scope "primitive" json in
      let* primitive = parse_primitive primitive_json in
      Ok (Core_ir.Expr.Primitive primitive)
  | _ ->
      invalid_field scope "kind" "`constant`, `var`, `apply`, `lambda`, `let`, `sequence`, `if_then_else`, or `primitive`"

and parse_apply_callee = fun json ->
  let scope = "callee" in
  let* kind = string_field scope "kind" json in
  match kind with
  | "direct" ->
      let* function_json = field scope "function" json in
      let* function_name = parse_entity_id function_json in
      Ok (Core_ir.Expr.Direct function_name)
  | "indirect" ->
      let* expr_json = field scope "expr" json in
      let* expr = parse_expr expr_json in
      Ok (Core_ir.Expr.Indirect expr)
  | _ ->
      invalid_field scope "kind" "`direct` or `indirect`"

and parse_apply = fun json ->
  let scope = "apply" in
  let* callee_json = field scope "callee" json in
  let* callee = parse_apply_callee callee_json in
  let* arguments = array_field scope "arguments" json in
  let* arguments = map_results arguments parse_expr in
  Ok Core_ir.Expr.{ callee; arguments }

and parse_lambda = fun json ->
  let scope = "lambda" in
  let* params = array_field scope "params" json in
  let* params =
    map_results params
      (fun json ->
        match Json.get_string json with
        | Some value -> Ok Core_ir.Expr.{ entity_id = Core_ir.Entity_id.from_name value; name = value }
        | None ->
            let entry_scope = "lambda.param" in
            let* entity_id =
              match Json.get_field "entity_id" json with
              | Some entity_id_json -> parse_entity_id entity_id_json
              | None ->
                  let* name = string_field entry_scope "name" json in
                  Ok (Core_ir.Entity_id.from_name name)
            in
            let* name = string_field entry_scope "name" json in
            Ok Core_ir.Expr.{ entity_id; name })
  in
  let* body_json = field scope "body" json in
  let* body = parse_expr body_json in
  Ok Core_ir.Expr.{ params; body }

and parse_expr_binding = fun json ->
  let scope = "binding" in
  let* entity_id =
    match Json.get_field "entity_id" json with
    | Some entity_id_json -> parse_entity_id entity_id_json
    | None ->
        let* name = string_field scope "name" json in
        Ok (Core_ir.Entity_id.from_name name)
  in
  let* name = string_field scope "name" json in
  let* expr_json = field scope "expr" json in
  let* expr = parse_expr expr_json in
  Ok Core_ir.Expr.{ entity_id; name; expr }

and parse_let = fun json ->
  let scope = "let" in
  let* rec_flag = parse_rec_flag scope json in
  let* bindings = array_field scope "bindings" json in
  let* bindings = map_results bindings parse_expr_binding in
  let* body_json = field scope "body" json in
  let* body = parse_expr body_json in
  Ok Core_ir.Expr.{ rec_flag; bindings; body }

and parse_sequence = fun json ->
  let scope = "sequence" in
  let* first_json = field scope "first" json in
  let* first = parse_expr first_json in
  let* second_json = field scope "second" json in
  let* second = parse_expr second_json in
  Ok Core_ir.Expr.{ first; second }

and parse_if_then_else = fun json ->
  let scope = "if_then_else" in
  let* condition_json = field scope "condition" json in
  let* condition = parse_expr condition_json in
  let* then_json = field scope "then" json in
  let* then_ = parse_expr then_json in
  let* else_json = field scope "else" json in
  let* else_ = parse_expr else_json in
  Ok Core_ir.Expr.{ condition; then_; else_ }

and parse_primitive = fun json ->
  let scope = "primitive" in
  let* name = string_field scope "name" json in
  let* arguments = array_field scope "arguments" json in
  let* arguments = map_results arguments parse_expr in
  let normalized_name =
    match name with
    | "%addfloat" -> "add_float"
    | "%subfloat" -> "subtract_float"
    | "%mulfloat" -> "multiply_float"
    | "%divfloat" -> "divide_float"
    | "%addint" -> "add_int"
    | "%subint" -> "subtract_int"
    | "%mulint" -> "multiply_int"
    | "%divint" -> "divide_int"
    | "%modint" -> "modulo_int"
    | "%concatstring" -> "concatenate_string"
    | "%string_of_int" -> "int_to_string"
    | "%string_of_float" -> "float_to_string"
    | "%int_of_string" -> "int_of_string"
    | "%float_of_string" -> "float_of_string"
    | "%eq" -> "equal"
    | "%neq" -> "not_equal"
    | "%lt" -> "less_than"
    | "%le" -> "less_or_equal"
    | "%gt" -> "greater_than"
    | "%ge" -> "greater_or_equal"
    | "%sqrtfloat" -> "float_sqrt"
    | "%tuple_make" -> "tuple_make"
    | "%tuple_get" -> "tuple_get"
    | "%trace" -> "trace"
    | other -> other
  in
  match Core_ir.Primitive.from_string normalized_name with
  | Some primitive -> Ok Core_ir.Expr.{ primitive; arguments }
  | None -> invalid_field scope "name" "a known Core IR primitive name"

let parse_binding = fun json ->
  let scope = "binding" in
  let* entity_id =
    match Json.get_field "entity_id" json with
    | Some entity_id_json -> parse_entity_id entity_id_json
    | None ->
        let* name = string_field scope "name" json in
        Ok (Core_ir.Entity_id.from_name name)
  in
  let* name = string_field scope "name" json in
  let* expr_json = field scope "expr" json in
  let* expr = parse_expr expr_json in
  Ok Core_ir.Binding.{ entity_id; name; expr }

let parse_export = fun json ->
  let scope = "export" in
  let* name = string_field scope "name" json in
  let* symbol_json = field scope "symbol" json in
  let* symbol = parse_entity_id symbol_json in
  Ok Core_ir.Export.{ name; symbol }

let parse_init_item = fun json ->
  let scope = "item" in
  let* kind = string_field scope "kind" json in
  match kind with
  | "binding" ->
      let* binding_json = field scope "binding" json in
      let* binding = parse_binding binding_json in
      Ok (Core_ir.Init_item.Binding binding)
  | "eval" ->
      let* expr_json = field scope "expr" json in
      let* expr = parse_expr expr_json in
      Ok (Core_ir.Init_item.Eval expr)
  | _ ->
      invalid_field scope "kind" "`binding` or `eval`"

let parse_binding_group = fun json ->
  let scope = "group" in
  let* rec_flag = parse_rec_flag scope json in
  let* items = array_field scope "items" json in
  let* items = map_results items parse_init_item in
  let* exports = array_field scope "exports" json in
  let* exports = map_results exports parse_export in
  Ok Core_ir.Binding_group.{ rec_flag; items; exports }

let parse_compilation_unit = fun json ->
  let* unit_id_json = field "compilation_unit" "unit_id" json in
  let* unit_id = parse_unit_id unit_id_json in
  let* exports = array_field "compilation_unit" "exports" json in
  let* exports = map_results exports parse_export in
  let* init = array_field "compilation_unit" "init" json in
  let* init = map_results init parse_binding_group in
  Ok Core_ir.Compilation_unit.{ unit_id; exports; init }
