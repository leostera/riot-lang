open Std
open Std.Data
module Jir = Raml.Js.Jir
module Core = Raml.CoreIR

let ( let* ) = Result.and_then

let fixtures_dir = Path.v "compiler/raml/tests/fixtures/jir"

let snapshots_dir = Path.v "compiler/raml/tests/fixtures/js"

let append_snapshot_suffix = fun path suffix ->
  format Format.[ str (Path.to_string (Path.remove_extension path)); str suffix ]
  |> Path.from_string
  |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let snapshot_path = fun ~(ctx:Test.FixtureRunner.ctx) ->
  Path.join snapshots_dir ctx.fixture_relpath |> fun path -> append_snapshot_suffix path ".expected"

let with_snapshot_path = fun path (ctx: Test.ctx) ->
  let fixture =
    match ctx.fixture with
    | Some fixture -> { fixture with snapshot_path = Some path }
    | None -> panic "expected fixture-backed test context"
  in
  Test.Context.with_fixture ctx fixture

let keep_json = fun path ->
  match Path.extension path with
  | Some ".json" -> `keep
  | _ -> `skip

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

let binder_of_name = fun name ->
  let path = Core.Surface_path.from_segments [ "__jir_fixture"; name ] in
  Jir.Binder.make ~name (Core.Binding_id.persistent path)

let entity_of_name = fun name -> binder_of_name name |> Jir.Binder.entity_id

let strip_js_extension = fun value ->
  if String.ends_with ~suffix:".js" value then
    String.sub value 0 (String.length value - 3)
  else
    value

let parse_module_ref = fun scope value ->
  let value =
    if String.starts_with ~prefix:"./" value then
      String.sub value 2 (String.length value - 2)
    else
      value
  in
  let unit_name = strip_js_extension value in
  if String.equal unit_name "riot-runtime" then
    Ok (Jir.Modules.runtime unit_name)
  else if String.equal unit_name "" then
    invalid_field scope "from" "a non-empty module path"
  else
    Ok (Jir.Modules.sibling_unit unit_name)

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

let parse_number = fun json ->
  let scope = "number" in
  let* kind = string_field scope "kind" json in
  match kind with
  | "int" ->
      let* value = field scope "value" json in
      let value =
        match Json.get_int value with
        | Some value -> Ok value
        | None -> invalid_field scope "value" "an integer"
      in
      Result.map (fun value -> Jir.Literal.Int value) value
  | "float" ->
      let* value = field scope "value" json in
      let value =
        match float_of_json value with
        | Some value -> Ok value
        | None -> invalid_field scope "value" "a number"
      in
      Result.map (fun value -> Jir.Literal.Float value) value
  | _ ->
      invalid_field scope "kind" "`int` or `float`"

let parse_literal = fun json ->
  let scope = "literal" in
  let* kind = string_field scope "kind" json in
  match kind with
  | "undefined" ->
      Ok Jir.Literal.Undefined
  | "null" ->
      Ok Jir.Literal.Null
  | "bool" ->
      let* value = field scope "value" json in
      let value =
        match Json.get_bool value with
        | Some value -> Ok value
        | None -> invalid_field scope "value" "a boolean"
      in
      Result.map (fun value -> Jir.Literal.Bool value) value
  | "number" ->
      let* number_json = field scope "number" json in
      let* number = parse_number number_json in
      Ok (Jir.Literal.Number number)
  | "string" ->
      let* value = string_field scope "value" json in
      Ok (Jir.Literal.String value)
  | _ ->
      invalid_field scope "kind" "`undefined`, `null`, `bool`, `number`, or `string`"

let parse_import = fun json ->
  let scope = "import" in
  let* from = string_field scope "from" json in
  let* from = parse_module_ref scope from in
  let namespace =
    match Json.get_field "namespace" json with
    | None -> Ok false
    | Some value -> (
        match Json.get_bool value with
        | Some value -> Ok value
        | None -> invalid_field scope "namespace" "a boolean"
      )
  in
  let* namespace = namespace in
  let imported =
    match Json.get_field "imported" json with
    | None
    | Some Json.Null -> Ok None
    | Some value -> (
        match Json.get_string value with
        | Some value -> Ok (Some value)
        | None -> invalid_field scope "imported" "a string or null"
      )
  in
  let* imported = imported in
  let* local = string_field scope "local" json in
  let local = binder_of_name local in
  if namespace then
    Ok (Jir.Imports.namespace ~from ~local ())
  else
    Ok (Jir.Imports.make ~from ?imported ~local ())

let parse_runtime = fun json ->
  let scope = "runtime" in
  let* module_name = string_field scope "module_name" json in
  let* module_ref = parse_module_ref scope module_name in
  let* symbol = string_field scope "symbol" json in
  let* local = string_field scope "local" json in
  Ok (Jir.Runtime.make ~module_ref ~symbol ~local:(binder_of_name local) ())

let rec parse_expr = fun json ->
  let scope = "expr" in
  let* kind = string_field scope "kind" json in
  match kind with
  | "literal" ->
      let* literal_json = field scope "literal" json in
      let* literal = parse_literal literal_json in
      Ok (Jir.Expr.Literal literal)
  | "identifier" ->
      let* name = string_field scope "name" json in
      Ok (Jir.Expr.Identifier (entity_of_name name))
  | "imported" ->
      let* import_json = field scope "import" json in
      let* import = parse_import import_json in
      Ok (Jir.Expr.Imported import)
  | "runtime" ->
      let* helper_json = field scope "helper" json in
      let* helper = parse_runtime helper_json in
      Ok (Jir.Expr.Runtime_helper helper)
  | "function" ->
      let* function_json = field scope "function" json in
      let* function_ = parse_function function_json in
      Ok (Jir.Expr.Function function_)
  | "member" ->
      let* member_json = field scope "member" json in
      let* member = parse_member member_json in
      Ok (Jir.Expr.Member member)
  | "call" ->
      let* call_json = field scope "call" json in
      let* call = parse_call call_json in
      Ok (Jir.Expr.Call call)
  | "conditional" ->
      let* conditional_json = field scope "conditional" json in
      let* conditional = parse_conditional conditional_json in
      Ok (Jir.Expr.Conditional conditional)
  | "assignment" ->
      let* assignment_json = field scope "assignment" json in
      let* assignment = parse_assignment assignment_json in
      Ok (Jir.Expr.Assignment assignment)
  | _ ->
      invalid_field scope "kind" "`literal`, `identifier`, `imported`, `runtime`, `function`, `member`, `call`, `conditional`, or `assignment`"

and parse_call = fun json ->
  let scope = "call" in
  let* callee_json = field scope "callee" json in
  let* callee = parse_expr callee_json in
  let* arguments = array_field scope "arguments" json in
  let* arguments = map_results arguments parse_expr in
  Ok Jir.Expr.{ callee; arguments }

and parse_function = fun json ->
  let scope = "function" in
  let* params = array_field scope "params" json in
  let* params =
    map_results params
      (fun json ->
        match Json.get_string json with
        | Some value -> Ok (binder_of_name value)
        | None -> invalid_field scope "params" "an array of strings")
  in
  let* body = array_field scope "body" json in
  let* body = map_results body parse_statement in
  Ok Jir.Expr.{ params; body }

and parse_member = fun json ->
  let scope = "member" in
  let* object_json = field scope "object" json in
  let* object_ = parse_expr object_json in
  let* property = string_field scope "property" json in
  Ok Jir.Expr.{ object_; property }

and parse_conditional = fun json ->
  let scope = "conditional" in
  let* condition_json = field scope "condition" json in
  let* condition = parse_expr condition_json in
  let* then_json = field scope "then" json in
  let* then_ = parse_expr then_json in
  let* else_json = field scope "else" json in
  let* else_ = parse_expr else_json in
  Ok Jir.Expr.{ condition; then_; else_ }

and parse_assignment = fun json ->
  let scope = "assignment" in
  let* target = string_field scope "target" json in
  let* value_json = field scope "value" json in
  let* value = parse_expr value_json in
  Ok Jir.Expr.{ target = entity_of_name target; value }

and parse_declaration_kind = fun json ->
  let scope = "declaration" in
  let* kind = string_field scope "kind" json in
  match kind with
  | "const" -> Ok Jir.Declaration.Const
  | "let" -> Ok Jir.Declaration.Let
  | "var" -> Ok Jir.Declaration.Var
  | _ -> invalid_field scope "kind" "`const`, `let`, or `var`"

and parse_declaration = fun json ->
  let scope = "declaration" in
  let* kind = parse_declaration_kind json in
  let* name = string_field scope "name" json in
  let init =
    match Json.get_field "init" json with
    | None
    | Some Json.Null -> Ok None
    | Some init_json -> Result.map (fun value -> Some value) (parse_expr init_json)
  in
  let* init = init in
  Ok Jir.Declaration.{ kind; binder = binder_of_name name; init }

and parse_if_statement = fun json ->
  let scope = "if" in
  let* condition_json = field scope "condition" json in
  let* condition = parse_expr condition_json in
  let* then_json = array_field scope "then" json in
  let* then_ = map_results then_json parse_statement in
  let* else_json = array_field scope "else" json in
  let* else_ = map_results else_json parse_statement in
  Ok Jir.Statement.{ condition; then_; else_ }

and parse_statement = fun json ->
  let scope = "statement" in
  let* kind = string_field scope "kind" json in
  match kind with
  | "declaration" ->
      let* declaration_json = field scope "declaration" json in
      let* declaration = parse_declaration declaration_json in
      Ok (Jir.Statement.Declaration declaration)
  | "expression" ->
      let* expression_json = field scope "expression" json in
      let* expression = parse_expr expression_json in
      Ok (Jir.Statement.Expression expression)
  | "return" ->
      let* expression_json = field scope "expression" json in
      let* expression = parse_expr expression_json in
      Ok (Jir.Statement.Return expression)
  | "if" ->
      let* if_json = field scope "if" json in
      let* if_ = parse_if_statement if_json in
      Ok (Jir.Statement.If if_)
  | _ ->
      invalid_field scope "kind" "`declaration`, `expression`, `return`, or `if`"

let parse_export = fun json ->
  let scope = "export" in
  let* name = string_field scope "name" json in
  let* local = string_field scope "local" json in
  Ok Jir.Export.{ name; local = entity_of_name local }

let parse_program = fun json ->
  let* module_name = string_field "program" "module_name" json in
  let imports =
    match Json.get_field "imports" json with
    | None -> Ok []
    | Some value -> (
        match Json.get_array value with
        | Some imports -> map_results imports parse_import
        | None -> invalid_field "program" "imports" "an array"
      )
  in
  let* imports = imports in
  let* body = array_field "program" "body" json in
  let* body = map_results body parse_statement in
  let* exports = array_field "program" "exports" json in
  let* exports = map_results exports parse_export in
  Ok Jir.Program.{ module_name; imports; body; exports }

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* source = Result.map_error IO.error_message (Fs.read ctx.fixture_path) in
  let* json = Result.map_error Json.error_to_string (Json.from_string source) in
  let* program = parse_program json in
  Test.Snapshot.assert_json
    ~ctx:(with_snapshot_path (snapshot_path ~ctx) ctx.test)
    ~actual:(Jir.Program.to_json program)

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_json
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"raml:jir_fixture_tests" ~tests ~args)
    ~args:Env.args
    ()
