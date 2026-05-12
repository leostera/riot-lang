open Std
open Std.Data
module Jir = Raml.Js.Jir
module Jir_lowering = Raml.Js.Jir.Lowering
module Jst = Raml.Js.Jst

let fixtures_dir = Path.v "compiler/raml/tests/fixtures/corpus"

let snapshots_dir = Path.v "compiler/raml/tests/fixtures/js"

let logical_corpus_dir = Path.v "corpus"

let append_snapshot_suffix = fun path suffix ->
  format Format.[ str (Path.to_string (Path.remove_extension path)); str suffix ]
  |> Path.from_string
  |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let fixture_stem = fun path -> path |> Path.remove_extension |> Path.basename

let is_ascii_digit = fun char -> char >= '0' && char <= '9'

let strip_ordering_prefix = fun name ->
  let len = String.length name in
  let start =
    if len > 0 && (name.[0] = 'm' || name.[0] = 'M') then
      1
    else
      0
  in
  let rec consume_digits index =
    if index < len && is_ascii_digit name.[index] then
      consume_digits (index + 1)
    else
      index
  in
  let prefix_end = consume_digits start in
  if prefix_end > start && prefix_end < len && name.[prefix_end] = '_' then
    String.sub name (prefix_end + 1) (len - prefix_end - 1)
  else
    name

let logical_fixture_relpath = fun path ->
  let parts = String.split_on_char '/' (Path.to_string path) in
  let logical_parts =
    match List.rev parts with
    | [] -> []
    | basename :: rest -> List.rev (strip_ordering_prefix basename :: rest)
  in
  String.concat "/" logical_parts |> Path.from_string |> Result.expect ~msg:"logical fixture path should stay valid UTF-8"

let keep_named_source_fixture = fun ~names path ->
  match Path.extension path with
  | Some ".ml" ->
      if List.exists (String.equal (strip_ordering_prefix (fixture_stem path))) names then
        `keep
      else
        `skip
  | _ -> `skip

let keep_js_fixture = keep_named_source_fixture
  ~names:[
    "hello_world";
    "exported_constants";
    "integer_arithmetic";
    "top_level_function_direct_call";
    "float_arithmetic";
    "boolean_logic";
    "if_then_else";
    "option_pipeline";
    "tail_conditional_direct_call";
    "grouped_initialization_order";
    "tail_recursive_factorial";
    "mutual_recursion_even_odd";
    "let_shadowing";
    "local_function_capture";
    "tuples_and_patterns";
    "records_and_updates";
    "variants_and_match";
    "list_recursion_sum";
    "sequence_before_conditional";
    "local_functions_and_closures";
    "indirect_call_via_returned_closure";
    "partial_application";
    "sequence_and_ignore";
    "function_composition_pipeline";
    "phantom_length_vector";
    "prelude_option_match";
    "open_std_hello_world";
    "less_than_comparison";
    "greater_than_comparison";
    "less_or_equal_comparison";
    "greater_or_equal_comparison";
    "effect_position_local_let";
    "initializer_shadowing";
    "top_level_mutual_recursion";
    "external_print_endline";
    "prelude_result_match";
    "dead_local_bindings";
    "printf_and_print_endline";
    "string_concat";
    "string_of_int";
    "string_of_float";
    "float_of_string";
    "print_string";
    "print_newline";
    "int_of_string";
    "module_identity";
    "print_int";
    "print_char";
  ]

let stable_fixture_filename = fun (ctx: Test.FixtureRunner.ctx) ->
  Path.join logical_corpus_dir (logical_fixture_relpath ctx.fixture_relpath)

let read_source = fun (ctx: Test.FixtureRunner.ctx) ->
  Fs.read ctx.fixture_path |> Result.expect ~msg:"fixture should exist"

let snapshot_path = fun ~snapshot_dir ~(ctx:Test.FixtureRunner.ctx) ~suffix ->
  Path.join snapshot_dir ctx.fixture_relpath |> fun path -> append_snapshot_suffix path suffix

let with_snapshot_path = fun path (ctx: Test.ctx) ->
  let fixture =
    match ctx.fixture with
    | Some fixture -> { fixture with snapshot_path = Some path }
    | None -> panic "expected fixture-backed test context"
  in
  Test.Context.with_fixture ctx fixture

let check_source_text = fun ~filename text ->
  let parse_result = Syn.parse ~filename text in
  match Syn.build_cst parse_result with
  | Ok cst ->
      let origin = Typ.Model.Source.Path filename in
      let implicit_opens = [] in
      let source = Typ.Model.Source.make_prepared
        ~source_id:(Typ.Model.SourceId.from_int 0)
        ~kind:Typ.Model.Source.File
        ~module_name:(Typ.Model.Source.infer_module_name origin)
        ~implicit_opens
        ~origin
        ~revision:0
        ~source_hash:(Typ.Model.Source.hash ~implicit_opens ~cst)
        ~parse_result
        ~cst in
      Typ.check ~config:Raml.TestingHelpers.Test_fixture_typing.typing_config ~source
  | Error (Syn.Parse_diagnostics diagnostics) ->
      panic
        (format
          Format.[
            str "expected CST for ";
            str (Path.to_string filename);
            str ": ";
            str (String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics));
          ])
  | Error (Syn.Cst_builder_error error) ->
      panic
        (format
          Format.[
            str "expected CST for ";
            str (Path.to_string filename);
            str ": ";
            str error.message;
          ])

let render_json = Json.to_string_pretty

let render_typ_lowering_errors = fun errors ->
  Json.array (List.map Raml.Typ_lowering.error_to_json errors) |> render_json

let render_jir_lowering_errors = fun errors ->
  Json.array (List.map Jir_lowering.error_to_json errors) |> render_json

let js_snapshot_to_json = fun ~(jst:Jst.Program.t) ~(output:string) ->
  Json.obj [ ("jst", Jst.Program.to_json jst); ("js", Json.string output) ]

let compile_core_ir = fun (ctx: Test.FixtureRunner.ctx) ->
  let source = read_source ctx in
  let filename = stable_fixture_filename ctx in
  let report = check_source_text ~filename source in
  let semantic_tree = report.semantic_tree |> Option.expect ~msg:"expected semantic tree" in
  let source_unit = Raml.Source_unit.from_source ~relpath:filename ~source |> Result.expect ~msg:"fixture should produce a supported source unit" in
  match Raml.Typ_lowering.lower_file ~source_unit semantic_tree with
  | Ok compilation_unit -> compilation_unit
  | Error errors -> panic
    (format
      Format.[
        str "expected Typ -> Core IR lowering to succeed:\n";
        str (render_typ_lowering_errors errors);
      ])

let lower_jir = fun compilation_unit ->
  match Jir_lowering.lower_compilation_unit compilation_unit with
  | Ok program -> program
  | Error errors -> panic
    (format
      Format.[
        str "expected Core IR -> JIR lowering to succeed:\n";
        str (render_jir_lowering_errors errors);
      ])

let test_core_ir_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let compilation_unit = compile_core_ir ctx in
  let snapshot_path = snapshot_path ~snapshot_dir:snapshots_dir ~ctx ~suffix:".core_ir.expected" in
  Test.Snapshot.assert_json
    ~ctx:(with_snapshot_path snapshot_path ctx.test)
    ~actual:(Raml.CoreIR.Compilation_unit.to_json compilation_unit)

let test_jir_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let compilation_unit = compile_core_ir ctx in
  let program = lower_jir compilation_unit in
  let snapshot_path = snapshot_path ~snapshot_dir:snapshots_dir ~ctx ~suffix:".jir.expected" in
  Test.Snapshot.assert_json
    ~ctx:(with_snapshot_path snapshot_path ctx.test)
    ~actual:(Jir.Program.to_json program)

let test_js_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let compilation_unit = compile_core_ir ctx in
  let program = lower_jir compilation_unit in
  let snapshot_path = snapshot_path ~snapshot_dir:snapshots_dir ~ctx ~suffix:".js.expected" in
  let jst = Jst.Lowering.lower_program program in
  let output = Jst.Emitter.emit_program jst in
  let actual = js_snapshot_to_json ~jst ~output in
  Test.Snapshot.assert_json ~ctx:(with_snapshot_path snapshot_path ctx.test) ~actual

let () =
  Actors.run
    ~main:(fun ~args ->
      let core_ir_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_js_fixture
          ~run:(fun ctx -> test_core_ir_fixture ~ctx)
      in
      let jir_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_js_fixture
          ~run:(fun ctx -> test_jir_fixture ~ctx)
      in
      let js_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_js_fixture
          ~run:(fun ctx -> test_js_fixture ~ctx)
      in
      Test.Cli.main ~name:"raml:js_fixture_tests" ~tests:(core_ir_tests @ jir_tests @ js_tests) ~args)
    ~args:Env.args
    ()
