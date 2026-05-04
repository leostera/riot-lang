open Std

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

let keep_example_fixture = keep_named_source_fixture
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

let compile_fixture = fun (ctx: Test.FixtureRunner.ctx) ->
  let source = read_source ctx in
  let filename = stable_fixture_filename ctx in
  let config = Raml.TestingHelpers.Test_fixture_typing.raml_config
    ~host:Raml.Target.aarch64_apple_darwin
    ~target:Raml.Target.js_unknown_ecma in
  Raml.TestingHelpers.compile_source ~config ~relpath:filename source |> Result.expect ~msg:"fixture should compile into a backend-oriented compilation snapshot"

let test_compilation_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let snapshot_path = snapshot_path ~snapshot_dir:snapshots_dir ~ctx ~suffix:".compilation.expected" in
  let actual = compile_fixture ctx |> Raml.Compilation.to_json in
  Test.Snapshot.assert_json ~ctx:(with_snapshot_path snapshot_path ctx.test) ~actual

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_example_fixture
          ~run:(fun ctx -> test_compilation_fixture ~ctx)
      in
      Test.Cli.main ~name:"raml:compilation_fixture_tests" ~tests ~args)
    ~args:Env.args
    ()
