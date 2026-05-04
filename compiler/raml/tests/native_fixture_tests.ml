open Std
open Std.Data

let fixtures_dir = Path.v "compiler/raml/tests/fixtures/corpus"

let snapshots_dir = Path.v "compiler/raml/tests/fixtures/native"

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

let keep_native_fixture = keep_named_source_fixture
  ~names:[
    "hello_world";
    "exported_constants";
    "integer_arithmetic";
    "top_level_function_direct_call";
    "float_arithmetic";
    "boolean_logic";
    "if_then_else";
    "let_shadowing";
    "tuples_and_patterns";
    "records_and_updates";
    "variants_and_match";
    "option_pipeline";
    "list_recursion_sum";
    "tail_recursive_factorial";
    "mutual_recursion_even_odd";
    "local_functions_and_closures";
    "custom_infix_operators";
    "tail_conditional_direct_call";
    "grouped_initialization_order";
    "local_function_capture";
    "sequence_before_conditional";
    "indirect_call_via_returned_closure";
    "partial_application";
    "sequence_and_ignore";
    "function_composition_pipeline";
    "phantom_length_vector";
    "prelude_option_match";
    "prelude_result_match";
    "open_std_hello_world";
    "less_than_comparison";
    "greater_than_comparison";
    "less_or_equal_comparison";
    "greater_or_equal_comparison";
    "effect_position_local_let";
    "initializer_shadowing";
    "top_level_mutual_recursion";
    "external_print_endline";
    "dead_local_bindings";
    "printf_and_print_endline";
    "string_concat";
    "string_of_int";
    "print_string";
    "string_of_float";
    "float_of_string";
    "print_newline";
    "int_of_string";
    "module_identity";
    "print_int";
    "print_char"
  ]

let stable_fixture_filename = fun (ctx: Test.FixtureRunner.ctx) ->
  Path.join logical_corpus_dir (logical_fixture_relpath ctx.fixture_relpath)

let read_source = fun (ctx: Test.FixtureRunner.ctx) ->
  Fs.read ctx.fixture_path |> Result.expect ~msg:"fixture should exist"

let snapshot_path = fun ~(ctx:Test.FixtureRunner.ctx) ~suffix ->
  Path.join snapshots_dir ctx.fixture_relpath |> fun path -> append_snapshot_suffix path suffix

let with_snapshot_path = fun path (ctx: Test.ctx) ->
  let fixture =
    match ctx.fixture with
    | Some fixture -> { fixture with snapshot_path = Some path }
    | None -> panic "expected fixture-backed test context"
  in
  Test.Context.with_fixture ctx fixture

let json_field = Json.get_field

let json_field_string = fun name json ->
  match json_field name json with
  | Some value -> Json.get_string value
  | None -> None

let json_field_or_null = fun name json -> json_field name json |> Option.unwrap_or ~default:Json.null

let compile_fixture = fun (ctx: Test.FixtureRunner.ctx) ->
  let source = read_source ctx in
  let filename = stable_fixture_filename ctx in
  let config = Raml.TestingHelpers.Test_fixture_typing.raml_config
    ~host:Raml.Target.aarch64_apple_darwin
    ~target:Raml.Target.aarch64_apple_darwin in
  Raml.TestingHelpers.Example_pipeline.compile_source ~config ~relpath:filename ~source
  |> Result.expect ~msg:"fixture should compile into a native pipeline snapshot"

let native_pipeline_json = fun ctx -> compile_fixture ctx |> Raml.TestingHelpers.Example_pipeline.to_json

let lowered_stage = fun ~name pipeline ->
  pipeline |> json_field_or_null "lowered" |> json_field_or_null name

let lowered_stage_pass = fun ~stage ~pass pipeline ->
  lowered_stage ~name:stage pipeline |> json_field_or_null "passes" |> json_field_or_null pass

let codegen_stage = fun ~name pipeline ->
  pipeline |> json_field_or_null "codegen" |> json_field_or_null name

let render_stage_text = fun stage ->
  match (json_field_string "status" stage, json_field_string "output" stage) with
  | (Some "ok", Some output) -> output
  | _ -> Json.to_string_pretty stage

let render_link_text = fun native_stage ->
  match json_field_string "status" native_stage with
  | Some "ok" -> (
      match Raml.Native.Linker.plan
        ~host:Raml.Target.aarch64_apple_darwin
        ~target:Raml.Target.aarch64_apple_darwin
        ~artifact:Raml.Native.Linker.Executable
        ~input:(Path.v "build/fixture.s")
        ~output:(Path.v "build/fixture") with
      | Ok plan -> Raml.Native.Linker.plan_to_string plan
      | Error error -> Json.to_string_pretty (Raml.Native.Linker.error_to_json error)
    )
  | _ -> Json.to_string_pretty
    (Json.obj
      [
        ("status", Json.string "blocked");
        ("blocked_on", Json.string "native_codegen");
        ("native_codegen", native_stage);
      ])

type json_snapshot = {
  suffix: string;
  select: Json.t -> Json.t;
}

type text_snapshot = {
  suffix: string;
  render: Json.t -> string;
}

let make_stage_snapshot = fun ~stage ~suffix -> { suffix; select = lowered_stage ~name:stage }

let make_pass_snapshot = fun ~stage ~pass ->
  {
    suffix = format Format.[ str "."; str stage; str "."; str pass; str ".expected" ];
    select = lowered_stage_pass ~stage ~pass
  }

let json_snapshots = [
  make_stage_snapshot ~stage:"nir" ~suffix:".nir.expected";
  make_pass_snapshot ~stage:"nir" ~pass:"normalize";
  make_pass_snapshot ~stage:"nir" ~pass:"simplify";
  make_stage_snapshot ~stage:"mir" ~suffix:".mir.expected";
  make_pass_snapshot ~stage:"mir" ~pass:"canonicalize";
  make_pass_snapshot ~stage:"mir" ~pass:"insert_polls";
  make_stage_snapshot ~stage:"lir" ~suffix:".lir.expected";
  make_pass_snapshot ~stage:"lir" ~pass:"simplify";
  make_pass_snapshot ~stage:"lir" ~pass:"dead_code";
  make_pass_snapshot ~stage:"lir" ~pass:"schedule";
  make_pass_snapshot ~stage:"lir" ~pass:"layout_frames";
  make_pass_snapshot ~stage:"lir" ~pass:"allocate_homes";
  make_pass_snapshot ~stage:"lir" ~pass:"assign_homes";
  make_pass_snapshot ~stage:"lir" ~pass:"legalize";
  make_pass_snapshot ~stage:"lir" ~pass:"calling_convention";
]

let text_snapshots = [
  {
    suffix = ".native.expected";
    render = (fun pipeline -> pipeline |> codegen_stage ~name:"native" |> render_stage_text)
  };
  {
    suffix = ".link.expected";
    render = (fun pipeline -> pipeline |> codegen_stage ~name:"native" |> render_link_text)
  };
]

let test_json_snapshot_fixture = fun snapshot ~(ctx:Test.FixtureRunner.ctx) ->
  let actual = native_pipeline_json ctx |> snapshot.select in
  let path = snapshot_path ~ctx ~suffix:snapshot.suffix in
  Test.Snapshot.assert_json ~ctx:(with_snapshot_path path ctx.test) ~actual

let test_text_snapshot_fixture = fun snapshot ~(ctx:Test.FixtureRunner.ctx) ->
  let actual = native_pipeline_json ctx |> snapshot.render in
  let path = snapshot_path ~ctx ~suffix:snapshot.suffix in
  Test.Snapshot.assert_text ~ctx:(with_snapshot_path path ctx.test) ~actual

let fixture_cases = fun run ->
  Test.FixtureRunner.cases () ~dir:fixtures_dir ~filter:keep_native_fixture ~run

let json_snapshot_cases = fun snapshot ->
  fixture_cases (fun ctx -> test_json_snapshot_fixture snapshot ~ctx)

let text_snapshot_cases = fun snapshot ->
  fixture_cases (fun ctx -> test_text_snapshot_fixture snapshot ~ctx)

let () =
  Actors.run
    ~main:(fun ~args ->
      let json_tests = List.map json_snapshot_cases json_snapshots |> List.flatten in
      let text_tests = List.map text_snapshot_cases text_snapshots |> List.flatten in
      Test.Cli.main ~name:"raml:native_fixture_tests" ~tests:(json_tests @ text_tests) ~args)
    ~args:Env.args
    ()
