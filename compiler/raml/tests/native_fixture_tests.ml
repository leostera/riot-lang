open Std
open Std.Data

let fixtures_dir = Path.v "compiler/raml/tests/fixtures/corpus"

let snapshots_dir = Path.v "compiler/raml/tests/fixtures/native"

let logical_corpus_dir = Path.v "corpus"

let append_snapshot_suffix = fun path suffix ->
  format Format.[ str (Path.to_string (Path.remove_extension path)); str suffix ]
  |> Path.of_string
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
  String.concat "/" logical_parts |> Path.of_string |> Result.expect ~msg:"logical fixture path should stay valid UTF-8"

let keep_named_source_fixture = fun ~names path ->
  match Path.extension path with
  | Some ".ml" ->
      if List.exists (String.equal (strip_ordering_prefix (fixture_stem path))) names then
        `keep
      else
        `skip
  | _ -> `skip

let keep_native_fixture = keep_named_source_fixture ~names:[ "hello_world" ]

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

let json_field_or_null = fun name json ->
  json_field name json |> Option.unwrap_or ~default:Json.null

let compile_fixture = fun (ctx: Test.FixtureRunner.ctx) ->
  let source = read_source ctx in
  let filename = stable_fixture_filename ctx in
  Raml.Example_pipeline.compile_source
    ~host:Raml.Target.aarch64_apple_darwin
    ~target:Raml.Target.aarch64_apple_darwin
    ~relpath:filename
    ~source
  |> Result.expect ~msg:"fixture should compile into a native pipeline snapshot"

let native_pipeline_json = fun ctx -> compile_fixture ctx |> Raml.Example_pipeline.to_json

let lowered_stage = fun ~name pipeline ->
  pipeline |> json_field_or_null "lowered" |> json_field_or_null name

let codegen_stage = fun ~name pipeline ->
  pipeline |> json_field_or_null "codegen" |> json_field_or_null name

let render_stage_text = fun stage ->
  match (json_field_string "status" stage, json_field_string "output" stage) with
  | (Some "ok", Some output) -> output
  | _ -> Json.to_string_pretty stage

let render_link_text = fun native_stage ->
  match json_field_string "status" native_stage with
  | Some "ok" -> (
      match
        Raml.Native.Linker.plan
          ~host:Raml.Target.aarch64_apple_darwin
          ~target:Raml.Target.aarch64_apple_darwin
          ~artifact:Raml.Native.Linker.Executable
          ~input:(Path.v "build/fixture.s")
          ~output:(Path.v "build/fixture")
      with
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

let test_nir_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let actual = native_pipeline_json ctx |> lowered_stage ~name:"nir" in
  let path = snapshot_path ~ctx ~suffix:".nir.expected" in
  Test.Snapshot.assert_json ~ctx:(with_snapshot_path path ctx.test) ~actual

let test_mir_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let actual = native_pipeline_json ctx |> lowered_stage ~name:"mir" in
  let path = snapshot_path ~ctx ~suffix:".mir.expected" in
  Test.Snapshot.assert_json ~ctx:(with_snapshot_path path ctx.test) ~actual

let test_lir_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let actual = native_pipeline_json ctx |> lowered_stage ~name:"lir" in
  let path = snapshot_path ~ctx ~suffix:".lir.expected" in
  Test.Snapshot.assert_json ~ctx:(with_snapshot_path path ctx.test) ~actual

let test_native_emitter_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let native_stage = native_pipeline_json ctx |> codegen_stage ~name:"native" in
  let path = snapshot_path ~ctx ~suffix:".native.expected" in
  let actual = render_stage_text native_stage in
  Test.Snapshot.assert_text ~ctx:(with_snapshot_path path ctx.test) ~actual

let test_native_linker_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let native_stage = native_pipeline_json ctx |> codegen_stage ~name:"native" in
  let path = snapshot_path ~ctx ~suffix:".link.expected" in
  let actual = render_link_text native_stage in
  Test.Snapshot.assert_text ~ctx:(with_snapshot_path path ctx.test) ~actual

let () =
  Actors.run
    ~main:(fun ~args ->
      let nir_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_native_fixture
          ~run:(fun ctx -> test_nir_fixture ~ctx)
      in
      let mir_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_native_fixture
          ~run:(fun ctx -> test_mir_fixture ~ctx)
      in
      let lir_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_native_fixture
          ~run:(fun ctx -> test_lir_fixture ~ctx)
      in
      let emitter_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_native_fixture
          ~run:(fun ctx -> test_native_emitter_fixture ~ctx)
      in
      let linker_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_native_fixture
          ~run:(fun ctx -> test_native_linker_fixture ~ctx)
      in
      Test.Cli.main
        ~name:"raml:native_fixture_tests"
        ~tests:(nir_tests @ mir_tests @ lir_tests @ emitter_tests @ linker_tests)
        ~args)
    ~args:Env.args
    ()
