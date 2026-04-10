open Std
open Std.Data

let ( let* ) = Result.and_then

let fixtures_dir = Path.v "compiler/raml/tests/fixtures/core_ir"

let snapshots_dir = Path.v "compiler/raml/tests/fixtures/native"

let append_snapshot_suffix = fun path suffix ->
  format Format.[ str (Path.to_string (Path.remove_extension path)); str suffix ]
  |> Path.of_string
  |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let snapshot_path = fun ~(ctx:Test.FixtureRunner.ctx) ~suffix ->
  Path.join snapshots_dir ctx.fixture_relpath |> fun path -> append_snapshot_suffix path suffix

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

let compile_fixture = fun (ctx: Test.FixtureRunner.ctx) ->
  let* source = Result.map_error IO.error_message (Fs.read ctx.fixture_path) in
  let* json = Result.map_error Json.error_to_string (Json.of_string source) in
  let* compilation_unit = Raml.Core_ir_fixture_support.parse_compilation_unit json in
  let* nir =
    Result.map_error
      (fun errors ->
        errors |> List.map Raml.Native.Nir.Lowering.error_to_json |> Json.array |> Json.to_string)
      (Raml.Native.Nir.Lowering.lower_compilation_unit compilation_unit)
  in
  let mir = Raml.Native.Mir.Lowering.lower_program nir in
  let lir = Raml.Native.Lir.Lowering.lower_program mir in
  let* emitted =
    Result.map_error
      (fun error -> Raml.Native.Emitter.error_to_json error |> Json.to_string)
      (Raml.Native.Emitter.emit_program
        ~host:Raml.Target.aarch64_apple_darwin
        ~target:Raml.Target.aarch64_apple_darwin
        lir)
  in
  let* link_plan =
    Result.map_error
      (fun error -> Raml.Native.Linker.error_to_json error |> Json.to_string)
      (Raml.Native.Linker.plan
        ~host:Raml.Target.aarch64_apple_darwin
        ~target:Raml.Target.aarch64_apple_darwin
        ~artifact:Raml.Native.Linker.Executable
        ~input:(Path.v "build/fixture.s")
        ~output:(Path.v "build/fixture"))
  in
  Ok (nir, mir, lir, emitted, Raml.Native.Linker.plan_to_string link_plan)

let test_nir_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* (nir, _, _, _, _) = compile_fixture ctx in
  let snapshot_path = snapshot_path ~ctx ~suffix:".nir.expected" in
  Test.Snapshot.assert_json
    ~ctx:(with_snapshot_path snapshot_path ctx.test)
    ~actual:(Raml.Native.Nir.Program.to_json nir)

let test_mir_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* (_, mir, _, _, _) = compile_fixture ctx in
  let snapshot_path = snapshot_path ~ctx ~suffix:".mir.expected" in
  Test.Snapshot.assert_json
    ~ctx:(with_snapshot_path snapshot_path ctx.test)
    ~actual:(Raml.Native.Mir.Program.to_json mir)

let test_lir_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* (_, _, lir, _, _) = compile_fixture ctx in
  let snapshot_path = snapshot_path ~ctx ~suffix:".lir.expected" in
  Test.Snapshot.assert_json
    ~ctx:(with_snapshot_path snapshot_path ctx.test)
    ~actual:(Raml.Native.Lir.Program.to_json lir)

let test_native_emitter_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* (_, _, _, emitted, _) = compile_fixture ctx in
  let snapshot_path = snapshot_path ~ctx ~suffix:".native.expected" in
  Test.Snapshot.assert_text ~ctx:(with_snapshot_path snapshot_path ctx.test) ~actual:emitted

let test_native_linker_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* (_, _, _, _, link_plan) = compile_fixture ctx in
  let snapshot_path = snapshot_path ~ctx ~suffix:".link.expected" in
  Test.Snapshot.assert_text ~ctx:(with_snapshot_path snapshot_path ctx.test) ~actual:link_plan

let () =
  Actors.run
    ~main:(fun ~args ->
      let nir_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_json
          ~run:(fun ctx -> test_nir_fixture ~ctx)
      in
      let mir_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_json
          ~run:(fun ctx -> test_mir_fixture ~ctx)
      in
      let lir_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_json
          ~run:(fun ctx -> test_lir_fixture ~ctx)
      in
      let emitter_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_json
          ~run:(fun ctx -> test_native_emitter_fixture ~ctx)
      in
      let linker_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_json
          ~run:(fun ctx -> test_native_linker_fixture ~ctx)
      in
      Test.Cli.main
        ~name:"raml:native_fixture_tests"
        ~tests:(nir_tests @ mir_tests @ lir_tests @ emitter_tests @ linker_tests)
        ~args)
    ~args:Env.args
    ()
