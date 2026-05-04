open Std
open Std.Data
module Jir = Raml.Js.Jir
module Jir_lowering = Raml.Js.Jir.Lowering

let ( let* ) = Result.and_then

let fixtures_dir = Path.v "compiler/raml/tests/fixtures/jir_lowering"

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

let lowering_result_to_json = fun result ->
  match result with
  | Ok program -> Json.obj
    [ ("status", Json.string "ok"); ("program", Jir.Program.to_json program); ]
  | Error errors -> Json.obj
    [
      ("status", Json.string "error");
      ("errors", Json.array (List.map Jir_lowering.error_to_json errors));
    ]

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* source = Result.map_error IO.error_message (Fs.read ctx.fixture_path) in
  let* json = Result.map_error Json.error_to_string (Json.from_string source) in
  let* compilation_unit = Raml.TestingHelpers.Core_ir_fixture_support.parse_compilation_unit json in
  let actual = Jir_lowering.lower_compilation_unit compilation_unit |> lowering_result_to_json in
  Test.Snapshot.assert_json ~ctx:(with_snapshot_path (snapshot_path ~ctx) ctx.test) ~actual

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
      Test.Cli.main ~name:"raml:jir_lowering_fixture_tests" ~tests ~args)
    ~args:Env.args
    ()
