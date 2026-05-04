open Std
open Std.Data

let ( let* ) = Result.and_then

let fixtures_dir = Path.v "compiler/raml/tests/fixtures/core_ir"

let keep_json = fun path ->
  match Path.extension path with
  | Some ".json" -> `keep
  | _ -> `skip

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* source = Result.map_error IO.error_message (Fs.read ctx.fixture_path) in
  let* json = Result.map_error Json.error_to_string (Json.from_string source) in
  let* compilation_unit = Raml.TestingHelpers.Core_ir_fixture_support.parse_compilation_unit json in
  Test.Snapshot.assert_json
    ~ctx:ctx.test
    ~actual:(Raml.CoreIR.Compilation_unit.to_json compilation_unit)

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
      Test.Cli.main ~name:"raml:core_ir_fixture_tests" ~tests ~args)
    ~args:Env.args
    ()
