open Std
open Std.Data
open Syn

let fixture_root = Path.v "packages/syn/tests/deps_fixtures"

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create deps fixture source slice"

let has_expected = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> Test.FixtureRunner.Keep
  | _ -> Test.FixtureRunner.Skip

let render_actual = fun ~fixture_path ->
  let source =
    Fs.read fixture_path
    |> Result.expect ~msg:"failed to read deps fixture"
  in
  let parse_result = Syn.parse ~filename:fixture_path (source_slice source) in
  match Syn.Deps.from_parse_result parse_result with
  | Ok deps -> Json.to_string_pretty (Syn.Deps.to_json deps) ^ "\n"
  | Error (Syn.Deps.Parse_diagnostics diagnostics) ->
      "parse diagnostics:\n"
      ^ String.concat "\n" (List.map diagnostics ~fn:Diagnostic.to_string)
      ^ "\n"

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun value -> value)
    ~actual:(render_actual ~fixture_path:ctx.fixture_path)

let run_fixture = fun ctx -> test_fixture ~ctx

let main ~args =
  let tests = Test.FixtureRunner.cases () ~dir:fixture_root ~filter:has_expected ~run:run_fixture in
  Test.Cli.main ~name:"syn-deps-fixtures" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
