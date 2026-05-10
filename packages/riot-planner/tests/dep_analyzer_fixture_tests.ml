open Std
open Std.Data

module Test = Std.Test
module Dep_analyzer = Riot_planner.Dep_analyzer

let fixture_root = Path.v "packages/riot-planner/tests/deps_fixtures"

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create dep analyzer fixture source slice"

let has_expected = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> Test.FixtureRunner.Keep
  | _ -> Test.FixtureRunner.Skip

let sorted = fun values ->
  List.unique
    (List.sort values ~compare:String.compare)
    ~compare:String.compare

let modules_json = fun modules ->
  Json.Object [
    ("modules", Json.Array (List.map (sorted modules) ~fn:(fun name -> Json.String name)));
  ]

let diagnostics_to_string = fun diagnostics ->
  "parse diagnostics:\n"
  ^ String.concat "\n" (List.map diagnostics ~fn:Syn.Diagnostic.to_string)
  ^ "\n"

let render_actual = fun ~fixture_path ->
  let source =
    Fs.read fixture_path
    |> Result.expect ~msg:"failed to read dep analyzer fixture"
  in
  let parse_result = Syn.parse ~filename:fixture_path (source_slice source) in
  match Dep_analyzer.analyze
    ~source:fixture_path
    ~source_hash:(Crypto.hash_string source)
    parse_result with
  | Ok summary -> (
      match Dep_analyzer.resolve Dep_analyzer.Env.empty [ summary ] with
      | Ok [ resolved ] ->
          Dep_analyzer.ResolvedSource.modules resolved
          @ Dep_analyzer.ResolvedSource.unresolved resolved
          |> modules_json
          |> Json.to_string_pretty
          |> fun text -> text ^ "\n"
      | Ok _ -> "expected one resolved source\n"
      | Error _ -> "dependency resolution failed\n"
    )
  | Error (Dep_analyzer.Parse_diagnostics diagnostics) -> diagnostics_to_string diagnostics

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun value -> value)
    ~actual:(render_actual ~fixture_path:ctx.fixture_path)

let run_fixture = fun ctx -> test_fixture ~ctx

let main ~args =
  let tests = Test.FixtureRunner.cases () ~dir:fixture_root ~filter:has_expected ~run:run_fixture in
  Test.Cli.main ~name:"riot-planner-dep-analyzer-fixtures" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
