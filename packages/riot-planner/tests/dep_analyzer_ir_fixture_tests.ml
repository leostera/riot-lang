open Std
open Std.Data

module Test = Std.Test
module Dep_analyzer = Riot_planner.Dep_analyzer

let fixture_root = Path.v "packages/riot-planner/tests/deps_fixtures"

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create dep analyzer IR fixture source slice"

let keep_source_fixture = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> Test.FixtureRunner.Keep
  | _ -> Test.FixtureRunner.Skip

let ir_snapshot_path = fun fixture_path ->
  Path.to_string (Path.remove_extension fixture_path) ^ ".ir.expected"
  |> Path.from_string
  |> Result.expect ~msg:"IR fixture snapshot path should stay valid UTF-8"

let diagnostics_to_string = fun diagnostics ->
  "parse diagnostics:\n"
  ^ String.concat "\n" (List.map diagnostics ~fn:Syn.Diagnostic.to_string)
  ^ "\n"

let pretty_json = fun text ->
  match Json.from_string text with
  | Ok json -> Json.to_string_pretty json ^ "\n"
  | Error err -> text ^ "\n/* invalid JSON: " ^ Json.error_to_string err ^ " */\n"

let render_actual = fun ~fixture_path ~fixture_relpath ->
  let source =
    Fs.read fixture_path
    |> Result.expect ~msg:"failed to read dep analyzer IR fixture"
  in
  let parse_result = Syn.parse ~filename:fixture_path (source_slice source) in
  match Dep_analyzer.analyze
    ~source:fixture_relpath
    ~source_hash:(Crypto.hash_string source)
    parse_result with
  | Ok summary -> (
      match Serde_json.to_string Dep_analyzer.source_summary_serializer summary with
      | Ok json -> pretty_json json
      | Error err -> "IR serialization failed: " ^ Serde.Error.to_string err ^ "\n"
    )
  | Error (Dep_analyzer.Parse_diagnostics diagnostics) -> diagnostics_to_string diagnostics

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun value -> value)
    ~actual:(render_actual ~fixture_path:ctx.fixture_path ~fixture_relpath:ctx.fixture_relpath)

let main ~args =
  let tests =
    Test.FixtureRunner.cases
      ()
      ~dir:fixture_root
      ~filter:keep_source_fixture
      ~snapshot_path:(fun path -> Some (ir_snapshot_path path))
      ~run:(fun ctx -> test_fixture ~ctx)
  in
  Test.Cli.main ~name:"riot-planner-dep-analyzer-ir-fixtures" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
