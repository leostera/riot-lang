open Std
open Typ
open Typ.Analysis
open Typ.Diagnostics
open Typ.Model
open Typ.Session

let diagnostics_dir = Path.v "packages/typ/tests/diagnostics"

let diagnostic_marker_path = fun path ->
  match Path.extension path with
  | Some ext -> Path.add_extension path ~ext:(ext ^ ".diagnostic")
  | None -> Path.add_extension path ~ext:"diagnostic"

let filter_diagnostic_fixture = fun path ->
  match Path.extension path with
  | Some ".ml" | Some ".mli" ->
      let marker_path = diagnostic_marker_path path in
      let exists = Fs.exists marker_path |> Result.unwrap_or ~default:false in
      if exists then
        `keep
      else `skip
  | _ -> `skip

let diagnostics_to_json = fun (report: Check_result.t) ->
  Std.Data.Json.Object [
    "parse_diagnostics", Std.Data.Json.Array (List.map Syn.Diagnostic.to_json report.parse_diagnostics);
    "lowering_diagnostics", Std.Data.Json.Array (List.map Diagnostic.to_json report.lowering_diagnostics);
    "typing_diagnostics", Std.Data.Json.Array (List.map Diagnostic.to_json report.typing_diagnostics);
  ]

let stable_fixture_filename = fun (ctx: Test.FixtureRunner.ctx) -> Path.join diagnostics_dir ctx.fixture_relpath

let parse_failure_report = fun ~filename ->
  fun parse_result ->
    fun error ->
      let source_id = SourceId.of_int 0 in
      let (parse_diagnostics, lowering_diagnostics) =
        match error with
        | Syn.Parse_diagnostics diagnostics -> diagnostics, []
        | Syn.Cst_builder_error builder_error -> parse_result.Syn.Parser.diagnostics, [ Diagnostic.CstBuilderError { builder_error } ]
      in
      {
        Check_result.source_id;
        filename;
        parse_diagnostics;
        item_tree = None;
        body_arena = None;
        origin_map = None;
        semantic_tree = None;
        lowering_diagnostics;
        typing_diagnostics = [];
        file_summary = FileSummary.missing ~source_id ();
        type_index = TypeIndex.empty;
        exports = [];
        item_traces = [];
        expr_traces = []
      }

let check_source_text = fun ~filename text ->
  let parse_result = Syn.parse ~filename text in
  match Syn.build_cst parse_result with
  | Ok cst ->
      let origin = Source.Path filename in
      let implicit_opens = [] in
      let source = Source.make_prepared ~source_id:(SourceId.of_int 0) ~kind:Source.File ~module_name:(Source.infer_module_name origin) ~implicit_opens ~origin ~revision:0 ~source_hash:(Source.hash ~implicit_opens ~cst) ~parse_result ~cst in Typ.check ~config:Config.default ~source
  | Error error -> parse_failure_report ~filename parse_result error

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source = Fs.read ctx.fixture_path |> Result.expect ~msg:"fixture should exist" in
  let report = check_source_text ~filename:(stable_fixture_filename ctx) source in Test.Snapshot.assert_json ~ctx:ctx.test ~actual:(diagnostics_to_json report)

let main ~args =
  let tests = Test.FixtureRunner.cases () ~dir:diagnostics_dir ~filter:filter_diagnostic_fixture ~run:(
    fun ctx -> test_fixture ~ctx
  ) in Test.Cli.main ~name:"typ:diagnostics" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
