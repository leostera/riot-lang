open Std
open Std.Data

let fixtures_dir = Path.v "compiler/raml/tests/fixtures/typ_lowering"

let keep_source_fixture = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> `keep
  | _ -> `skip

let stable_fixture_filename = fun (ctx: Test.FixtureRunner.ctx) ->
  Path.join fixtures_dir ctx.fixture_relpath

let check_source_text = fun ~filename text ->
  let parse_result = Syn.parse ~filename text in
  match Syn.build_cst parse_result with
  | Ok cst ->
      let origin = Typ.Model.Source.Path filename in
      let implicit_opens = [] in
      let source = Typ.Model.Source.make_prepared
        ~source_id:(Typ.Model.SourceId.from_int 0)
        ~kind:Typ.Model.Source.File
        ~module_name:(Typ.Model.Source.infer_module_name origin)
        ~implicit_opens
        ~origin
        ~revision:0
        ~source_hash:(Typ.Model.Source.hash ~implicit_opens ~cst)
        ~parse_result
        ~cst in
      Typ.check ~config:Raml.TestingHelpers.Test_fixture_typing.typing_config ~source
  | Error (Syn.Parse_diagnostics diagnostics) ->
      panic
        (format
          Format.[
            str "expected CST for ";
            str (Path.to_string filename);
            str ": ";
            str (String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics));
          ])
  | Error (Syn.Cst_builder_error error) ->
      panic
        (format
          Format.[
            str "expected CST for ";
            str (Path.to_string filename);
            str ": ";
            str error.message;
          ])

let lowering_result_to_json = fun result ->
  match result with
  | Ok compilation_unit -> Json.obj
    [
      ("status", Json.string "ok");
      ("compilation_unit", Raml.CoreIR.Compilation_unit.to_json compilation_unit);
    ]
  | Error errors -> Json.obj
    [
      ("status", Json.string "error");
      ("errors", Json.array (List.map Raml.Typ_lowering.error_to_json errors));
    ]

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source = Fs.read ctx.fixture_path |> Result.expect ~msg:"fixture should exist" in
  let filename = stable_fixture_filename ctx in
  let report = check_source_text ~filename source in
  let semantic_tree = report.semantic_tree |> Option.expect ~msg:"expected semantic tree" in
  let source_unit = Raml.Source_unit.from_source ~relpath:filename ~source |> Result.expect ~msg:"fixture should produce a supported source unit" in
  let actual = Raml.Typ_lowering.lower_file ~source_unit semantic_tree |> lowering_result_to_json in
  Test.Snapshot.assert_json ~ctx:ctx.test ~actual

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_source_fixture
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"raml:typ_lowering_fixture_tests" ~tests ~args)
    ~args:Env.args
    ()
