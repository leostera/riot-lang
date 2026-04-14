open Std

let ( let* ) = fun result fn -> Result.and_then result ~fn

let name = "typ:checker"

let fixtures_dir = Path.v "packages/typ/tests/fixtures/corpus"

let fixture_filter = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> `keep
  | _ -> `skip

let checker_test = fun (ctx: Test.FixtureRunner.ctx) ->
  let* file = Fs.read ctx.fixture_path |> Result.map_err ~fn:IO.error_message in
  let parse_result = Syn.parse ~filename:ctx.fixture_path file in
  let* cst =
    Syn.build_cst parse_result |> Result.map_err ~fn:
      (
        function
        | Syn.Parse_diagnostics diagnostics -> diagnostics
        |> List.map ~fn:Syn.Diagnostic.to_string
        |> String.concat "\n"
        | Syn.Cst_builder_error { message } -> message
      )
  in
  let source = Typ.Model.Source.make ~text:file in
  let typings = Typ.Check.check ~source cst in
  let* json_text = Serde_json.to_string Typ.Check.Typings.serializer typings
  |> Result.map_err ~fn:Serde.Error.to_string in
  let* json = Data.Json.of_string json_text |> Result.map_err ~fn:Data.Json.error_to_string in
  Test.Snapshot.assert_json ~ctx:ctx.test ~actual:json

let tests =
  Test.FixtureRunner.cases
    ()
    ~dir:fixtures_dir
    ~filter:fixture_filter
    ~snapshot_path:(fun path -> Some (Path.add_extension path ~ext:"checker.expected"))
    ~run:checker_test

let main = fun ~args -> Test.Cli.main ~name ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
