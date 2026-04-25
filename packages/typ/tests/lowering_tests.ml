open Std

let ( let* ) = fun result fn -> Result.and_then result ~fn

let name = "typ:lowering"

let fixtures_dir = Path.v "packages/typ/tests/fixtures/corpus"

let fixture_filter = fun path ->
  match Path.extension path with
  | Some ".ml" | Some ".mli" -> `keep
  | _ -> `skip

let source_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create lowering test source slice"

let lowering_test = fun (ctx: Test.FixtureRunner.ctx) ->
  let* file = Fs.read ctx.fixture_path |> Result.map_err ~fn:IO.error_message
  in
  let parse_result = Syn.parse ~filename:ctx.fixture_path (source_slice file) in
  let source = Typ.Model.Source.make ~text:file in
  let semtree = Typ.Lower.lower ~source parse_result in
  let* json_text = Serde_json.to_string Typ.Lower.serializer semtree |> Result.map_err ~fn:Serde.Error.to_string
  in
  let* json = Data.Json.of_string json_text |> Result.map_err ~fn:Data.Json.error_to_string in Test.Snapshot.assert_json ~ctx:ctx.test ~actual:json

let tests = Test.FixtureRunner.cases () ~dir:fixtures_dir ~filter:fixture_filter ~snapshot_path:(
  fun path -> Some (Path.add_extension path ~ext:"semtree.expected")
) ~run:lowering_test

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
