open Std
open Std.Data
open Syn

let canonicalize_json =
  let rec loop = function
    | Json.Object fields ->
        Json.Object (
          fields |> List.map (fun (key, value) -> (key, loop value)) |> List.sort
            (fun (left, _) (right, _) ->
              String.compare left right)
        )
    | Json.Array items -> Json.Array (List.map loop items)
    | other -> other
  in
  loop

let test_diagnostic = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source = Fs.read ctx.fixture_path |> Result.expect ~msg:"Failed to read test file" in
  let parse_result = Syn.parse ~filename:ctx.fixture_path source in
  let actual_diagnostics = parse_result.Parser.diagnostics in
  let actual_json = Json.Array (List.map Diagnostic.to_json actual_diagnostics) |> canonicalize_json in
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun json -> Json.to_string_pretty json ^ "\n")
    ~actual:actual_json

let diagnostic_marker_path = fun path ->
  match Path.extension path with
  | Some ext -> Path.add_extension path ~ext:(ext ^ ".diagnostic")
  | None -> Path.add_extension path ~ext:"diagnostic"

let filter_diagnostic_fixture = fun path ->
  match Path.extension path with
  | Some ".ml" ->
      let diagnostic_path = diagnostic_marker_path path in
      let exists = Fs.exists diagnostic_path |> Result.unwrap_or ~default:false in
      if exists then
        `keep
      else
        `skip
  | _ -> `skip

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:(Path.v "packages/syn/tests/diagnostics")
          ~filter:filter_diagnostic_fixture
          ~run:(fun ctx -> test_diagnostic ~ctx)
      in
      Test.Cli.main ~name:"syn-diagnostics" ~tests ~args)
    ~args:Env.args
    ()
