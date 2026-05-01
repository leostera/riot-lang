open Std
open Std.Data
open Std.Collections
open Syn

module Iterator = Iter.Iterator

let canonicalize_json =
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | Json.Object fields ->
        Json.Object (
          fields
          |> List.map ~fn:(fun (key, value) -> (key, loop value))
          |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
        )
    | Json.Array items -> Json.Array (List.map items ~fn:loop)
    | other -> other
  in
  loop

let test_diagnostic = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source =
    Fs.read ctx.fixture_path
    |> Result.expect ~msg:"Failed to read test file"
  in
  let source =
    match IO.IoVec.IoSlice.from_string source with
    | Ok source -> source
    | Error error ->
        panic ("failed to create diagnostic source slice: " ^ IO.IoVec.error_message error)
  in
  let parse_result = Syn.parse ~filename:ctx.fixture_path source in
  let actual_diagnostics = parse_result.Parser.diagnostics in
  let items = ref [] in
  Vector.iter actual_diagnostics
  |> Iterator.for_each ~fn:(fun diagnostic -> items := Diagnostic.to_json diagnostic :: !items);
  let actual_json =
    Json.Array (List.reverse !items)
    |> canonicalize_json
  in
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun json -> Json.to_string_pretty json ^ "\n")
    ~actual:actual_json

let diagnostic_marker_path = fun path -> Path.add_extension path ~ext:"diagnostic"

let filter_diagnostic_fixture = fun path ->
  match Path.extension path with
  | Some ".ml" ->
      let diagnostic_path = diagnostic_marker_path path in
      let exists =
        Fs.exists diagnostic_path
        |> Result.unwrap_or ~default:false
      in
      if exists then
        Test.FixtureRunner.Keep
      else
        Test.FixtureRunner.Skip
  | _ -> Test.FixtureRunner.Skip

let run_diagnostic = fun ctx -> test_diagnostic ~ctx

let main ~args =
  let tests =
    Test.FixtureRunner.cases
      ()
      ~dir:(Path.v "packages/syn/tests/diagnostics")
      ~filter:filter_diagnostic_fixture
      ~run:run_diagnostic
  in
  Test.Cli.main ~name:"syn-diagnostics" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
