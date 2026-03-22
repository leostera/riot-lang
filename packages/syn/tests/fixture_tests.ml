open Std
open Std.Data
open Syn

let parse_result_to_json result =
  let kind_to_json kind = Json.String (SyntaxKind.to_string kind) in
  let text_to_json text = Json.String text in
  let tree_json =
    Ceibo.Green.to_json ~kind_to_json ~text_to_json
      (Ceibo.Green.Node result.Parser.tree)
  in
  Json.Object
    [
      ("tree", tree_json);
      ( "diagnostics",
        Json.Array (List.map Diagnostic.to_json result.Parser.diagnostics) );
    ]

let normalize_json json =
  Json.to_string json

let parse_expected_json raw_json =
  Json.of_string raw_json
  |> Result.expect ~msg:"Failed to parse expected JSON fixture"

let test_fixture fixture_path expected_path =
  let source =
    Fs.read (Path.v fixture_path) |> Result.expect ~msg:"Failed to read fixture"
  in
  let expected_json =
    Fs.read (Path.v expected_path)
    |> Result.expect ~msg:"Failed to read expected"
  in

  let parse_result = Syn.parse ~filename:(Path.v fixture_path) source in
  let actual_json = parse_result_to_json parse_result in
  let actual_str = normalize_json actual_json in
  let expected_str = parse_expected_json expected_json |> normalize_json in

  if actual_str = expected_str then Ok ()
  else
    Error
      ("Parse tree mismatch for " ^ fixture_path ^ "\nExpected:\n" ^
       expected_str ^ "\n\nActual:\n" ^ actual_str ^ "\n")

let discover_fixtures () =
  let fixtures_dir = Path.v "packages/syn/tests/fixtures" in
  let entries_iter =
    Fs.read_dir fixtures_dir
    |> Result.expect ~msg:"Failed to read fixtures directory"
  in
  let entries = Iter.MutIterator.to_list entries_iter in

  List.filter_map
    (fun entry ->
      let path = Path.to_string (Path.join fixtures_dir entry) in
      if
        String.ends_with ~suffix:".ml" path
        || String.ends_with ~suffix:".mli" path
      then
        let expected_path = path ^ ".expected_lossless.json" in
        let exists =
          Fs.exists (Path.v expected_path) |> Result.unwrap_or ~default:false
        in
        if exists then Some (path, expected_path) else None
      else None)
    entries
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let fixtures = discover_fixtures () in
      let tests =
        List.map
          (fun (fixture_path, expected_path) ->
            let name = Path.basename (Path.v fixture_path) in
            Test.case name (fun () -> test_fixture fixture_path expected_path))
          fixtures
      in
      Test.Cli.main ~name:"syn-fixtures" ~tests ~args)
    ~args:Env.args ()
