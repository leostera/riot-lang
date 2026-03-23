open Std

let sample_ml = Path.v "sample.ml"
let workspace_files =
  [
    Path.v "packages/krasny/src/main.ml";
    Path.v "packages/krasny/src/Krasny.ml";
    Path.v "packages/syn/src/syntax_kind.ml";
    Path.v "packages/syn/src/syntax_kind.mli";
  ]

let parse_ml source = Syn.parse ~filename:sample_ml source

let parse_file path =
  let source = Fs.read path |> Result.expect ~msg:"fixture file should exist" in
  Syn.parse ~filename:path source

let assert_roundtrip_hash path =
  let parsed = parse_file path in
  let original_hash = Krasny.syntax_hash parsed in
  let formatted = Krasny.format parsed in
  let reparsed = Syn.parse ~filename:path formatted in
  let reparsed_hash = Krasny.syntax_hash reparsed in
  Test.assert_equal ~expected:original_hash ~actual:reparsed_hash

let tests =
  [
    Test.case "format returns the original source for a simple implementation"
      (fun () ->
        let source = "let x = 1 + 2\n" in
        let actual = parse_ml source |> Krasny.format in
        Test.assert_equal ~expected:source ~actual;
        Ok ());
    Test.case "format preserves comments and trivia losslessly for now" (fun () ->
        let source = "(* hi *)\nlet x = 1  +  2\n" in
        let actual = parse_ml source |> Krasny.format in
        Test.assert_equal ~expected:source ~actual;
        Ok ());
    Test.case "format preserves syntax hash for selected codebase files"
      (fun () ->
        List.iter assert_roundtrip_hash workspace_files;
        Ok ());
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"krasny:format" ~tests ~args:Env.args)
    ~args:Env.args ()
