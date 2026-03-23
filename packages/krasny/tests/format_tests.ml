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
  let formatted =
    Krasny.format parsed |> Result.expect ~msg:"selected repo files should format"
  in
  let reparsed = Syn.parse ~filename:path formatted in
  let reparsed_hash = Krasny.syntax_hash reparsed in
  Test.assert_equal ~expected:original_hash ~actual:reparsed_hash

let tests =
  [
    Test.case "format returns the original source for a simple implementation"
      (fun () ->
        let source = "let x = 1 + 2\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"simple implementations should format"
        in
        Test.assert_equal ~expected:source ~actual;
        Ok ());
    Test.case "format preserves comments and trivia losslessly for now" (fun () ->
        let source = "(* hi *)\nlet x = 1  +  2\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"commented sources should format"
        in
        Test.assert_equal ~expected:source ~actual;
        Ok ());
    Test.case "format inserts blank lines between top-level let bindings"
      (fun () ->
        let source = "let x = 1\nlet y = 2\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"top-level lets should format"
        in
        Test.assert_equal ~expected:"let x = 1\n\nlet y = 2\n" ~actual;
        Ok ());
    Test.case "format normalizes parenthesized literals and negative literals"
      (fun () ->
        let source = "let x = (1)\nlet y = -2.5\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"simple literals should format"
        in
        Test.assert_equal ~expected:"let x = 1\n\nlet y = (-2.5)\n" ~actual;
        Ok ());
    Test.case "format expands nested let-in bindings across lines" (fun () ->
        let source = "let x =\n  let y = 1 in let z = 2 in y + z\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"nested let expressions should format"
        in
        Test.assert_equal
          ~expected:"let x =\n  let y = 1 in\n  let z = 2 in\n  y + z\n"
          ~actual;
        Ok ());
    Test.case "format rewrites single-case function expressions into fun"
      (fun () ->
        let source = "let f = function x, y -> x + y\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"single-case function expressions should format"
        in
        Test.assert_equal ~expected:"let f = fun (x, y) -> x + y\n" ~actual;
        Ok ());
    Test.case "format expands multi-case function expressions across lines"
      (fun () ->
        let source = "let f = function [] -> 0 | x :: xs -> x\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"multi-case function expressions should format"
        in
        Test.assert_equal
          ~expected:"let f = function \n  | [] -> 0 \n  | x :: xs -> x\n"
          ~actual;
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
