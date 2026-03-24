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
    Test.case "format preserves leading comments before formatted items" (fun () ->
        let source = "(* hi *)\nlet x = 1 + 2\nlet y = 3 + 4\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"leading comments should stay attached to the next item"
        in
        Test.assert_equal
          ~expected:"(* hi *)\nlet x = 1 + 2\n\nlet y = 3 + 4\n"
          ~actual;
        Ok ());
    Test.case "format preserves leading docstrings before formatted items" (fun () ->
        let source = "(** hi *)\nlet x = 1 + 2\nlet y = 3 + 4\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"leading docstrings should stay attached to the next item"
        in
        Test.assert_equal
          ~expected:"(** hi *)\nlet x = 1 + 2\n\nlet y = 3 + 4\n"
          ~actual;
        Ok ());
    Test.case "format preserves unsupported let bindings between formatted lets"
      (fun () ->
        let source = "(* intro *)\nlet x = 1 + 2\nlet f x = x + 1\nlet y = 3 + 4\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect
               ~msg:"unsupported let bindings should be preserved verbatim"
        in
        Test.assert_equal
          ~expected:"(* intro *)\nlet x = 1 + 2\n\nlet f x = x + 1\n\nlet y = 3 + 4\n"
          ~actual;
        Ok ());
    Test.case "format preserves unsupported top-level items between formatted lets"
      (fun () ->
        let source =
          "open Std\ntype t =\n  | A\n  | B\n(* keep with x *)\nlet x = 1 + 2\nlet y = 3 + 4\n"
        in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect
               ~msg:"mixed implementation files should preserve unsupported items verbatim"
        in
        Test.assert_equal
          ~expected:
            "open Std\ntype t =\n  | A\n  | B\n(* keep with x *)\n\n\nlet x = 1 + 2\n\nlet y = 3 + 4\n"
          ~actual;
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
    Test.case "format keeps chars, unit, and bare identifiers stable" (fun () ->
        let source = "let c = 'a'\nlet u = ()\nlet x = y\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"simple atoms should format"
        in
        Test.assert_equal ~expected:"let c = 'a'\n\nlet u = ()\n\nlet x = y\n" ~actual;
        Ok ());
    Test.case "format keeps infix expressions stable" (fun () ->
        let source =
          "let arithmetic = 1 + (2 * 3)\nlet comparisons = 1 < 2 && 2 < 3\nlet logic = (true && false) || true\n"
        in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"infix expressions should format"
        in
        Test.assert_equal
          ~expected:
            "let arithmetic = 1 + (2 * 3)\n\nlet comparisons = 1 < 2 && 2 < 3\n\nlet logic = (true && false) || true\n"
          ~actual;
        Ok ());
    Test.case "format keeps if expressions stable" (fun () ->
        let source =
          "let choose = if a && b then 1 else 0\nlet guard = if true then ()\n"
        in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"if expressions should format"
        in
        Test.assert_equal
          ~expected:"let choose = if a && b then 1 else 0\n\nlet guard = if true then ()\n"
          ~actual;
        Ok ());
    Test.case "format preserves let rec and let-and expressions" (fun () ->
        let source =
          "let rec_case =\n  let rec f n = if n = 0 then 1 else n * f (n - 1) in\n  f 5\nlet and_case =\n  let a = 1 and b = 2 in\n  a + b\n"
        in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"let variants should format"
        in
        Test.assert_equal
          ~expected:
            "let rec_case =\n  let rec f n = if n = 0 then 1 else n * f (n - 1) in\n  f 5\n\nlet and_case =\n  let a = 1 and b = 2 in\n  a + b\n"
          ~actual;
        Ok ());
    Test.case "format keeps labeled and optional forms stable" (fun () ->
        let source =
          "let label_arg = f ~y\nlet optional_arg = f ?y\nlet labeled_fun = fun ~y -> y + 1\nlet optional_fun = fun ?(y = 0) -> y + 1\nlet optional_match = fun ?y -> match y with Some v -> v | None -> 0\nlet optional_tuple = fun ?y ?z -> (y, z)\n"
        in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"labeled and optional forms should format"
        in
        Test.assert_equal
          ~expected:
            "let label_arg = f ~y\n\nlet optional_arg = f ?y\n\nlet labeled_fun = fun ~y -> y + 1\n\nlet optional_fun = fun ?(y = 0) -> y + 1\n\nlet optional_match = fun ?y -> match y with Some v -> v | None -> 0\n\nlet optional_tuple = fun ?y ?z -> (y, z)\n"
          ~actual;
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
    Test.case "format expands match bodies inside fun expressions" (fun () ->
        let source = "let f = fun x -> match x with 0 -> \"zero\" | _ -> \"other\"\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"match bodies should format"
        in
        Test.assert_equal
          ~expected:
            "let f = fun x -> \n  match x with \n  | 0 -> \"zero\" \n  | _ -> \"other\"\n"
          ~actual;
        Ok ());
    Test.case "format rewrites simple multi-case functions into fun-match"
      (fun () ->
        let source = "let f = function 0 -> \"zero\" | _ -> \"other\"\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"simple multi-case functions should format"
        in
        Test.assert_equal
          ~expected:
            "let f = fun x ->\n  match x with\n  | 0 -> \"zero\" \n  | _ -> \"other\"\n"
          ~actual;
        Ok ());
    Test.case "format keeps or-pattern function cases multiline"
      (fun () ->
        let source = "let ors = function 1 | 2 | 3 -> true | _ -> false\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"or-pattern functions should format"
        in
        Test.assert_equal
          ~expected:"let ors = function \n | 1 \n | 2 \n | 3 -> true \n | _ -> false\n"
          ~actual;
        Ok ());
    Test.case "format keeps guarded function cases multiline" (fun () ->
        let source = "let f = function x when x > 0 -> x | _ -> 0\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"guarded function cases should format"
        in
        Test.assert_equal
          ~expected:"let f = \n  function \n  | x when x > 0 -> x \n  | _ -> 0\n"
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
