open Std

let sample_ml = Path.v "sample.ml"
let workspace_files =
  [
    Path.v "packages/syn/src/token_cursor.mli";
    Path.v "packages/std/src/int.ml";
    Path.v "packages/std/src/bool.ml";
    Path.v "packages/std/src/option.ml";
    Path.v "packages/std/src/result.ml";
  ]

let parse_ml source = Syn.parse ~filename:sample_ml source

let parse_file path =
  let source = Fs.read path |> Result.expect ~msg:"fixture file should exist" in
  Syn.parse ~filename:path source

let assert_idempotent ~source ~msg =
  let first =
    parse_ml source |> Krasny.format |> Result.expect ~msg
  in
  let second =
    parse_ml first |> Krasny.format |> Result.expect ~msg:"formatted output should reformat"
  in
  Test.assert_equal ~expected:first ~actual:second

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
    Test.case "format rewrites parameterized let bindings between formatted lets"
      (fun () ->
        let source = "(* intro *)\nlet x = 1 + 2\nlet f x = x + 1\nlet y = 3 + 4\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect
               ~msg:"parameterized let bindings should lower through explicit fun syntax"
        in
        Test.assert_equal
          ~expected:"(* intro *)\nlet x = 1 + 2\n\nlet f = fun x -> x + 1\n\nlet y = 3 + 4\n"
          ~actual;
        Ok ());
    Test.case "format keeps mixed trivia and unsupported items parseable" (fun () ->
        let source =
          {|open Std
type t =
  | A
  | B
(* keep with x *)
let x = 1 + 2
let y = 3 + 4
|}
        in
        assert_idempotent ~source ~msg:"mixed implementation files should format";
        Ok ());
    Test.case "format keeps tuple/list/array docs idempotent" (fun () ->
        let source =
          {|let tuple_value = (left_side_identifier, right_side_identifier, final_identifier)
let list_value = [first_item_identifier; second_item_identifier; third_item_identifier]
let array_value = [|first_item_identifier; second_item_identifier; third_item_identifier|]
|}
        in
        assert_idempotent ~source ~msg:"collection expressions should stay stable";
        Ok ());
    Test.case "format keeps function and match lowering idempotent" (fun () ->
        let source =
          {|let f = function x, y -> x + y
let g = function 0 -> "zero" | _ -> "other"
let h = fun x -> match x with 0 -> "zero" | _ -> "other"
|}
        in
        assert_idempotent ~source ~msg:"function and match forms should stay stable";
        Ok ());
    Test.case "format keeps let/if/sequence layouts idempotent" (fun () ->
        let source =
          {|let x =
  if a then (
    b;
    c)
  else d

let y =
  let rec f n = if n = 0 then 1 else n * f (n - 1) in
  f 5
|}
        in
        assert_idempotent ~source ~msg:"control-flow layouts should stay stable";
        Ok ());
    Test.case "format keeps typed and labeled bindings idempotent" (fun () ->
        let source =
          {|let delimiter_of_keyword : keyword -> delimiter option = function | Begin -> Some BeginEnd | _ -> None
let label_arg = f ~y
let optional_arg = f ?y
let optional_fun = fun ?(y = 0) -> y + 1
|}
        in
        assert_idempotent ~source ~msg:"typed/labeled forms should stay stable";
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
