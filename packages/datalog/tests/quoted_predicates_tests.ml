open Std
open Datalog

let test_single_quoted_predicate () =
  let query = "'codedb:attr:path'(E, V)" in
  match Parser.parse_query query with
  | Error diags ->
      let msg =
        List.map (fun d -> Parser.Diagnostic.to_string d) diags
        |> String.concat "; "
      in
      panic ("Parse failed: " ^ msg)
  | Ok cst -> (
      match Ast_from_cst.query_of_cst cst with
      | Error e -> panic ("AST conversion failed: " ^ e)
      | Ok (Ast.Single atom) ->
          assert (atom.predicate = "codedb:attr:path");
          println "✓ Single-quoted predicate works"
      | Ok (Ast.Multi _) -> panic "Expected Single atom, got Multi")

let test_double_quoted_predicate () =
  let query = "\"codedb:attr:path\"(E, V)" in
  match Parser.parse_query query with
  | Error diags ->
      let msg =
        List.map (fun d -> Parser.Diagnostic.to_string d) diags
        |> String.concat "; "
      in
      panic ("Parse failed: " ^ msg)
  | Ok cst -> (
      match Ast_from_cst.query_of_cst cst with
      | Error e -> panic ("AST conversion failed: " ^ e)
      | Ok (Ast.Single atom) ->
          assert (atom.predicate = "codedb:attr:path");
          println "✓ Double-quoted predicate works"
      | Ok (Ast.Multi _) -> panic "Expected Single atom, got Multi")

let test_unquoted_still_works () =
  let query = "path(E, V)" in
  match Parser.parse_query query with
  | Error diags ->
      let msg =
        List.map (fun d -> Parser.Diagnostic.to_string d) diags
        |> String.concat "; "
      in
      panic ("Parse failed: " ^ msg)
  | Ok cst -> (
      match Ast_from_cst.query_of_cst cst with
      | Error e -> panic ("AST conversion failed: " ^ e)
      | Ok (Ast.Single atom) ->
          assert (atom.predicate = "path");
          println "✓ Unquoted predicate still works"
      | Ok (Ast.Multi _) -> panic "Expected Single atom, got Multi")

let test_special_uri_chars () =
  let query = "'http://example.org/pred#frag?x=1'(E, V)" in
  match Parser.parse_query query with
  | Error diags ->
      let msg =
        List.map (fun d -> Parser.Diagnostic.to_string d) diags
        |> String.concat "; "
      in
      panic ("Parse failed: " ^ msg)
  | Ok cst -> (
      match Ast_from_cst.query_of_cst cst with
      | Error e -> panic ("AST conversion failed: " ^ e)
      | Ok (Ast.Single atom) ->
          let expected = "http://example.org/pred#frag?x=1" in
          assert (atom.predicate = expected);
          println "✓ Special URI characters work"
      | Ok (Ast.Multi _) -> panic "Expected Single atom, got Multi")

let test_quoted_in_rule_head () =
  let program = "'my:pred'(X) :- other(X)." in
  match Parser.parse program with
  | Error diags ->
      let msg =
        List.map (fun d -> Parser.Diagnostic.to_string d) diags
        |> String.concat "; "
      in
      panic ("Parse failed: " ^ msg)
  | Ok cst -> (
      match Ast_from_cst.program_of_cst cst with
      | Error e -> panic ("AST conversion failed: " ^ e)
      | Ok prog -> (
          match prog.rules with
          | [ rule ] ->
              assert (rule.head.predicate = "my:pred");
              println "✓ Quoted predicate in rule head works"
          | _ -> panic "Expected exactly 1 rule"))

let test_quoted_in_fact () =
  let program = "'my:fact'(\"value\")." in
  match Parser.parse program with
  | Error diags ->
      let msg =
        List.map (fun d -> Parser.Diagnostic.to_string d) diags
        |> String.concat "; "
      in
      panic ("Parse failed: " ^ msg)
  | Ok cst -> (
      match Ast_from_cst.program_of_cst cst with
      | Error e -> panic ("AST conversion failed: " ^ e)
      | Ok prog -> (
          match prog.facts with
          | [ fact ] ->
              assert (fact.predicate = "my:fact");
              println "✓ Quoted predicate in fact works"
          | _ -> panic "Expected exactly 1 fact"))

let () =
  println "Running quoted predicate tests...\n";

  test_single_quoted_predicate ();
  test_double_quoted_predicate ();
  test_unquoted_still_works ();
  test_special_uri_chars ();
  test_quoted_in_rule_head ();
  test_quoted_in_fact ();

  println "\n✅ All quoted predicate tests passed!"
