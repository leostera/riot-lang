open Std

let source_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create riot-fix test source slice"

let parse_source_file = fun source ->
  let parsed = Syn.parse ~filename:(Path.v "fix_test.ml") (source_slice source) in
  if Std.Collections.Vector.length parsed.Syn.Parser.diagnostics > 0 then
    panic "riot-fix test source should parse";
  Syn.Ast.SourceFile.make parsed.Syn.Parser.tree

let span_of_token = fun token ->
  let start, end_ = Syn.Ast.Token.raw_range token in Syn.Ceibo.Span.make ~start ~end_

let find_token_by_text = fun root text ->
  let found = ref None in
  Syn.Ast.Node.for_each_token root ~fn:(
    fun token ->
      if Option.is_none !found && String.equal (Syn.Ast.Token.text token) text then
        found := Some token
  );
  !found

let token_by_text_at = fun source text ~start ->
  let root = parse_source_file source in
  let found = ref None in
  Syn.Ast.Node.for_each_token root ~fn:(
    fun token ->
      let token_start, _ = Syn.Ast.Token.raw_range token in
      if Option.is_none !found && token_start = start && String.equal (Syn.Ast.Token.text token) text then
        found := Some token
  );
  match !found with
  | Some token -> token
  | None -> panic ("expected to find token " ^ text ^ " at offset " ^ Int.to_string start)

let warning_diagnostic = fun ~rule_id ~message ~token ~fix -> Riot_fix.Diagnostic.make ~severity:Warning ~kind:(Riot_fix.Diagnostic.Known { rule_id; message }) ~span:(span_of_token token) ~fix ()

let replace_token_rule = fun ~rule_id ~needle ~replacement ->
  let rule_id = Riot_fix.Rule_id.of_string rule_id in
  let message = "Replace " ^ needle ^ " with " ^ replacement in Riot_fix.Rule.make ~id:rule_id ~description:message ~explain:message ~run:(
    fun _ctx red_root ->
      match find_token_by_text red_root needle with
      | None -> []
      | Some token ->
          let fix = Riot_fix.Fix.make ~title:message ~operations:[ Riot_fix.Fix.replace_token_with_text ~target:token ~text:replacement ] in [ warning_diagnostic ~rule_id ~message ~token ~fix ]
  ) ()

let overlapping_replace_rule = fun ~rule_id ~needle ~replacement ~overlap_text ->
  let rule_id = Riot_fix.Rule_id.of_string rule_id in
  let message = "Apply an overlapping replacement to " ^ needle in Riot_fix.Rule.make ~id:rule_id ~description:message ~explain:message ~run:(
    fun _ctx red_root ->
      match find_token_by_text red_root needle with
      | None -> []
      | Some token ->
          let fix = Riot_fix.Fix.make ~title:message ~operations:[ Riot_fix.Fix.replace_node_with_text ~target:red_root ~text:replacement ] in
          ignore overlap_text;
          [ warning_diagnostic ~rule_id ~message ~token ~fix ]
  ) ()

let tests =
  [
    Test.case "apply single operation"
      (
        fun _ctx ->
          let source = "open Stdlib\n" in
          let token = token_by_text_at source "Stdlib" ~start:5 in
          let actual = Riot_fix.Fix.apply_operation ~source (Riot_fix.Fix.replace_token_with_text ~target:token ~text:"Std") |> Result.expect ~msg:"apply single operation failed" in
          Test.assert_equal ~expected:"open Std\n" ~actual;
          Ok ()
      );
    Test.case "apply multiple operations in descending span order"
      (
        fun _ctx ->
          let source = "open Stdlib\nlet q : int Queue.t = Queue.create ()\n" in
          let stdlib_token = token_by_text_at source "Stdlib" ~start:5 in
          let queue_type_token = token_by_text_at source "Queue" ~start:24 in
          let queue_value_token = token_by_text_at source "Queue" ~start:34 in
          let fixes = [ Riot_fix.Fix.make ~title:"replace Queue" ~operations:[ Riot_fix.Fix.replace_token_with_text ~target:queue_type_token ~text:"Std.Collections.Queue"; Riot_fix.Fix.replace_token_with_text ~target:queue_value_token ~text:"Std.Collections.Queue" ]; Riot_fix.Fix.make ~title:"replace Stdlib" ~operations:[ Riot_fix.Fix.replace_token_with_text ~target:stdlib_token ~text:"Std" ] ] in
          let actual = Riot_fix.Fix.apply_fixes ~source fixes |> Result.expect ~msg:"apply multiple edits failed" in
          let expected = "open Std\nlet q : int Std.Collections.Queue.t = \
           Std.Collections.Queue.create ()\n" in
          Test.assert_equal ~expected ~actual;
          Ok ()
      );
    Test.case "reject overlapping operations"
      (
        fun _ctx ->
          let source = "open Stdlib\n" in
          let root = parse_source_file source in
          let token = token_by_text_at source "Stdlib" ~start:5 in
          let fix = Riot_fix.Fix.make ~title:"bad overlap" ~operations:[ Riot_fix.Fix.replace_token_with_text ~target:token ~text:"Std"; Riot_fix.Fix.replace_node_with_text ~target:root ~text:"open Std\n" ] in
          Test.assert_error (Riot_fix.Fix.apply_fix ~source fix);
          Ok ()
      );
    Test.case "source runner skips linting when parsing fails"
      (
        fun _ctx ->
          let source = "let =" in
          let result = Riot_fix.Source_runner.run_rule ~rule:(replace_token_rule ~rule_id:"test:broken-source" ~needle:"let" ~replacement:"and") source in
          Test.assert_true (not (List.is_empty result.parse_diagnostics));
          Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
          Ok ()
      );
    Test.case "rule-test applies snake-case type fixes and reruns cleanly"
      (
        fun _ctx ->
          let source = "type userProfile = { name : string }\n" in
          let result = Riot_fix.Rule_test.run_rule ~rule:(Riot_fix.Rules.Snake_case_type_names.make ()) source |> Result.expect ~msg:"expected snake-case type rule to apply" in
          Test.assert_equal ~expected:1 ~actual:(List.length result.applied_fixes);
          Test.assert_equal ~expected:(Some "type user_profile = { name : string }\n") ~actual:result.fixed_source;
          let remaining =
            match result.after with
            | Some after -> after.diagnostics
            | None -> []
          in
          Test.assert_equal ~expected:[] ~actual:remaining;
          Ok ()
      );
    Test.case "rule-test applies multiple non-overlapping fixes"
      (
        fun _ctx ->
          let source = "type userProfile = int\nlet old_value = userProfile\n" in
          let result = Riot_fix.Rule_test.run ~rules:[ Riot_fix.Rules.Snake_case_type_names.make (); replace_token_rule ~rule_id:"test:rename-binding" ~needle:"old_value" ~replacement:"current_value" ] source |> Result.expect ~msg:"expected multiple safe fixes to apply" in
          Test.assert_equal ~expected:2 ~actual:(List.length result.initial.diagnostics);
          Test.assert_equal ~expected:2 ~actual:(List.length result.applied_fixes);
          Test.assert_equal ~expected:(Some "type user_profile = int\nlet current_value = userProfile\n") ~actual:result.fixed_source;
          let remaining =
            match result.after with
            | Some after -> after.diagnostics
            | None -> []
          in
          Test.assert_equal ~expected:[] ~actual:remaining;
          Ok ()
      );
    Test.case "rule-test rejects overlapping fixes from multiple rules"
      (
        fun _ctx ->
          let source = "let old_value = 1\n" in
          let actual = Riot_fix.Rule_test.run ~rules:[ replace_token_rule ~rule_id:"test:replace-whole-token" ~needle:"old_value" ~replacement:"value"; overlapping_replace_rule ~rule_id:"test:replace-tail" ~needle:"old_value" ~replacement:"tail" ~overlap_text:"ld_value" ] source in
          Test.assert_error actual;
          Ok ()
      );
  ]

let main ~args:_ = Test.Cli.main ~name:"riot-fix:fix" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
