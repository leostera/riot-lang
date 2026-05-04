open Std

let iter_fold = fun fold value ~fn ->
  fold
    value
    ~init:()
    ~fn:(fun item () ->
      fn item;
      Syn.Ast.Continue ())

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create riot-fix test source slice"

let parse_source_file = fun source ->
  let parsed = Syn.parse ~filename:(Path.v "fix_test.ml") (source_slice source) in
  if Std.Collections.Vector.length parsed.Syn.Parser.diagnostics > 0 then
    panic "riot-fix test source should parse";
  Syn.Ast.SourceFile.make parsed.Syn.Parser.tree

let span_of_token = fun token ->
  Syn.Span.make
    ~start:(Syn.Ast.Token.span_start token)
    ~end_:(Syn.Ast.Token.span_end token)

let find_token_by_text = fun root text ->
  let found = ref None in
  iter_fold
    Syn.Ast.Node.fold_token
    root
    ~fn:(fun token ->
      if Option.is_none !found && String.equal (Syn.Ast.Token.text token) text then
        found := Some token);
  !found

let token_by_text_at = fun source text ~start ->
  let root = parse_source_file source in
  let found = ref None in
  iter_fold
    Syn.Ast.Node.fold_token
    (Syn.Ast.SourceFile.as_node root)
    ~fn:(fun token ->
      let token_start = Syn.Ast.Token.span_start token in
      if
        Option.is_none !found && token_start = start && String.equal (Syn.Ast.Token.text token) text
      then
        found := Some token);
  match !found with
  | Some token -> token
  | None -> panic ("expected to find token " ^ text ^ " at offset " ^ Int.to_string start)

let warning_diagnostic = fun ~rule_id ~message ~token ~fix ->
  Riot_fix.Diagnostic.make
    ~severity:Warning
    ~kind:(Riot_fix.Diagnostic.Known { rule_id; message })
    ~span:(span_of_token token)
    ~fix
    ()

let replace_token_rule = fun ~rule_id ~needle ~replacement ->
  let rule_id = Riot_fix.Rule_id.from_string rule_id in
  let message = "Replace " ^ needle ^ " with " ^ replacement in
  Riot_fix.Rule.make
    ~id:rule_id
    ~description:message
    ~explain:message
    ~run:(fun _ctx red_root ->
      match find_token_by_text red_root needle with
      | None -> []
      | Some token ->
          let fix =
            Riot_fix.Fix.make
              ~title:message
              ~operations:[ Riot_fix.Fix.replace_token_with_text ~target:token ~text:replacement ]
          in
          [ warning_diagnostic ~rule_id ~message ~token ~fix ])
    ()

let overlapping_replace_rule = fun ~rule_id ~needle ~replacement ~overlap_text ->
  let rule_id = Riot_fix.Rule_id.from_string rule_id in
  let message = "Apply an overlapping replacement to " ^ needle in
  Riot_fix.Rule.make
    ~id:rule_id
    ~description:message
    ~explain:message
    ~run:(fun _ctx red_root ->
      match find_token_by_text red_root needle with
      | None -> []
      | Some token ->
          let fix =
            Riot_fix.Fix.make
              ~title:message
              ~operations:[ Riot_fix.Fix.replace_node_with_text ~target:red_root ~text:replacement ]
          in
          ignore overlap_text;
          [ warning_diagnostic ~rule_id ~message ~token ~fix ])
    ()

let tests = [
  Test.case
    "apply single operation"
    (fun _ctx ->
      let source = "open Stdlib\n" in
      let token = token_by_text_at source "Stdlib" ~start:5 in
      let actual =
        Riot_fix.Fix.apply_operation
          ~source
          (Riot_fix.Fix.replace_token_with_text ~target:token ~text:"Std")
        |> Result.expect ~msg:"apply single operation failed"
      in
      Test.assert_equal ~expected:"open Std\n" ~actual;
      Ok ());
  Test.case
    "apply multiple operations in descending span order"
    (fun _ctx ->
      let source = "open Stdlib\nlet q : int Queue.t = Queue.create ()\n" in
      let stdlib_token = token_by_text_at source "Stdlib" ~start:5 in
      let queue_type_token = token_by_text_at source "Queue" ~start:24 in
      let queue_value_token = token_by_text_at source "Queue" ~start:34 in
      let fixes = [
        Riot_fix.Fix.make
          ~title:"replace Queue"
          ~operations:[
            Riot_fix.Fix.replace_token_with_text
              ~target:queue_type_token
              ~text:"Std.Collections.Queue";
            Riot_fix.Fix.replace_token_with_text
              ~target:queue_value_token
              ~text:"Std.Collections.Queue";
          ];
        Riot_fix.Fix.make
          ~title:"replace Stdlib"
          ~operations:[ Riot_fix.Fix.replace_token_with_text ~target:stdlib_token ~text:"Std" ];
      ]
      in
      let actual =
        Riot_fix.Fix.apply_fixes ~source fixes
        |> Result.expect ~msg:"apply multiple edits failed"
      in
      let expected =
        "open Std\nlet q : int Std.Collections.Queue.t = \
           Std.Collections.Queue.create ()\n"
      in
      Test.assert_equal ~expected ~actual;
      Ok ());
  Test.case
    "reject overlapping operations"
    (fun _ctx ->
      let source = "open Stdlib\n" in
      let root = parse_source_file source in
      let token = token_by_text_at source "Stdlib" ~start:5 in
      let fix =
        Riot_fix.Fix.make
          ~title:"bad overlap"
          ~operations:[
            Riot_fix.Fix.replace_token_with_text ~target:token ~text:"Std";
            Riot_fix.Fix.replace_node_with_text
              ~target:(Syn.Ast.SourceFile.as_node root)
              ~text:"open Std\n";
          ]
      in
      Test.assert_error (Riot_fix.Fix.apply_fix ~source fix);
      Ok ());
  Test.case
    "source runner skips linting when parsing fails"
    (fun _ctx ->
      let source = "let =" in
      let result =
        Riot_fix.Source_runner.run_rule
          ~rule:(replace_token_rule ~rule_id:"test:broken-source" ~needle:"let" ~replacement:"and")
          source
      in
      Test.assert_true (not (List.is_empty result.parse_diagnostics));
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule-test applies token fixes and reruns cleanly"
    (fun _ctx ->
      let source = "let old_value = 1\n" in
      let result =
        Riot_fix.Rule_test.run_rule
          ~rule:(replace_token_rule
            ~rule_id:"test:rename-binding"
            ~needle:"old_value"
            ~replacement:"current_value")
          source
        |> Result.expect ~msg:"expected token replacement rule to apply"
      in
      Test.assert_equal ~expected:1 ~actual:(List.length result.applied_fixes);
      Test.assert_equal ~expected:(Some "let current_value = 1\n") ~actual:result.fixed_source;
      let remaining =
        match result.after with
        | Some after -> after.diagnostics
        | None -> []
      in
      Test.assert_equal ~expected:[] ~actual:remaining;
      Ok ());
  Test.case
    "rule-test applies multiple non-overlapping fixes"
    (fun _ctx ->
      let source = "let old_value = 1\nlet other_value = 2\n" in
      let result =
        Riot_fix.Rule_test.run
          ~rules:[
            replace_token_rule
              ~rule_id:"test:rename-old-binding"
              ~needle:"old_value"
              ~replacement:"current_value";
            replace_token_rule
              ~rule_id:"test:rename-other-binding"
              ~needle:"other_value"
              ~replacement:"next_value";
          ]
          source
        |> Result.expect ~msg:"expected multiple safe fixes to apply"
      in
      Test.assert_equal ~expected:2 ~actual:(List.length result.initial.diagnostics);
      Test.assert_equal ~expected:2 ~actual:(List.length result.applied_fixes);
      Test.assert_equal
        ~expected:(Some "let current_value = 1\nlet next_value = 2\n")
        ~actual:result.fixed_source;
      let remaining =
        match result.after with
        | Some after -> after.diagnostics
        | None -> []
      in
      Test.assert_equal ~expected:[] ~actual:remaining;
      Ok ());
  Test.case
    "rule-test rejects overlapping fixes from multiple rules"
    (fun _ctx ->
      let source = "let old_value = 1\n" in
      let actual =
        Riot_fix.Rule_test.run
          ~rules:[
            replace_token_rule
              ~rule_id:"test:replace-whole-token"
              ~needle:"old_value"
              ~replacement:"value";
            overlapping_replace_rule
              ~rule_id:"test:replace-tail"
              ~needle:"old_value"
              ~replacement:"tail"
              ~overlap_text:"ld_value";
          ]
          source
      in
      Test.assert_error actual;
      Ok ());
]

let main ~args:_ = Test.Cli.main ~name:"riot-fix:fix" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
