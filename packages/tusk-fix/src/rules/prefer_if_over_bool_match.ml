open Std

let rule_id = "prefer-if-over-bool-match"
let rule_name = "Prefer If Over Bool Match"
let rule_code = "F0132"

let rule_description =
  "Matching on booleans should be written as `if` expressions"

let rule_message =
  "Replace boolean matches with `if` expressions."

let rule_explain =
  {|
Matching on booleans should usually be written as `if` expressions.

Why this rule exists:
- `match is_ready with true -> ... | false -> ...` is just a noisier `if`.
- `if` makes the branching condition obvious immediately.
- When the fallback branch is just `()`, the shorter conditional form is even clearer.

Examples:
  Bad:    match is_ready with true -> render () | false -> fallback ()
  Better: if is_ready then render () else fallback ()

  Bad:    match is_ready with false -> render () | _ -> ()
  Better: if not is_ready then render ()
|}

type case_pattern_kind =
  | TruePattern
  | FalsePattern
  | WildcardPattern
  | OtherPattern

let rec is_unit_expression = function
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.Unit _) -> true
  | Syn.Cst.Expression.Parenthesized expr ->
      is_unit_expression (Syn.Cst.ParenthesizedExpression.inner expr)
  | _ -> false

let rec case_pattern_kind = function
  | Syn.Cst.Pattern.Literal (Syn.Cst.PatternLiteral.Bool { literal_token; _ }) ->
      if String.equal (Syn.Cst.Token.text literal_token) "true" then
        TruePattern
      else
        FalsePattern
  | Syn.Cst.Pattern.Wildcard _ -> WildcardPattern
  | Syn.Cst.Pattern.Parenthesized { inner; _ } -> case_pattern_kind inner
  | Syn.Cst.Pattern.Identifier _ | Syn.Cst.Pattern.Literal _
  | Syn.Cst.Pattern.Unknown _ ->
      OtherPattern

let suggestion_for_match expr =
  match Syn.Cst.MatchExpression.cases expr with
  | [ first_case; second_case ] -> (
      match
        case_pattern_kind (Syn.Cst.MatchCase.pattern first_case),
        case_pattern_kind (Syn.Cst.MatchCase.pattern second_case)
      with
      | TruePattern, FalsePattern ->
          "Rewrite this match as `if <condition> then ... else ...`."
      | FalsePattern, TruePattern ->
          "Rewrite this match as `if not <condition> then ... else ...`."
      | TruePattern, WildcardPattern ->
          if is_unit_expression (Syn.Cst.MatchCase.body second_case) then
            "Rewrite this match as `if <condition> then ...`."
          else
            "Rewrite this match as `if <condition> then ... else ...`."
      | FalsePattern, WildcardPattern ->
          if is_unit_expression (Syn.Cst.MatchCase.body second_case) then
            "Rewrite this match as `if not <condition> then ...`."
          else
            "Rewrite this match as `if not <condition> then ... else ...`."
      | WildcardPattern, TruePattern
      | WildcardPattern, FalsePattern
      | WildcardPattern, WildcardPattern
      | TruePattern, OtherPattern
      | FalsePattern, OtherPattern
      | WildcardPattern, OtherPattern
      | OtherPattern, _ ->
          "Rewrite this boolean match as an `if` expression.")
  | _ -> "Rewrite this boolean match as an `if` expression."

let should_flag_match expr =
  match Syn.Cst.MatchExpression.cases expr with
  | [ first_case; second_case ] ->
      Syn.Cst.MatchCase.guard first_case = None
      && Syn.Cst.MatchCase.guard second_case = None
      &&
      match
        case_pattern_kind (Syn.Cst.MatchCase.pattern first_case),
        case_pattern_kind (Syn.Cst.MatchCase.pattern second_case)
      with
      | TruePattern, FalsePattern
      | FalsePattern, TruePattern
      | TruePattern, WildcardPattern
      | FalsePattern, WildcardPattern ->
          true
      | WildcardPattern, _
      | OtherPattern, _
      | _, OtherPattern
      | WildcardPattern, WildcardPattern ->
          false
      | _ -> false
  | _ -> false

let make_diagnostic expr =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.MatchExpression.syntax_node expr))
    ~suggestion:(suggestion_for_match expr)
    ()

let safe_should_flag_match expr =
  try should_flag_match expr with
  | Match_failure _ -> false

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Match expr when safe_should_flag_match expr ->
      Some (make_diagnostic expr)
  | _ -> None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.expressions source_file
      |> List.filter_map diagnostic_for_expression

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
