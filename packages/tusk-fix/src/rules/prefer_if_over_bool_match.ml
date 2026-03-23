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
      is_unit_expression expr.inner
  | _ -> false

let rec case_pattern_kind = function
  | Syn.Cst.Pattern.Literal
      { literal = Syn.Cst.PatternLiteral.Bool { literal_token; _ }; _ } ->
      if String.equal (Syn.Cst.Token.text literal_token) "true" then
        TruePattern
      else
        FalsePattern
  | Syn.Cst.Pattern.Wildcard _ -> WildcardPattern
  | Syn.Cst.Pattern.Parenthesized { inner; _ } -> case_pattern_kind inner
  | Syn.Cst.Pattern.Identifier _ | Syn.Cst.Pattern.Literal _ ->
      OtherPattern
  | Syn.Cst.Pattern.Extension _
  | Syn.Cst.Pattern.Lazy _ | Syn.Cst.Pattern.Exception _
  | Syn.Cst.Pattern.Range _ | Syn.Cst.Pattern.Operator _
  | Syn.Cst.Pattern.FirstClassModule _ | Syn.Cst.Pattern.PolyVariant _
  | Syn.Cst.Pattern.PolyVariantInherit _ | Syn.Cst.Pattern.Constructor _
  | Syn.Cst.Pattern.Tuple _ | Syn.Cst.Pattern.List _ | Syn.Cst.Pattern.Array _
  | Syn.Cst.Pattern.Record _ | Syn.Cst.Pattern.Cons _ | Syn.Cst.Pattern.Or _
  | Syn.Cst.Pattern.Alias _ | Syn.Cst.Pattern.Typed _
  | Syn.Cst.Pattern.Effect _ | Syn.Cst.Pattern.LocalOpen _ ->
      OtherPattern

let suggestion_for_match (expr : Syn.Cst.match_expression) =
  match expr.cases with
  | [ first_case; second_case ] -> (
      match
        case_pattern_kind first_case.pattern,
        case_pattern_kind second_case.pattern
      with
      | TruePattern, FalsePattern ->
          "Rewrite this match as `if <condition> then ... else ...`."
      | FalsePattern, TruePattern ->
          "Rewrite this match as `if not <condition> then ... else ...`."
      | TruePattern, WildcardPattern ->
          if is_unit_expression second_case.body then
            "Rewrite this match as `if <condition> then ...`."
          else
            "Rewrite this match as `if <condition> then ... else ...`."
      | FalsePattern, WildcardPattern ->
          if is_unit_expression second_case.body then
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

let should_flag_match (expr : Syn.Cst.match_expression) =
  match expr.cases with
  | [ first_case; second_case ] ->
      first_case.guard = None
      && second_case.guard = None
      &&
      match
        case_pattern_kind first_case.pattern,
        case_pattern_kind second_case.pattern
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

let make_diagnostic (expr : Syn.Cst.match_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
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
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.expressions_of_structure_item
      |> List.filter_map diagnostic_for_expression

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
