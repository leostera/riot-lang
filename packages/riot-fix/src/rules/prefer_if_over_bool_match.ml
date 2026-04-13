open Std

let rule_id = "prefer-if-over-bool-match"

let rule_description = "Matching on booleans should be written as `if` expressions"

let rule_explain = {|
Booleans already describe a two-way branch. Writing `match is_ready with true -> ... |
false -> ...` repeats that fact in a more verbose form than `if`.

`if` keeps the reader focused on the condition first and the two outcomes second. That
is exactly what boolean branching is for. The simplification becomes even clearer when
the negative branch is just `()`, because `if cond then effect ()` already expresses
"do this only when the condition holds."

Reserve `match` for the places where you really are inspecting several cases or want
pattern matching, not for the most ordinary boolean branch.
|}

type case_pattern_kind =
  | TruePattern
  | FalsePattern
  | WildcardPattern
  | OtherPattern

let rec is_unit_expression = function
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.Unit _) -> true
  | Syn.Cst.Expression.Parenthesized expr -> is_unit_expression expr.inner
  | _ -> false

let rec case_pattern_kind = function
  | Syn.Cst.Pattern.Literal { literal=Syn.Cst.PatternLiteral.Bool { literal_token; _ }; _ } ->
      if String.equal (Syn.Cst.Token.text literal_token) "true" then
        TruePattern
      else
        FalsePattern
  | Syn.Cst.Pattern.Wildcard _ -> WildcardPattern
  | Syn.Cst.Pattern.Parenthesized { inner; _ } -> case_pattern_kind inner
  | Syn.Cst.Pattern.Identifier _
  | Syn.Cst.Pattern.Literal _ -> OtherPattern
  | Syn.Cst.Pattern.Extension _
  | Syn.Cst.Pattern.Lazy _
  | Syn.Cst.Pattern.Exception _
  | Syn.Cst.Pattern.Range _
  | Syn.Cst.Pattern.Operator _
  | Syn.Cst.Pattern.FirstClassModule _
  | Syn.Cst.Pattern.PolyVariant _
  | Syn.Cst.Pattern.PolyVariantInherit _
  | Syn.Cst.Pattern.Constructor _
  | Syn.Cst.Pattern.Tuple _
  | Syn.Cst.Pattern.List _
  | Syn.Cst.Pattern.Array _
  | Syn.Cst.Pattern.Record _
  | Syn.Cst.Pattern.Cons _
  | Syn.Cst.Pattern.Or _
  | Syn.Cst.Pattern.Alias _
  | Syn.Cst.Pattern.Typed _
  | Syn.Cst.Pattern.Effect _
  | Syn.Cst.Pattern.LocalOpen _ -> OtherPattern

let suggestion_for_match = fun (expr: Syn.Cst.match_expression) ->
  match expr.cases with
  | [first_case;second_case] -> (
      match case_pattern_kind first_case.pattern, case_pattern_kind second_case.pattern with
      | TruePattern, FalsePattern -> "Rewrite this match as `if <condition> then ... else ...`."
      | FalsePattern, TruePattern -> "Rewrite this match as `if not <condition> then ... else ...`."
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
      | (WildcardPattern, TruePattern)
      | (TruePattern, TruePattern)
      | (WildcardPattern, FalsePattern)
      | (FalsePattern, FalsePattern)
      | (WildcardPattern, WildcardPattern)
      | (TruePattern, OtherPattern)
      | (FalsePattern, OtherPattern)
      | (WildcardPattern, OtherPattern)
      | (OtherPattern, _) -> "Rewrite this boolean match as an `if` expression."
    )
  | _ -> "Rewrite this boolean match as an `if` expression."

let if_text = fun ~condition ~then_branch ?else_branch () ->
  match else_branch with
  | Some else_branch -> "if " ^ condition ^ " then " ^ then_branch ^ " else " ^ else_branch
  | None -> "if " ^ condition ^ " then " ^ then_branch

let source_slice = fun ~source span ->
  let len = Syn.Ceibo.Span.(span.end_ - span.start) in
  String.sub source ~offset:span.start ~len

let source_of_node_without_outer_trivia = fun ~source node ->
  let tokens =
    Traversal.find_tokens
      (fun token -> not (Traversal.is_trivia (Syn.Ceibo.Red.SyntaxToken.kind token)))
      node
  in
  match tokens with
  | [] -> source_slice ~source (Syn.Ceibo.Red.SyntaxNode.span node)
  | first :: rest ->
      let last =
        match List.reverse rest with
        | last :: _ -> last
        | [] -> first
      in
      let start = (Syn.Ceibo.Red.SyntaxToken.span first).start in
      let end_ = (Syn.Ceibo.Red.SyntaxToken.span last).end_ in
      source_slice ~source (Syn.Ceibo.Span.make ~start ~end_)

let expression_source = fun ~source expr ->
  source_of_node_without_outer_trivia ~source (Syn.Cst.Expression.syntax_node expr)

let fix_text_for_match = fun ~source ->
  fun (expr: Syn.Cst.match_expression) ->
    match expr.cases with
    | [first_case;second_case] ->
        let scrutinee = expression_source ~source expr.scrutinee in
        let negated_scrutinee = "not (" ^ scrutinee ^ ")" in
        let first_body = expression_source ~source first_case.body in
        let second_body = expression_source ~source second_case.body in
        (
          match case_pattern_kind first_case.pattern, case_pattern_kind second_case.pattern with
          | TruePattern, FalsePattern -> Some (if_text
            ~condition:scrutinee
            ~then_branch:first_body
            ~else_branch:second_body
            ())
          | FalsePattern, TruePattern -> Some (if_text
            ~condition:negated_scrutinee
            ~then_branch:first_body
            ~else_branch:second_body
            ())
          | TruePattern, WildcardPattern ->
              if is_unit_expression second_case.body then
                Some (if_text ~condition:scrutinee ~then_branch:first_body ())
              else
                Some (if_text ~condition:scrutinee ~then_branch:first_body ~else_branch:second_body ())
          | FalsePattern, WildcardPattern ->
              if is_unit_expression second_case.body then
                Some (if_text ~condition:negated_scrutinee ~then_branch:first_body ())
              else
                Some (if_text
                  ~condition:negated_scrutinee
                  ~then_branch:first_body
                  ~else_branch:second_body
                  ())
          | _ -> None
        )
    | _ -> None

let make_fix = fun ~source ->
  fun (expr: Syn.Cst.match_expression) ->
    match fix_text_for_match ~source expr with
    | None -> None
    | Some text -> Some (Fix.make
      ~title:"Rewrite boolean match as an if expression"
      ~operations:[ Fix.replace_node_with_text ~target:expr.syntax_node ~text; ])

let should_flag_match = fun (expr: Syn.Cst.match_expression) ->
  match expr.cases with
  | [first_case;second_case] ->
      first_case.guard = None && second_case.guard = None && (
        match case_pattern_kind first_case.pattern, case_pattern_kind second_case.pattern with
        | (TruePattern, FalsePattern)
        | (FalsePattern, TruePattern)
        | (TruePattern, WildcardPattern)
        | (FalsePattern, WildcardPattern) -> true
        | (WildcardPattern, _)
        | (OtherPattern, _)
        | (_, OtherPattern)
        | _ -> false
      )
  | _ -> false

let make_diagnostic = fun ~source ->
  fun (expr: Syn.Cst.match_expression) ->
    let fix = make_fix ~source expr in
    Diagnostic.make
      ~severity:Warning
      ~kind:(Diagnostic.Known { rule_id; message = rule_description })
      ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
      ~suggestion:(suggestion_for_match expr)
      ?fix
      ()

let safe_should_flag_match = fun expr ->
  try should_flag_match expr with
  | Match_failure _ -> false

let diagnostic_for_expression = fun ~source ->
  function
  | Syn.Cst.Expression.Match expr when safe_should_flag_match expr -> Some (make_diagnostic ~source expr)
  | _ -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.map ~fn:Traversal.expressions_of_structure_item
  |> List.concat
  |> List.filter_map ~fn:(diagnostic_for_expression ~source:ctx.source)

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
