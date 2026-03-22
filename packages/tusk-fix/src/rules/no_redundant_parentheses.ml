open Std

let rule_id = "no-redundant-parentheses"
let rule_name = "No Redundant Parentheses"
let rule_code = "F0136"

let rule_description =
  "Obvious grouping parentheses should be removed"

let rule_message =
  "Obvious grouping parentheses should be removed."

let rule_explain =
  {|
Avoid obvious grouping parentheses.

Parentheses around a single identifier, literal, or another parenthesized expression do not add information.
They make the expression visually heavier without clarifying precedence or evaluation order.
Keep parentheses when they actually disambiguate the expression, but drop them when the grouping is already obvious.

Examples:
  Avoid:   let value = (result)
  Avoid:   let value = ((result))
  Better:  let value = result
|}

let rec child_expressions = function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Unknown _ ->
      []
  | Syn.Cst.Expression.Apply expr ->
      [ Syn.Cst.ApplyExpression.callee expr; Syn.Cst.ApplyExpression.argument expr ]
  | Syn.Cst.Expression.Infix expr ->
      [ Syn.Cst.InfixExpression.left expr; Syn.Cst.InfixExpression.right expr ]
  | Syn.Cst.Expression.Fun expr ->
      [ Syn.Cst.FunExpression.body expr ]
  | Syn.Cst.Expression.Function expr ->
      Syn.Cst.FunctionExpression.cases expr
      |> List.concat_map (fun case ->
             (match Syn.Cst.MatchCase.guard case with
             | Some guard -> [ guard ]
             | None -> [])
             @ [ Syn.Cst.MatchCase.body case ])
  | Syn.Cst.Expression.Let expr ->
      [ Syn.Cst.LetExpression.bound_value expr; Syn.Cst.LetExpression.body expr ]
  | Syn.Cst.Expression.Match expr ->
      Syn.Cst.MatchExpression.scrutinee expr
      :: (Syn.Cst.MatchExpression.cases expr
         |> List.concat_map (fun case ->
                (match Syn.Cst.MatchCase.guard case with
                | Some guard -> [ guard ]
                | None -> [])
                @ [ Syn.Cst.MatchCase.body case ]))
  | Syn.Cst.Expression.Try expr ->
      Syn.Cst.TryExpression.body expr
      :: (Syn.Cst.TryExpression.cases expr
         |> List.concat_map (fun case ->
                (match Syn.Cst.MatchCase.guard case with
                | Some guard -> [ guard ]
                | None -> [])
                @ [ Syn.Cst.MatchCase.body case ]))
  | Syn.Cst.Expression.If expr ->
      let base =
        [ Syn.Cst.IfExpression.condition expr; Syn.Cst.IfExpression.then_branch expr ]
      in
      (match Syn.Cst.IfExpression.else_branch expr with
      | Some else_branch -> base @ [ else_branch ]
      | None -> base)
  | Syn.Cst.Expression.Parenthesized expr ->
      [ Syn.Cst.ParenthesizedExpression.inner expr ]

let opens_with_begin ({ syntax_node; _ } : Syn.Cst.parenthesized_expression) =
  Syn.Ceibo.Red.SyntaxNode.children syntax_node
  |> Std.Collections.Array.to_list
  |> List.find_map (function
         | Syn.Ceibo.Red.Token token ->
             let text = Syn.Ceibo.Red.SyntaxToken.text token in
             if String.equal text " " || String.equal text "\n" || String.equal text "\t" then
               None
             else
               Some (String.equal text "begin")
         | _ -> None)
  |> Option.unwrap_or ~default:false

let is_obviously_redundant = function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Parenthesized _ ->
      true
  | Syn.Cst.Expression.Apply _
  | Syn.Cst.Expression.Infix _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Unknown _ ->
      false

let make_diagnostic expr =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:
      (Syn.Cst.ParenthesizedExpression.syntax_node expr
      |> Syn.Ceibo.Red.SyntaxNode.span)
    ~suggestion:"Remove these redundant parentheses."
    ()

let rec diagnostics_for_expression ~inside_redundant_chain = function
  | Syn.Cst.Expression.Parenthesized expr ->
      let inner = Syn.Cst.ParenthesizedExpression.inner expr in
      let nested =
        diagnostics_for_expression
          ~inside_redundant_chain:(inside_redundant_chain || is_obviously_redundant inner)
          inner
      in
      if opens_with_begin expr then
        nested
      else if is_obviously_redundant inner && not inside_redundant_chain then
        make_diagnostic expr :: nested
      else
        nested
  | expr ->
      child_expressions expr
      |> List.concat_map (diagnostics_for_expression ~inside_redundant_chain:false)

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.let_bindings source_file
      |> List.concat_map (fun binding ->
             diagnostics_for_expression ~inside_redundant_chain:false
               (Syn.Cst.LetBinding.value binding))

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
