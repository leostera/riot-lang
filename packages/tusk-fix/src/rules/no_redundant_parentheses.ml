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
  | Syn.Cst.Expression.Literal _ ->
      []
  | Syn.Cst.Expression.Apply expr ->
      expr.callee
      ::
      (match expr.argument with
      | Syn.Cst.Positional argument -> [ argument ]
      | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } ->
          Option.to_list value)
  | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
      [ receiver ]
  | Syn.Cst.Expression.Infix expr ->
      [ Syn.Cst.InfixExpression.left expr; Syn.Cst.InfixExpression.right expr ]
  | Syn.Cst.Expression.Fun expr ->
      [ expr.body ]
  | Syn.Cst.Expression.Function expr ->
      expr.cases
      |> List.concat_map (fun (case : Syn.Cst.match_case) ->
             (match case.guard with
             | Some guard -> [ guard ]
             | None -> [])
             @ [ case.body ])
  | Syn.Cst.Expression.Let expr ->
      [ expr.bound_value; expr.body ]
  | Syn.Cst.Expression.Match expr ->
      expr.scrutinee
      :: (expr.cases
         |> List.concat_map (fun (case : Syn.Cst.match_case) ->
                (match case.guard with
                | Some guard -> [ guard ]
                | None -> [])
                @ [ case.body ]))
  | Syn.Cst.Expression.Try expr ->
      expr.body
      :: (expr.cases
         |> List.concat_map (fun (case : Syn.Cst.match_case) ->
                (match case.guard with
                | Some guard -> [ guard ]
                | None -> [])
                @ [ case.body ]))
  | Syn.Cst.Expression.If expr ->
      let base = [ expr.condition; expr.then_branch ] in
      (match expr.else_branch with
      | Some else_branch -> base @ [ else_branch ]
      | None -> base)
  | Syn.Cst.Expression.Parenthesized expr ->
      [ expr.inner ]
  | _ ->
      []

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
  | Syn.Cst.Expression.FieldAccess _
  | Syn.Cst.Expression.Infix _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.If _ ->
      false
  | _ ->
      false

let make_diagnostic (expr : Syn.Cst.parenthesized_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(expr.syntax_node |> Syn.Ceibo.Red.SyntaxNode.span)
    ~suggestion:"Remove these redundant parentheses."
    ()

let rec diagnostics_for_expression ~inside_redundant_chain = function
  | Syn.Cst.Expression.Parenthesized expr ->
      let inner = expr.inner in
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
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.let_bindings_of_structure_item
      |> List.concat_map (fun binding ->
             diagnostics_for_expression ~inside_redundant_chain:false
               (Syn.Cst.LetBinding.value binding))

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
