open Std

let rule_id = "no-redundant-else-unit"
let rule_name = "No Redundant Else Unit"
let rule_code = "F0128"

let rule_description =
  "Else branches that only return unit should be omitted"

let rule_message =
  "Remove else () from if expressions whose else branch does nothing."

let rule_explain =
  {|
Else branches that only return unit should usually be omitted.

Why this rule exists:
- `if cond then expr` already means “do this effect conditionally”.
- Adding `else ()` says the same thing more noisily.
- Keeping the shorter form makes control flow easier to scan.

Examples:
  Bad:    if is_ready then render () else ()
  Better: if is_ready then render ()
|}

let rec is_unit_expression = function
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.Unit _) -> true
  | Syn.Cst.Expression.Parenthesized expr ->
      is_unit_expression (Syn.Cst.ParenthesizedExpression.inner expr)
  | _ -> false

let make_diagnostic expr =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.IfExpression.syntax_node expr))
    ~suggestion:"Remove else () from this if expression."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.If expr -> (
      match Syn.Cst.IfExpression.else_branch expr with
      | Some else_branch when is_unit_expression else_branch ->
          Some (make_diagnostic expr)
      | _ -> None)
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
