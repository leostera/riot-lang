open Std

let rule_id = "prefer-sequences-over-let-unit"
let rule_description =
  "Effectful let-unit bindings should be written as `;` sequences"

let rule_explain =
  {|
`let () = side_effect () in next ()` is usually just sequencing spelled in a heavier
way. The unit pattern is not carrying information there; it is only forcing the reader
to parse a `let` where a plain sequence would have said the same thing more directly.

`side_effect (); next ()` makes the flow obvious immediately: do this, then do that.
Reserve `let () = ... in ...` for the places where binding the unit result is part of
the point, not for ordinary effect sequencing.

This rule exists because unit-binding syntax can make straightforward imperative steps
look more abstract than they really are.
|}

let rec is_unit_pattern = function
  | Syn.Cst.Pattern.Literal { literal = Syn.Cst.PatternLiteral.Unit _; _ } -> true
  | Syn.Cst.Pattern.Parenthesized { inner; _ } -> is_unit_pattern inner
  | Syn.Cst.Pattern.Identifier _ | Syn.Cst.Pattern.Wildcard _
  | Syn.Cst.Pattern.Literal _ ->
      false
  | _ ->
      false

let make_diagnostic (expr : Syn.Cst.let_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
    ~suggestion:"Replace this let-unit binding with a `;` sequence."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Let expr
    when is_unit_pattern expr.binding_pattern ->
      Some (make_diagnostic expr)
  | _ -> None

let check_tree (ctx : Rule.context) _red_root =
  Rule_query.expressions ctx
  |> List.filter_map diagnostic_for_expression

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
