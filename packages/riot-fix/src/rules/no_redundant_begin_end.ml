open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-redundant-begin-end"

let rule_description = "begin/end blocks should be replaced by ordinary grouping or removed"

let rule_explain =
  {|
`begin ... end` is a perfectly valid grouping construct, but for ordinary expression
grouping it is usually heavier than necessary. Most readers parse parentheses faster
than `begin` and `end`, especially when the grouped expression is short.

If the block exists only to force grouping, prefer plain parentheses or remove the
grouping entirely when precedence already makes the expression obvious.

This keeps the visual weight of the code proportional to the job the grouping is
actually doing.
|}

let opens_with_begin = fun expr ->
  Ast.Node.first_child_token expr ~kind:Syn.SyntaxKind.BEGIN_KW
  |> Option.is_some

let make_diagnostic = fun expr inner ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node expr)
    ~suggestion:"Replace begin/end with ordinary grouping or remove it entirely."
    ~fix:(Fix.make
      ~title:"Replace begin/end with ordinary grouping"
      ~operations:[ Fix.replace_node ~target:expr ~replacement:inner; ])
    ()

let check_expression = fun diagnostics expr ->
  let expr_node = Ast.Expr.as_node expr in
  if Syn.SyntaxKind.(Ast.Node.kind expr_node = PAREN_EXPR) && opens_with_begin expr_node then
    match H.first_child_expr expr with
    | Some inner ->
        H.push_diagnostic diagnostics (make_diagnostic expr_node (Ast.Expr.as_node inner))
    | None -> ()

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_expr =
      Some (fun visitor expr ->
        check_expression diagnostics expr;
        (visitor, Syn.Visitor.Continue));
  }
  in
  Syn.Visitor.make ~ctx:() ~hooks
  |> fun visitor ->
    ignore (Syn.Visitor.visit_node visitor root);
    H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
