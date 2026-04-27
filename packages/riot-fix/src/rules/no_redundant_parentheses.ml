open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.of_string "no-redundant-parentheses"

let rule_description = "Obvious grouping parentheses should be removed"

let rule_explain =
  {|
Parentheses are valuable when they explain precedence or mark a meaningful grouping.
When they only wrap a single identifier, literal, or another parenthesized expression,
they stop clarifying anything and start adding visual noise.

This rule only targets the cases where the grouping is already obvious. It is not a
general attack on parentheses. Keep them when they disambiguate an expression, but do
not leave them around as punctuation residue once they stop doing real work.
|}

let opens_with_begin = fun expr ->
  Ast.Node.first_child_token expr ~kind:Syn.SyntaxKind.BEGIN_KW
  |> Option.is_some

let is_obviously_redundant = fun expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Path _
  | Ast.Expr.Literal _
  | Ast.Expr.Parenthesized _ -> true
  | _ -> false

let make_diagnostic = fun expr inner ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node expr)
    ~suggestion:"Remove these redundant parentheses."
    ~fix:(Fix.make
      ~title:"Remove redundant parentheses"
      ~operations:[ Fix.replace_node ~target:expr ~replacement:inner; ])
    ()

let rec diagnostics_for_expression = fun diagnostics ~inside_redundant_chain expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Parenthesized { inner = Some inner } ->
      let redundant = is_obviously_redundant inner in
      if redundant && not inside_redundant_chain && not (opens_with_begin expr) then
        H.push_diagnostic diagnostics (make_diagnostic expr inner);
      diagnostics_for_expression
        diagnostics
        ~inside_redundant_chain:(inside_redundant_chain || redundant)
        inner
  | _ ->
      Ast.Expr.for_each_child_expr
        expr
        ~fn:(diagnostics_for_expression diagnostics ~inside_redundant_chain:false)

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks =
    {
      Syn.Visitor.empty_hooks with
      enter_expr =
        Some (fun visitor expr ->
          diagnostics_for_expression diagnostics ~inside_redundant_chain:false expr;
          (visitor, Syn.Visitor.Skip_subtree));
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
