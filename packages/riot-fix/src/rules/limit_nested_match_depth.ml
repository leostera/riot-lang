open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "limit-nested-match-depth"

let rule_description = "Deep towers of nested match expressions should be flattened"

let rule_explain =
  {|
More than three layers of nested `match` usually means the control flow has stopped
fitting in the reader's head. Each additional `match` introduces another indentation level,
another set of cases, and another question about which values are still in scope.

When a branch has to open yet another `match`, the code often wants one of three
things instead: a helper function, a smaller intermediate value, or a different data
shape that captures the combination up front.

As a rough rule, once you are writing a fourth nested `match`, stop and ask whether
the innermost logic deserves a name of its own.
|}

let max_nested_match_depth = 3

let make_diagnostic = fun expr depth ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Expr.as_node expr))
    ~suggestion:("Reduce nested match depth from "
    ^ Int.to_string depth
    ^ " by extracting a helper or flattening the control flow.")
    ()

let for_each_case_expr = fun expr ~fn ->
  H.iter_fold
    Ast.Expr.fold_match_case
    expr
    ~fn:(fun match_case ->
      match Ast.MatchCase.view match_case with
      | Ast.MatchCase.Case { guard; body; _ } ->
          Option.for_each guard ~fn;
          fn body
      | Ast.MatchCase.Unknown _ -> ())

let for_each_nested_expr = fun expr ~fn ->
  H.iter_fold Ast.Expr.fold_child_expr expr ~fn;
  for_each_case_expr expr ~fn

let rec max_child_depth = fun expr ->
  let depth = ref 0 in
  for_each_nested_expr expr ~fn:(fun child -> depth := Int.max !depth (match_chain_depth child));
  !depth

and match_chain_depth = fun expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Match _ -> 1 + max_child_depth expr
  | _ -> max_child_depth expr

let rec diagnostics_for_expression = fun diagnostics ~inside_match expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Match _ ->
      for_each_nested_expr expr ~fn:(diagnostics_for_expression diagnostics ~inside_match:true);
      if not inside_match then
        let depth = match_chain_depth expr in
        if depth > max_nested_match_depth then
          H.push_diagnostic diagnostics (make_diagnostic expr depth)
  | _ -> for_each_nested_expr expr ~fn:(diagnostics_for_expression diagnostics ~inside_match)

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_expr =
      Some (fun visitor expr ->
        diagnostics_for_expression diagnostics ~inside_match:false expr;
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
