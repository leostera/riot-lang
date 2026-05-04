open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "limit-parenthesis-depth"

let rule_description = "Deep chains of parenthesized expressions should be avoided"

let rule_explain =
  {|
Heavy parenthesization is usually a sign that the code wants to be flatter.
When readers have to count closing delimiters to understand an expression, the shape
of the program is doing more work than the names inside it.

Sometimes the right fix is simply to remove redundant grouping. Other times the
expression wants an intermediate name, a helper function, or a pipeline that makes
evaluation order obvious without so much punctuation.

If a line keeps growing more parentheses just to stay understandable, it is already
telling you that the current shape is too dense.
|}

let max_parenthesis_depth = 5

let make_diagnostic = fun expr depth ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node expr)
    ~suggestion:("Reduce parenthesis depth from "
    ^ Int.to_string depth
    ^ " by removing redundant grouping or extracting a named value")
    ()

let raw_first_child_expr = fun expr ->
  let found = ref None in
  H.iter_fold
    Ast.Node.fold_child_node
    expr
    ~fn:(fun child ->
      match !found with
      | Some _ -> ()
      | None ->
          if Option.is_some (Ast.cast_result_to_option (Ast.Expr.cast child)) then
            found := Some child);
  !found

let raw_for_each_child_expr = fun expr ~fn ->
  H.iter_fold
    Ast.Node.fold_child_node
    expr
    ~fn:(fun child ->
      if Option.is_some (Ast.cast_result_to_option (Ast.Expr.cast child)) then
        fn child)

let rec parenthesis_chain_depth = fun expr ->
  if Syn.SyntaxKind.(Ast.Node.kind expr = PAREN_EXPR) then
    match raw_first_child_expr expr with
    | Some inner -> 1 + parenthesis_chain_depth inner
    | None -> 0
  else
    0

let rec diagnostics_for_expression = fun diagnostics expr ->
  if Syn.SyntaxKind.(Ast.Node.kind expr = PAREN_EXPR) then
    match raw_first_child_expr expr with
    | Some inner ->
        let depth = parenthesis_chain_depth expr in
        let inner_depth = parenthesis_chain_depth inner in
        if depth >= max_parenthesis_depth && inner_depth < max_parenthesis_depth then
          H.push_diagnostic diagnostics (make_diagnostic expr depth);
        diagnostics_for_expression diagnostics inner
    | None -> ()
  else
    raw_for_each_child_expr expr ~fn:(diagnostics_for_expression diagnostics)

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_expr =
      Some (fun visitor expr ->
        diagnostics_for_expression diagnostics (Ast.Expr.as_node expr);
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
