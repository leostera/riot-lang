open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-redundant-else-unit"

let rule_description = "Else branches that only return unit should be removed"

let rule_explain =
  {|
An `else ()` branch says the same thing as omitting the branch: nothing happens
when the condition is false.

Prefer `if ok then render ()` over `if ok then render () else ()` so the useful
branch is the only branch the reader has to scan.
|}

let expr_source = fun ctx expr ->
  H.node_source ctx (Ast.Expr.as_node expr)
  |> String.trim

let is_unit_expr = fun ctx expr -> String.equal (expr_source ctx expr) "()"

let replacement_text = fun ctx condition then_branch ->
  "if " ^ expr_source ctx condition ^ " then " ^ expr_source ctx then_branch

let make_diagnostic = fun ctx expr condition then_branch ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Expr.as_node expr))
    ~suggestion:"Drop the `else ()` branch."
    ~fix:(Fix.make
      ~title:"Remove redundant else unit branch"
      ~operations:[
        Fix.replace_node_with_text
          ~target:(Ast.Expr.as_node expr)
          ~text:(replacement_text ctx condition then_branch);
      ])
    ()

let diagnostic_for_expr = fun ctx expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.If { condition; then_branch; else_branch = Some else_branch } when is_unit_expr
    ctx
    else_branch -> Some (make_diagnostic ctx expr condition then_branch)
  | _ -> None

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_expr =
      Some (fun visitor expr ->
        (
          match diagnostic_for_expr ctx expr with
          | Some diagnostic -> H.push_diagnostic diagnostics diagnostic
          | None -> ()
        );
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
