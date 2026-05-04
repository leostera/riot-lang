open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-custom-operators"

let rule_description = "Custom infix operators should be avoided in favor of named functions"

let rule_explain =
  {|
Custom symbolic operators trade a small amount of local brevity for a lot of global
guesswork. Unless the operator is already conventional, readers have to stop and
remember what `%>`, `>>?`, or `<+>` means before they can follow the expression.

They are also unpleasant to search for, awkward to discuss in review, and easy to
confuse with similar punctuation from other libraries.

A named function usually tells the story directly. `compose_right value next` may be
longer than `value %> next`, but it is obvious to readers who have never seen the
code before.
|}

let allowed_infix_operators = [
  "=";
  "<>";
  "!=";
  "<";
  ">";
  "<=";
  ">=";
  "&&";
  "||";
  "+";
  "-";
  "*";
  "/";
  "+.";
  "-.";
  "*.";
  "/.";
  "mod";
  "land";
  "lor";
  "lxor";
  "lsl";
  "lsr";
  "asr";
  "::";
  "@";
  "^";
  "|>";
  "@@";
]

let should_flag_operator = fun operator ->
  not
    (List.contains allowed_infix_operators ~value:operator)

let make_diagnostic = fun token ->
  let operator = Ast.Token.text token in
  H.diagnostic_for_token
    ~rule_id
    ~message:rule_description
    ~token
    ~suggestion:("Replace " ^ operator ^ " with a named function")
    ()

let rec diagnostics_for_expression = fun diagnostics expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Infix { operator; _ } ->
      if should_flag_operator (Ast.Token.text operator) then
        H.push_diagnostic diagnostics (make_diagnostic operator);
      H.iter_fold Ast.Expr.fold_child_expr expr ~fn:(diagnostics_for_expression diagnostics)
  | _ -> H.iter_fold Ast.Expr.fold_child_expr expr ~fn:(diagnostics_for_expression diagnostics)

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_expr =
      Some (fun visitor expr ->
        diagnostics_for_expression diagnostics expr;
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
