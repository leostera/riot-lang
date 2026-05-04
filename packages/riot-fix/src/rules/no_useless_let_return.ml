open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-useless-let-return"

let rule_description = "Bindings that immediately return the bound name should be collapsed"

let rule_explain =
  {|
A local `let` binding should make code clearer, share a value, or introduce a
useful name. When the body immediately returns the same binding, the name is just
a detour.

Prefer `compute ()` over `let value = compute () in value` unless the temporary
name adds real meaning in the surrounding code.
|}

let rec unwrap_parens = fun expr -> H.unwrap_expr expr

let expr_path_name = fun expr ->
  match Ast.Expr.view (unwrap_parens expr) with
  | Ast.Expr.Ident { ident } -> (
      match Ast.Ident.last_segment ident with
      | Some token -> Some (Ast.Token.text token)
      | None -> None
    )
  | _ -> None

let binding_has_no_parameters = fun binding -> not (H.binding_has_parameters binding)

let make_fix = fun expr binding ->
  match Ast.LetBinding.body binding with
  | Some bound_value ->
      Some (Fix.make
        ~title:"Replace this let-binding with its bound expression"
        ~operations:[
          Fix.replace_node
            ~target:(Ast.Expr.as_node expr)
            ~replacement:(Ast.Expr.as_node bound_value);
        ])
  | None -> None

let make_diagnostic = fun expr binding ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Expr.as_node expr))
    ~suggestion:"Replace this let-binding with its bound expression."
    ?fix:(make_fix expr binding)
    ()

let diagnostic_for_expr = fun expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Let { first_binding = binding; body } when binding_has_no_parameters binding -> (
      match (H.binding_name_token binding, expr_path_name body) with
      | (Some token, Some returned_name) when String.equal (Ast.Token.text token) returned_name ->
          Some (make_diagnostic expr binding)
      | _ -> None
    )
  | _ -> None

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_expr =
      Some (fun visitor expr ->
        (
          match diagnostic_for_expr expr with
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
