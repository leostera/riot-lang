open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-boolean-comparisons-in-conditionals"

let rule_description = "Boolean conditions should not compare directly with true or false"

let rule_explain =
  {|
Boolean comparisons inside conditionals add a second boolean expression around the
one you already have.

Prefer `if is_ready then render ()` over `if is_ready = true then render ()`, and
prefer `if not is_ready then render ()` over `if is_ready = false then render ()`.
|}

let rec unwrap_expr = fun expr -> H.unwrap_expr expr

let bool_literal = fun expr ->
  match Ast.Expr.view (unwrap_expr expr) with
  | Ast.Expr.Literal { token } -> (
      match Ast.Token.text token with
      | "true" -> Some true
      | "false" -> Some false
      | _ -> None
    )
  | Ast.Expr.Ident { ident } -> (
      match Ast.Ident.text ident with
      | "true" -> Some true
      | "false" -> Some false
      | _ -> None
    )
  | _ -> None

let comparison_operator = fun token ->
  match Ast.Token.text token with
  | "=" -> Some true
  | "!="
  | "<>" -> Some false
  | _ -> None

let replacement_text = fun ctx ~operator_is_equal ~bool_value operand ->
  let operand_text =
    H.node_source ctx (Ast.Expr.as_node operand)
    |> String.trim
  in
  let keep_direct =
    if operator_is_equal then
      bool_value
    else
      not bool_value
  in
  if keep_direct then
    operand_text
  else
    "not (" ^ operand_text ^ ")"

let comparison_replacement = fun ctx condition ->
  match Ast.Expr.view (unwrap_expr condition) with
  | Ast.Expr.Infix { left; operator; right } -> (
      match comparison_operator operator with
      | None -> None
      | Some operator_is_equal -> (
          match (bool_literal left, bool_literal right) with
          | (Some bool_value, None) ->
              Some (replacement_text ctx ~operator_is_equal ~bool_value right)
          | (None, Some bool_value) ->
              Some (replacement_text ctx ~operator_is_equal ~bool_value left)
          | _ -> None
        )
    )
  | _ -> None

let make_diagnostic = fun ctx condition replacement ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Expr.as_node condition))
    ~suggestion:"Use the boolean expression directly in the conditional."
    ~fix:(Fix.make
      ~title:"Remove boolean comparison"
      ~operations:[
        Fix.replace_node_with_text ~target:(Ast.Expr.as_node condition) ~text:replacement;
      ])
    ()

let diagnostic_for_expr = fun ctx expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.If { condition; _ } -> (
      match comparison_replacement ctx condition with
      | Some replacement -> Some (make_diagnostic ctx condition replacement)
      | None -> None
    )
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
