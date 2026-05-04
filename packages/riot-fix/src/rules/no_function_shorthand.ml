open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-function-shorthand"

let rule_description = "Prefer explicit function parameters over `function` shorthand"

let rule_explain =
  {|
The `function` shorthand hides the value being matched. Explicit parameters make
the flow easier to name, annotate, and extend.

Prefer `fun value -> match value with ...` over `function ...`.
|}

let function_keyword = "function"

let replacement_text = fun ctx expr ->
  let source =
    H.node_source ctx (Ast.Expr.as_node expr)
    |> String.trim
  in
  if String.starts_with ~prefix:function_keyword source then
    let cases =
      String.sub
        source
        ~offset:(String.length function_keyword)
        ~len:(String.length source - String.length function_keyword)
      |> String.trim
    in
    Some ("fun value -> match value with " ^ cases)
  else
    None

let make_diagnostic = fun ctx expr replacement ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Expr.as_node expr))
    ~suggestion:"Introduce an explicit parameter and match on it."
    ~fix:(Fix.make
      ~title:"Expand function shorthand"
      ~operations:[ Fix.replace_node_with_text ~target:(Ast.Expr.as_node expr) ~text:replacement; ])
    ()

let diagnostic_for_expr = fun ctx expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Fun { body = Ast.Expr.Body_cases _; _ } -> (
      match replacement_text ctx expr with
      | Some replacement -> Some (make_diagnostic ctx expr replacement)
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
