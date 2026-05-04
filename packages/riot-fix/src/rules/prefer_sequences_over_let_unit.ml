open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "prefer-sequences-over-let-unit"

let rule_description = "Use sequences instead of `let () = ... in ...`"

let rule_explain =
  {|
`let () = effect () in next ()` adds a binding that cannot carry information.
When the left side is unit, the expression is really a sequence.

Prefer `effect (); next ()`. Keep `let` for names that are reused later.
|}

let node_source = fun ctx node ->
  H.node_source ctx node
  |> String.trim

let is_unit_pattern = fun ctx pattern ->
  String.equal
    (node_source ctx (Ast.Pattern.as_node pattern))
    "()"

let indentation_at = fun source offset ->
  let rec find_line_start index =
    if index <= 0 then
      0
    else
      match String.get source ~at:(index - 1) with
      | Some '\n' -> index
      | Some _
      | None -> find_line_start (index - 1)
  in
  let line_start = find_line_start offset in
  String.make ~len:(offset - line_start) ~char:' '

let separator_between = fun ctx left right ->
  let stop = Ast.Node.span_end left in
  let start = Ast.Node.span_start right in
  if start <= stop then
    ""
  else
    H.source_span ctx.Rule.source stop start

let replacement_text = fun ctx expr bound_value body ->
  let expr_node = Ast.Expr.as_node expr in
  let bound_node = Ast.Expr.as_node bound_value in
  let body_node = Ast.Expr.as_node body in
  let bound_source = node_source ctx bound_node in
  let body_source = node_source ctx body_node in
  let between = separator_between ctx bound_node body_node in
  let separator =
    if String.contains between "\n" then
      "\n" ^ indentation_at ctx.Rule.source (Ast.Node.span_start expr_node)
    else
      " "
  in
  "(" ^ bound_source ^ ");" ^ separator ^ body_source

let make_diagnostic = fun ctx expr binding body ->
  match Ast.LetBinding.body binding with
  | Some bound_value ->
      Some (H.diagnostic
        ~rule_id
        ~message:rule_description
        ~span:(H.span_of_node (Ast.Expr.as_node expr))
        ~suggestion:"Rewrite the unit binding as a sequence."
        ~fix:(Fix.make
          ~title:"Rewrite unit let-binding as a sequence"
          ~operations:[
            Fix.replace_node_with_text
              ~target:(Ast.Expr.as_node expr)
              ~text:(replacement_text ctx expr bound_value body);
          ])
        ())
  | None -> None

let diagnostic_for_expr = fun ctx expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Let { first_binding = binding; body } -> (
      match Ast.LetBinding.pattern binding with
      | Some pattern when is_unit_pattern ctx pattern -> make_diagnostic ctx expr binding body
      | _ -> None
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
