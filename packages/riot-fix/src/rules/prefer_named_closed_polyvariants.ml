open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "prefer-named-closed-polyvariants"

let rule_description = "Closed polyvariants should be named"

let rule_explain =
  {|
Inline closed polyvariant rows make signatures harder to scan and harder to reuse.

Prefer naming the row once with a type alias, then reference that alias from values,
containers, and larger type expressions.
|}

let rec unwrap_type = fun type_expr -> H.unwrap_type_expr type_expr

let is_closed_polyvariant_type = fun ctx type_expr ->
  let text =
    H.node_source ctx (Ast.TypeExpr.as_node (unwrap_type type_expr))
    |> String.trim
  in
  String.starts_with ~prefix:"[" text
  && not (String.starts_with ~prefix:"[>" text)
  && not (String.starts_with ~prefix:"[<" text)
  && String.ends_with ~suffix:"]" text
  && String.contains text "`"

let diagnostic_for_type = fun type_expr ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.TypeExpr.as_node type_expr))
    ~suggestion:"Move the closed polyvariant row behind a named type alias."
    ()

let rec check_type_expr = fun ctx diagnostics ~allow_named_alias_root type_expr ->
  if is_closed_polyvariant_type ctx type_expr then
    if not allow_named_alias_root then
      H.push_diagnostic diagnostics (diagnostic_for_type type_expr)
    else
      ()
  else
    match Ast.TypeExpr.view type_expr with
    | Ast.TypeExpr.Arrow { arg; ret; _ } ->
        check_type_expr ctx diagnostics ~allow_named_alias_root:false arg;
        check_type_expr ctx diagnostics ~allow_named_alias_root:false ret
    | Ast.TypeExpr.Forall { body; _ } ->
        check_type_expr ctx diagnostics ~allow_named_alias_root:false body
    | Ast.TypeExpr.Alias { typ; _ } ->
        check_type_expr ctx diagnostics ~allow_named_alias_root:false typ
    | Ast.TypeExpr.Tuple { parts } ->
        Vector.for_each parts ~fn:(check_type_expr ctx diagnostics ~allow_named_alias_root:false)
    | Ast.TypeExpr.Apply { args; _ } ->
        Vector.for_each args ~fn:(check_type_expr ctx diagnostics ~allow_named_alias_root:false)
    | Ast.TypeExpr.Error node
    | Ast.TypeExpr.Unknown node ->
        H.iter_fold
          Ast.Node.fold_child_node
          node
          ~fn:(fun node ->
            match Ast.cast_result_to_option (Ast.TypeExpr.cast node) with
            | Some type_expr ->
                check_type_expr ctx diagnostics ~allow_named_alias_root:false type_expr
            | None -> ())
    | Ast.TypeExpr.Ident _
    | Ast.TypeExpr.Var _
    | Ast.TypeExpr.Wildcard -> ()

let check_type_declaration = fun ctx diagnostics declaration ->
  H.iter_fold
    Ast.TypeDeclaration.fold_member
    declaration
    ~fn:(fun member ->
      Option.for_each
        (Ast.TypeDeclaration.Member.manifest member)
        ~fn:(check_type_expr ctx diagnostics ~allow_named_alias_root:true))

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_value_declaration =
      Some (fun visitor declaration ->
        Option.for_each
          (Ast.ValueDeclaration.type_annotation declaration)
          ~fn:(check_type_expr ctx diagnostics ~allow_named_alias_root:false);
        (visitor, Syn.Visitor.Continue));
    enter_type_declaration =
      Some (fun visitor declaration ->
        check_type_declaration ctx diagnostics declaration;
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
