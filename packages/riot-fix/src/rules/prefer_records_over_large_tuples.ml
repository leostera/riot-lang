open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "prefer-records-over-large-tuples"

let rule_description = "Large tuples should be records"

let rule_explain =
  {|
Large tuple types are hard to read at call sites because each position carries
meaning only by convention.

Prefer a record when a tuple grows large or repeats the same slot shape. Field
names make construction, pattern matching, and later changes easier to review.
|}

let rec unwrap_type = fun type_expr -> H.unwrap_type_expr type_expr

let diagnostic_for_type = fun type_expr ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.TypeExpr.as_node type_expr))
    ~suggestion:"Use a record type instead of a large positional tuple."
    ()

let rec collect_tuple_parts = fun ctx parts type_expr ->
  let type_expr = unwrap_type type_expr in
  match Ast.TypeExpr.view type_expr with
  | Ast.TypeExpr.Tuple { parts = type_parts } ->
      Vector.for_each type_parts ~fn:(collect_tuple_parts ctx parts)
  | _ ->
      Vector.push
        parts
        ~value:(
          H.node_source ctx (Ast.TypeExpr.as_node type_expr)
          |> String.trim
        )

let has_repeated_part = fun parts ->
  let len = Vector.length parts in
  let rec outer index =
    if Int.(index >= len) then
      false
    else
      let value = Vector.get_unchecked parts ~at:index in
      let rec inner next =
        if Int.(next >= len) then
          outer (index + 1)
        else if String.equal value (Vector.get_unchecked parts ~at:next) then
          true
        else
          inner (next + 1)
      in
      inner (index + 1)
  in
  outer 0

let tuple_should_be_record = fun ctx type_expr ->
  let parts = Vector.with_capacity ~size:8 in
  collect_tuple_parts ctx parts type_expr;
  let len = Vector.length parts in
  Int.(len >= 5) || Int.(len >= 4) && has_repeated_part parts

let rec check_type_expr = fun ctx diagnostics type_expr ->
  match Ast.TypeExpr.view (unwrap_type type_expr) with
  | Ast.TypeExpr.Tuple _ ->
      if tuple_should_be_record ctx type_expr then
        H.push_diagnostic diagnostics (diagnostic_for_type type_expr)
      else
        H.iter_fold
          Ast.TypeExpr.fold_child_type
          type_expr
          ~fn:(check_type_expr ctx diagnostics)
  | _ -> H.iter_fold
    Ast.TypeExpr.fold_child_type
    type_expr
    ~fn:(check_type_expr ctx diagnostics)

let check_variant_constructor_rhs = fun ctx diagnostics rhs ->
  match rhs with
  | Ast.VariantConstructor.Payload { payload = Ast.VariantConstructor.TypeExpr type_expr; _ } ->
      check_type_expr ctx diagnostics type_expr
  | Ast.VariantConstructor.Gadt { result; _ } -> check_type_expr ctx diagnostics result
  | Ast.VariantConstructor.Payload { payload = Ast.VariantConstructor.Record _; _ }
  | Ast.VariantConstructor.Plain -> ()

let check_type_declaration = fun ctx diagnostics declaration ->
  H.iter_fold
    Ast.TypeDeclaration.fold_member
    declaration
    ~fn:(fun member ->
      Option.for_each
        (Ast.TypeDeclaration.Member.manifest member)
        ~fn:(check_type_expr ctx diagnostics);
      Option.for_each
        (Ast.TypeDeclaration.Member.variant_type member)
        ~fn:(fun variant_type ->
          H.iter_fold
            Ast.VariantType.fold_constructor
            variant_type
            ~fn:(fun constructor ->
              match Ast.VariantConstructor.view constructor with
              | Ast.VariantConstructor.Constructor { rhs; _ } ->
                  check_variant_constructor_rhs ctx diagnostics rhs
              | Ast.VariantConstructor.Unknown _ -> ())))

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_type_declaration =
      Some (fun visitor declaration ->
        check_type_declaration ctx diagnostics declaration;
        (visitor, Syn.Visitor.Continue));
    enter_value_declaration =
      Some (fun visitor declaration ->
        Option.for_each
          (Ast.ValueDeclaration.type_annotation declaration)
          ~fn:(check_type_expr ctx diagnostics);
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
