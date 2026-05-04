open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-positional-bool-parameters"

let rule_description = "Boolean parameters should be named"

let rule_explain =
  {|
Positional booleans make call sites read like `render true user`, which forces the
reader to remember what the flag means.

Prefer a named parameter such as `~enabled:bool` or a richer variant when there are
multiple modes.
|}

let rec unwrap_type = fun type_expr -> H.unwrap_type_expr type_expr

let is_bool_type = fun ctx type_expr ->
  String.equal
    (
      H.node_source ctx (Ast.TypeExpr.as_node (unwrap_type type_expr))
      |> String.trim
    )
    "bool"

let diagnostic_for_node = fun node ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node node)
    ~suggestion:"Use a named boolean argument instead of a positional one."
    ()

let diagnostic_for_type = fun type_expr -> diagnostic_for_node (Ast.TypeExpr.as_node type_expr)

let is_named_parameter_pattern = fun pattern ->
  match Ast.cast_result_to_option (Ast.Parameter.cast (Ast.Pattern.as_node pattern)) with
  | Some parameter -> Option.is_some (H.parameter_kind parameter)
  | None -> false

let rec check_parameter_pattern = fun ctx diagnostics pattern ->
  if
    Syn.SyntaxKind.(Ast.Node.kind (Ast.Pattern.as_node pattern) = CONSTRAINT_PATTERN)
    && not (is_named_parameter_pattern pattern)
  then
    match Ast.Pattern.view pattern with
    | Ast.Pattern.Constraint { annotation; _ } when is_bool_type ctx annotation ->
        H.push_diagnostic diagnostics (diagnostic_for_type annotation)
    | _ -> ()

let rec check_pattern_node_tree = fun ctx diagnostics node ->
  match Ast.cast_result_to_option (Ast.Pattern.cast node) with
  | Some pattern ->
      if not (is_named_parameter_pattern pattern) then (
        check_parameter_pattern ctx diagnostics pattern;
        H.iter_fold
          Ast.Node.fold_child_node
          node
          ~fn:(check_pattern_node_tree ctx diagnostics)
      )
  | None -> H.iter_fold
    Ast.Node.fold_child_node
    node
    ~fn:(check_pattern_node_tree ctx diagnostics)

let check_pattern_tree = fun ctx diagnostics pattern ->
  check_pattern_node_tree
    ctx
    diagnostics
    (Ast.Pattern.as_node pattern)

let rec check_application_arguments = fun ctx diagnostics pattern ->
  match Ast.Pattern.view pattern with
  | Ast.Pattern.Constructor { payload = Some argument; _ } ->
      check_pattern_tree ctx diagnostics argument
  | Ast.Pattern.Constraint { pattern; _ } -> check_application_arguments ctx diagnostics pattern
  | _ -> ()

let check_let_binding_parameters = fun ctx diagnostics binding ->
  let seen_binding_pattern = ref false in
  H.iter_fold
    Ast.Node.fold_child_node
    (Ast.LetBinding.as_node binding)
    ~fn:(fun node ->
      match Ast.cast_result_to_option (Ast.Pattern.cast node) with
      | Some pattern ->
          if !seen_binding_pattern then
            check_pattern_tree ctx diagnostics pattern
          else (
            check_application_arguments ctx diagnostics pattern;
            seen_binding_pattern := true
          )
      | None -> ())

let rec check_type_expr = fun ctx diagnostics type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Ast.TypeExpr.Arrow { label; arg; ret } ->
      (
        match label with
        | Some _ -> ()
        | None ->
            if is_bool_type ctx arg then
              H.push_diagnostic diagnostics (diagnostic_for_type arg)
      );
      check_type_expr ctx diagnostics ret
  | _ -> H.iter_fold
    Ast.TypeExpr.fold_child_type
    type_expr
    ~fn:(check_type_expr ctx diagnostics)

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_let_binding =
      Some (fun visitor binding ->
        check_let_binding_parameters ctx diagnostics binding;
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
