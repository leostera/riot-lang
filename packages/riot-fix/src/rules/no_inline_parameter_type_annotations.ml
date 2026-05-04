open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-inline-parameter-type-annotations"

let rule_description =
  "Function parameter type annotations should live in the function signature, not inline on each parameter"

let rule_explain =
  {|
Inline parameter annotations scatter a function signature across the argument list.
That makes the API harder to skim because readers have to reconstruct the type from
several small annotations instead of reading it in one place.

Putting the type on the binding itself keeps the interface intact:
`let render : int -> bool -> view = ...`.
The parameters can then stay focused on names, and the type stays focused on the
shape of the function.

This also makes later refactors easier, because the function already has a single
obvious place where its signature lives.
|}

let rec pattern_has_inline_type = fun pattern ->
  match Ast.Pattern.view (H.unwrap_pattern pattern) with
  | Ast.Pattern.Constraint _ -> true
  | _ -> false

and parameter_has_inline_type = fun parameter ->
  match Ast.Parameter.view parameter with
  | Ast.Parameter.Param { pattern = Some pattern; _ } -> pattern_has_inline_type pattern
  | _ -> false

let make_diagnostic = fun parameter ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Parameter.as_node parameter))
    ~suggestion:"Move the parameter type annotation into the function signature"
    ()

let diagnostic_for_binding = fun binding ->
  let found = ref None in
  H.iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun parameter ->
      match !found with
      | Some _ -> ()
      | None ->
          if parameter_has_inline_type parameter then
            found := Some (make_diagnostic parameter));
  !found

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  H.for_each_let_binding
    root
    ~fn:(fun binding ->
      if H.binding_is_function binding then
        match diagnostic_for_binding binding with
        | Some diagnostic -> H.push_diagnostic diagnostics diagnostic
        | None -> ());
  H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
