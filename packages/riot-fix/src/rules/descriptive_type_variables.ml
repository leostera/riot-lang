open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "descriptive-type-variables"

let rule_description =
  "Type variables in type definitions should use descriptive names instead of short placeholders"

let rule_explain =
  {|
Public type parameters should explain the role they play. Names such as `'value`
and `'error` carry useful meaning in signatures, while placeholders such as `'a`
force readers to infer that meaning from the rest of the type expression.

This rule only checks parameters declared on the type itself. Nested uses remain
alone so existing polymorphic payloads can still mention imported or locally
introduced variables without producing noisy diagnostics.
|}

let is_descriptive = fun text -> String.length text > 1 || String.equal text "t"

let make_diagnostic = fun token ->
  let original = Ast.Token.text token in
  H.diagnostic_for_token
    ~rule_id
    ~message:rule_description
    ~token
    ~suggestion:("Rename '" ^ original ^ " to a descriptive type variable such as 'value")
    ()

let check_parameter = fun parameter diagnostics ->
  match parameter with
  | Ast.TypeDeclaration.Named { name; _ } when not (is_descriptive (Ast.Token.text name)) ->
      H.push_diagnostic diagnostics (make_diagnostic name)
  | _ -> ()

let check_member = fun member diagnostics ->
  H.iter_fold
    Ast.TypeDeclaration.Member.fold_parameter
    member
    ~fn:(fun parameter -> check_parameter parameter diagnostics)

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  H.for_each_type_declaration
    root
    ~fn:(fun declaration ->
      H.iter_fold
        Ast.TypeDeclaration.fold_member
        declaration
        ~fn:(fun member -> check_member member diagnostics));
  H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
