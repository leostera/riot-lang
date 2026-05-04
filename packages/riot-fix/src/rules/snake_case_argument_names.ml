open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "snake-case-argument-names"

let rule_description = "Argument names should use snake_case instead of camelCase"

let rule_explain =
  {|
Function arguments should use `snake_case`, including positional, labeled, and
optional arguments. This keeps call signatures consistent and makes argument
names predictable when they appear at call sites.

Use names such as `user_id`, `display_name`, and `page_size` instead of
`userId`, `displayName`, and `pageSize`.
|}

let make_fix = fun token replacement ->
  H.replace_token_fix
    ~title:("Rename argument " ^ Ast.Token.text token ^ " to " ^ replacement)
    ~token
    ~text:replacement

let make_diagnostic = fun token ->
  let original = Ast.Token.text token in
  let replacement = H.to_snake_case original in
  H.diagnostic_for_token
    ~rule_id
    ~message:rule_description
    ~token
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ~fix:(make_fix token replacement)
    ()

let check_parameter = fun pattern diagnostics ->
  match H.parameter_name_token pattern with
  | Some token when H.should_be_snake_case (Ast.Token.text token) ->
      H.push_diagnostic diagnostics (make_diagnostic token)
  | _ -> ()

let check_binding = fun binding diagnostics ->
  H.iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun parameter -> check_parameter parameter diagnostics)

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  H.for_each_let_binding root ~fn:(fun binding -> check_binding binding diagnostics);
  H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
