open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "snake-case-variable-names"

let rule_description = "Variable names should use snake_case instead of camelCase"

let rule_explain =
  {|
Value bindings that are not functions should use `snake_case`. The distinction
between values and functions already comes from how they are used; using a second
camelCase convention for values adds visual noise without improving clarity.

Use names such as `current_user` and `page_size` instead of `currentUser` and
`pageSize`.
|}

let make_fix = fun token replacement ->
  H.replace_token_fix
    ~title:("Rename variable " ^ Ast.Token.text token ^ " to " ^ replacement)
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

let check_binding = fun binding diagnostics ->
  if H.binding_is_function binding then
    ()
  else
    match H.binding_name_token binding with
    | Some token when H.should_be_snake_case (Ast.Token.text token) ->
        H.push_diagnostic diagnostics (make_diagnostic token)
    | _ -> ()

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
