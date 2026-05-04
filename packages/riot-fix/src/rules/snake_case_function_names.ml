open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "snake-case-function-names"

let rule_description = "Function names should use snake_case instead of camelCase"

let rule_explain =
  {|
Function bindings should use `snake_case` names. A single naming convention makes
call sites easier to scan and keeps function names visually aligned with values,
arguments, and fields.

Use `parse_user` or `render_item` instead of `parseUser` or `renderItem`.
|}

let make_fix = fun token replacement ->
  H.replace_token_fix
    ~title:("Rename function " ^ Ast.Token.text token ^ " to " ^ replacement)
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
    match H.binding_name_token binding with
    | Some token when H.should_be_snake_case (Ast.Token.text token) ->
        H.push_diagnostic diagnostics (make_diagnostic token)
    | _ -> ()
  else
    ()

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
