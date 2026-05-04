open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "avoid-single-letter-function-names"

let rule_description =
  "Function names should be descriptive instead of using single-letter placeholders"

let rule_explain =
  {|
Placeholder names make small examples shorter, but they make real functions
harder to understand once code grows. A descriptive function name gives readers
the intent before they inspect the implementation.

Prefer names such as `render_user`, `parse_header`, or `next_state` over `f`
and `g`.
|}

let should_flag = fun text -> String.length text = 1 && not (String.equal text "_")

let make_diagnostic = fun token ->
  H.diagnostic_for_token
    ~rule_id
    ~message:rule_description
    ~token
    ~suggestion:"Use a descriptive function name"
    ()

let check_binding = fun binding diagnostics ->
  if H.binding_is_function binding then
    match H.binding_name_token binding with
    | Some token when should_flag (Ast.Token.text token) ->
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
