open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-exn-suffix-functions"

let rule_description = "Function names should not end with _exn"

let rule_explain =
  {|
Names ending in `_exn` normalize exception-throwing control flow as part of the API.
That makes failure behavior harder to reason about at call sites and encourages
exceptions for situations that are often expected, such as parse failure or lookup
failure.

Prefer APIs that return `Result` or `Option` and make failure explicit in the type.
If an exceptional variant truly has to exist, it should usually be the less prominent
entry point rather than the one that shapes the naming of the whole interface.

The goal here is not just naming. It is to steer APIs away from using exceptions as
ordinary control flow.
|}

let make_diagnostic = fun token ->
  let name = Ast.Token.text token in
  H.diagnostic_for_token
    ~rule_id
    ~message:rule_description
    ~token
    ~suggestion:("Rename " ^ name ^ " to remove the _exn suffix and prefer a Result/Option API.")
    ()

let check_binding = fun binding diagnostics ->
  if H.binding_is_function binding then
    match H.binding_name_token binding with
    | Some token when String.ends_with ~suffix:"_exn" (Ast.Token.text token) ->
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
