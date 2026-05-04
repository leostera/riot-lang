open Std

module H = Rule_helpers
module Ast = Syn.Ast
module Kind = Syn.SyntaxKind

let rule_id = Rule_id.from_string "snake-case-polyvariant-tags"

let rule_description = "Polymorphic variant tags should use snake_case instead of camelCase"

let rule_explain =
  {|
Polymorphic variant tags are lower-case names prefixed by a backtick, so their
payload name should follow the same `snake_case` convention as values and record
fields.

Use `` `guest_user`` instead of `` `GuestUser``.
|}

let make_fix = fun token replacement ->
  H.replace_token_fix
    ~title:("Rename polyvariant tag " ^ Ast.Token.text token ^ " to " ^ replacement)
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

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let after_backtick = ref false in
  H.iter_fold
    Ast.Node.fold_token
    root
    ~fn:(fun token ->
      let kind = Ast.Token.kind token in
      if Kind.(kind = BACKTICK) then
        after_backtick := true
      else (
        if
          !after_backtick && Kind.(kind = IDENT) && H.should_be_snake_case (Ast.Token.text token)
        then
          H.push_diagnostic diagnostics (make_diagnostic token);
        after_backtick := false
      ));
  H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
