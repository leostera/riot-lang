open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "snake-case-type-names"

let rule_description = "Type names should use snake_case instead of camelCase"

let rule_explain =
  {|
Type aliases and declarations should use predictable `snake_case` names. Keeping
type names in the same naming family as values and record fields makes signatures
easier to scan and avoids adding a second lower-case naming convention.

Use names such as `user_profile` instead of `userProfile`. Short conventional
names such as `t` are handled by the single-letter-name rule.
|}

let make_fix = fun token replacement ->
  H.replace_token_fix
    ~title:("Rename type " ^ Ast.Token.text token ^ " to " ^ replacement)
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

let check_member = fun member diagnostics ->
  match Ast.TypeDeclaration.Member.name member with
  | Some ident -> (
      match H.ident_last_segment ident with
      | Some token when H.should_be_snake_case (Ast.Token.text token) ->
          H.push_diagnostic diagnostics (make_diagnostic token)
      | _ -> ()
    )
  | _ -> ()

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
