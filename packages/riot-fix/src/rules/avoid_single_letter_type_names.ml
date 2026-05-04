open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "avoid-single-letter-type-names"

let rule_description =
  "Type names should be descriptive instead of using single-letter placeholders, except for t"

let rule_explain =
  {|
Single-letter type names are usually placeholders. The conventional module-local
type name `t` is useful, but other one-letter names hide intent and become hard
to search for.

Use a descriptive type name such as `user`, `message`, or `request_context`.
|}

let should_flag = fun text -> String.length text = 1 && not (String.equal text "t")

let make_diagnostic = fun token ->
  H.diagnostic_for_token
    ~rule_id
    ~message:rule_description
    ~token
    ~suggestion:"Use a descriptive type name, or `t` for the module primary type"
    ()

let check_member = fun member diagnostics ->
  match Ast.TypeDeclaration.Member.name member with
  | Some ident -> (
      match H.ident_last_segment ident with
      | Some token when should_flag (Ast.Token.text token) ->
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
