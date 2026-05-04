open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "snake-case-record-fields"

let rule_description = "Record field names should use snake_case instead of camelCase"

let rule_explain =
  {|
Record fields are usually read in dense groups, so their naming needs to be calm
and regular. `snake_case` makes record types, record expressions, and field
accesses line up with ordinary value names.

Use `display_name` instead of `displayName`.
|}

let make_fix = fun token replacement ->
  H.replace_token_fix
    ~title:("Rename record field " ^ Ast.Token.text token ^ " to " ^ replacement)
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

let check_record_type = fun record diagnostics ->
  H.iter_fold
    Ast.RecordType.fold_field
    record
    ~fn:(fun field ->
      match Ast.RecordField.view field with
      | Ast.RecordField.Field { name; _ } -> (
          match Ast.Ident.last_segment name with
          | Some token when H.should_be_snake_case (Ast.Token.text token) ->
              H.push_diagnostic diagnostics (make_diagnostic token)
          | _ -> ()
        )
      | _ -> ());
  ()

let check_member = fun member diagnostics ->
  match Ast.TypeDeclaration.Member.record_type member with
  | Some record -> check_record_type record diagnostics
  | None -> ()

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
