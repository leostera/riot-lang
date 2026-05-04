open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "class-case-constructors"

let rule_description = "Variant constructors should use ClassCase instead of underscores"

let rule_explain =
  {|
Variant constructors stand out as constructors in OCaml, so they should use
`ClassCase` names. Keeping underscores out of constructor names makes variants
visually distinct from values and fields while still staying predictable.

Use `GuestUser` instead of `Guest_user`.
|}

let make_fix = fun token replacement ->
  H.replace_token_fix
    ~title:("Rename constructor " ^ Ast.Token.text token ^ " to " ^ replacement)
    ~token
    ~text:replacement

let make_diagnostic = fun token ->
  let original = Ast.Token.text token in
  let replacement = H.to_class_case original in
  H.diagnostic_for_token
    ~rule_id
    ~message:rule_description
    ~token
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ~fix:(make_fix token replacement)
    ()

let check_variant = fun variant diagnostics ->
  H.iter_fold
    Ast.VariantType.fold_constructor
    variant
    ~fn:(fun constructor ->
      match Ast.VariantConstructor.view constructor with
      | Ast.VariantConstructor.Constructor { name; _ } -> (
          match Ast.Ident.last_segment name with
          | Some token when H.should_be_class_case (Ast.Token.text token) ->
              H.push_diagnostic diagnostics (make_diagnostic token)
          | _ -> ()
        )
      | _ -> ());
  ()

let check_member = fun member diagnostics ->
  match Ast.TypeDeclaration.Member.variant_type member with
  | Some variant -> check_variant variant diagnostics
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
