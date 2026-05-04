open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "prefer-t-for-single-type-modules"

let rule_description = "Modules with a single primary type should name it `t`"

let rule_explain =
  {|
When a module exposes one central type, the conventional spelling is `Module.t`.
Repeating the module name in the type name makes call sites heavier without adding
information.

Prefer `User.t` over `User.user`.
|}

let push_type_name = fun names declaration ->
  H.iter_fold
    Ast.TypeDeclaration.fold_member
    declaration
    ~fn:(fun member ->
      match Ast.TypeDeclaration.Member.name member with
      | Some name -> Vector.push names ~value:name
      | None -> ())

let type_names_in_structure_item = fun names item ->
  match Ast.StructureItem.view item with
  | Ast.StructureItem.Type (Ast.TypeDeclarationItem declaration) -> push_type_name names declaration
  | _ -> ()

let type_names_in_signature_item = fun names item ->
  match Ast.SignatureItem.view item with
  | Ast.SignatureItem.Type (Ast.TypeDeclarationItem declaration) -> push_type_name names declaration
  | _ -> ()

let diagnostic_for_name = fun token ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_token token)
    ~suggestion:"Rename the single primary type in this module to `t`."
    ()

let diagnostic_for_names = fun diagnostics names ->
  if Int.equal (Vector.length names) 1 then
    let name = Vector.get_unchecked names ~at:0 in
    if not (String.equal (Ast.Ident.text name) "t") then
      Ast.Ident.last_segment name
      |> Option.for_each
        ~fn:(fun token -> H.push_diagnostic diagnostics (diagnostic_for_name token))

let check_module_declaration = fun diagnostics declaration ->
  let names = Vector.with_capacity ~size:(Ast.ModuleDeclaration.member_count declaration) in
  H.iter_fold
    Ast.ModuleDeclaration.fold_structure_item
    declaration
    ~fn:(type_names_in_structure_item names);
  H.iter_fold
    Ast.ModuleDeclaration.fold_signature_item
    declaration
    ~fn:(type_names_in_signature_item names);
  diagnostic_for_names diagnostics names

let check_module_type_declaration = fun diagnostics declaration ->
  let names =
    Vector.with_capacity ~size:(Ast.ModuleTypeDeclaration.signature_item_count declaration)
  in
  H.iter_fold
    Ast.ModuleTypeDeclaration.fold_signature_item
    declaration
    ~fn:(type_names_in_signature_item names);
  diagnostic_for_names diagnostics names

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_module_declaration =
      Some (fun visitor declaration ->
        check_module_declaration diagnostics declaration;
        (visitor, Syn.Visitor.Continue));
    enter_module_type_declaration =
      Some (fun visitor declaration ->
        check_module_type_declaration diagnostics declaration;
        (visitor, Syn.Visitor.Continue));
  }
  in
  Syn.Visitor.make ~ctx:() ~hooks
  |> fun visitor ->
    ignore (Syn.Visitor.visit_node visitor root);
    H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
