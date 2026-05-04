open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "class-case-module-names"

let rule_description = "Module names should use ClassCase instead of underscores"

let rule_explain =
  {|
Modules and module types should use `ClassCase`. This keeps module paths visually
distinct from values and fields while avoiding underscore-heavy names in paths.

Use `FooBar` instead of `Foo_bar`.
|}

let make_fix = fun token replacement ->
  H.replace_token_fix
    ~title:("Rename module " ^ Ast.Token.text token ^ " to " ^ replacement)
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

let check_name = fun token diagnostics ->
  if H.should_be_class_case (Ast.Token.text token) then
    H.push_diagnostic diagnostics (make_diagnostic token)
  else
    ()

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_module_declaration =
      Some (fun visitor declaration ->
        (
          match Ast.ModuleDeclaration.name declaration with
          | Some ident ->
              Ast.Ident.last_segment ident
              |> Option.for_each ~fn:(fun token -> check_name token diagnostics)
          | None -> ()
        );
        (visitor, Syn.Visitor.Continue));
    enter_module_type_declaration =
      Some (fun visitor declaration ->
        (
          match Ast.ModuleTypeDeclaration.name declaration with
          | Some ident ->
              Ast.Ident.last_segment ident
              |> Option.for_each ~fn:(fun token -> check_name token diagnostics)
          | None -> ()
        );
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
