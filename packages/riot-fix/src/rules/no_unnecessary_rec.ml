open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-unnecessary-rec"

let rule_description = "Recursive bindings should use rec only when needed"

let rule_explain =
  {|
Remove rec from let bindings that do not refer to themselves or another binding in
the same recursive group:

```ocaml
let render x = x + 1
```

Keeping `rec` only where recursion is required makes accidental cycles stand out.
|}

let diagnostic_for_rec = fun token ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_token token)
    ~suggestion:"Remove rec from this let binding."
    ()

let collect_binding_name = fun names binding ->
  match H.binding_name_token binding with
  | Some name -> Vector.push names ~value:(Ast.Token.text name)
  | None -> ()

let name_vector_contains = fun names name ->
  let found = ref false in
  Vector.for_each
    names
    ~fn:(fun candidate ->
      if String.equal candidate name then
        found := true);
  !found

let binding_body_references_any_name = fun names binding ->
  match Ast.LetBinding.body binding with
  | Some body ->
      let found = ref false in
      H.iter_fold
        Ast.Node.fold_token
        (Ast.Expr.as_node body)
        ~fn:(fun token ->
          if name_vector_contains names (Ast.Token.text token) then
            found := true);
      !found
  | None -> false

let let_declaration_has_recursive_reference = fun names declaration ->
  let found = ref false in
  H.iter_fold
    Ast.LetDeclaration.fold_binding
    declaration
    ~fn:(fun binding ->
      if binding_body_references_any_name names binding then
        found := true);
  !found

let check_let_declaration = fun diagnostics declaration ->
  match Ast.LetDeclaration.rec_token declaration with
  | Some rec_token ->
      let names = Vector.with_capacity ~size:(Ast.LetDeclaration.binding_count declaration) in
      H.iter_fold Ast.LetDeclaration.fold_binding declaration ~fn:(collect_binding_name names);
      if not (let_declaration_has_recursive_reference names declaration) then
        H.push_diagnostic diagnostics (diagnostic_for_rec rec_token)
  | None -> ()

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_let_declaration =
      Some (fun visitor declaration ->
        check_let_declaration diagnostics declaration;
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
