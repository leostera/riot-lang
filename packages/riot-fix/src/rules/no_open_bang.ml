open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-open-bang"

let rule_description = "Avoid open! and prefer plain open or explicit qualification"

let rule_explain =
  {|
`open!` suppresses the compiler warning that would normally tell you about accidental
shadowing. That is a strong tradeoff to make for a small reduction in module
qualification.

If an open is truly harmless, plain `open` keeps the code readable without disabling
the warning mechanism. If the scope is sensitive, explicit qualification like
`List.map` or `Http.Response.ok` makes the dependency even clearer.

This rule exists because shadowing bugs are cheap to introduce and annoying to notice
late. `open!` makes that problem easier to miss.
|}

let make_diagnostic = fun token ->
  H.diagnostic_for_token
    ~rule_id
    ~message:rule_description
    ~token
    ~suggestion:"Remove ! or qualify the module usage explicitly."
    ~fix:(H.replace_token_fix ~title:"Replace open! with plain open" ~token ~text:"")
    ()

let bang_token = fun open_declaration ->
  let found = ref None in
  H.iter_fold
    Ast.Node.fold_child_token
    (Ast.OpenDeclaration.as_node open_declaration)
    ~fn:(fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if Syn.SyntaxKind.(Ast.Token.kind token = BANG) then
            found := Some token);
  !found

let check_open = fun diagnostics open_declaration ->
  match bang_token open_declaration with
  | Some token -> H.push_diagnostic diagnostics (make_diagnostic token)
  | None -> ()

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_open_declaration =
      Some (fun visitor open_declaration ->
        check_open diagnostics open_declaration;
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
