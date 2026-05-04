open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-eta-expansion"

let rule_description = "Eta-expanded wrappers should be collapsed"

let rule_explain =
  {|
An eta-expanded wrapper forwards every argument to another function without adding
behavior.

Prefer `let wrap foo = foo` over `let wrap foo = fun value -> foo value`.
|}

let words = fun text ->
  String.split text ~by:" "
  |> List.filter ~fn:(fun word -> not (String.equal word ""))

let rec equal_string_lists = fun left right ->
  match (left, right) with
  | ([], []) -> true
  | (left :: left_tail, right :: right_tail) ->
      String.equal left right && equal_string_lists left_tail right_tail
  | _ -> false

let replacement_text = fun ctx expr ->
  let source =
    H.node_source ctx (Ast.Expr.as_node expr)
    |> String.trim
  in
  if not (String.starts_with ~prefix:"fun " source) then
    None
  else
    match String.split source ~by:"->" with
    | [ params_source; call_source ] -> (
        let params =
          String.sub params_source ~offset:4 ~len:(String.length params_source - 4)
          |> words
        in
        match words call_source with
        | callee :: args when equal_string_lists params args -> Some callee
        | _ -> None
      )
    | _ -> None

let make_diagnostic = fun ctx expr replacement ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Expr.as_node expr))
    ~suggestion:"Replace this wrapper with the function it forwards to."
    ~fix:(Fix.make
      ~title:"Collapse eta-expanded wrapper"
      ~operations:[ Fix.replace_node_with_text ~target:(Ast.Expr.as_node expr) ~text:replacement; ])
    ()

let diagnostic_for_binding = fun ctx binding ->
  match Ast.LetBinding.body binding with
  | Some body -> (
      match Ast.Expr.view body with
      | Ast.Expr.Fun _ -> (
          match replacement_text ctx body with
          | Some replacement -> Some (make_diagnostic ctx body replacement)
          | None -> None
        )
      | _ -> None
    )
  | None -> None

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_let_binding =
      Some (fun visitor binding ->
        (
          match diagnostic_for_binding ctx binding with
          | Some diagnostic -> H.push_diagnostic diagnostics diagnostic
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
