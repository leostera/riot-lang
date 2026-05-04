open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "prefer-pipelines-for-nested-calls"

let rule_description = "Deeply nested function calls should usually be written as pipelines"

let rule_explain =
  {|
Nested unary calls become hard to scan once the reader has to count closing
parentheses to understand the data flow:

```ocaml
foo (bar (baz (hex 1)))
```

Prefer a pipeline when the chain is deep enough:

```ocaml
hex 1 |> baz |> bar |> foo
```
|}

type call_chain = {
  base: Ast.Expr.t;
  functions: string Vector.t;
}

let rec unwrap_expr = fun expr -> H.unwrap_expr expr

let callee_text = fun ctx callee ->
  match Ast.Expr.view (unwrap_expr callee) with
  | Ast.Expr.Ident _
  | Ast.Expr.FieldAccess _ ->
      Some (
        H.node_source ctx (Ast.Expr.as_node callee)
        |> String.trim
      )
  | _ -> None

let rec collect_call_chain = fun ctx expr ->
  match Ast.Expr.view (unwrap_expr expr) with
  | Ast.Expr.Apply { callee; argument } -> (
      match callee_text ctx callee with
      | Some callee ->
          let argument = unwrap_expr argument in
          let chain =
            match collect_call_chain ctx argument with
            | Some chain -> chain
            | None ->
                {
                  base = argument;
                  functions = Vector.with_capacity ~size:(Ast.Expr.child_expr_count expr);
                }
          in
          Vector.push chain.functions ~value:callee;
          Some chain
      | None -> None
    )
  | _ -> None

let replacement_text = fun ctx chain ->
  let parts = Vector.with_capacity ~size:(Vector.length chain.functions + 1) in
  Vector.push
    parts
    ~value:(
      H.node_source ctx (Ast.Expr.as_node chain.base)
      |> String.trim
    );
  Vector.for_each chain.functions ~fn:(fun function_name -> Vector.push parts ~value:function_name);
  H.vector_to_list parts
  |> String.concat " |> "

let diagnostic_for_expr = fun ctx expr chain ->
  let replacement = replacement_text ctx chain in
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Expr.as_node expr))
    ~suggestion:"Rewrite this call chain as a pipeline."
    ~fix:(Fix.make
      ~title:"Rewrite nested calls as a pipeline"
      ~operations:[ Fix.replace_node_with_text ~target:(Ast.Expr.as_node expr) ~text:replacement; ])
    ()

let check_expr = fun ctx diagnostics expr ->
  match collect_call_chain ctx expr with
  | Some chain when Vector.length chain.functions >= 4 ->
      H.push_diagnostic diagnostics (diagnostic_for_expr ctx expr chain)
  | Some _
  | None -> ()

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_expr =
      Some (fun visitor expr ->
        check_expr ctx diagnostics expr;
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
