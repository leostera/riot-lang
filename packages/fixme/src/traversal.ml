open Std
open Std.Collections

module Ast = Syn.Ast

module Syntax_tree = Syn.SyntaxTree

type syntax_tree = Ast.Node.t

type syntax_node = Ast.Node.t

type syntax_token = Ast.Token.t

type syntax_element =
  | Node of syntax_node
  | Token of syntax_token

type red_tree = syntax_tree

type red_node = syntax_node

type red_token = syntax_token

type red_element = syntax_element

let node_of_child = fun (parent: syntax_node) id: syntax_node -> { tree = parent.Ast.tree; id }

let token_of_child = fun (parent: syntax_node) id: syntax_token -> { tree = parent.Ast.tree; id }

let child_element = fun (parent: syntax_node) ->
  function
  | Syntax_tree.Node id -> Some (Node (node_of_child parent id))
  | Syntax_tree.Token id -> Some (Token (token_of_child parent id))
  | Syntax_tree.Missing _ -> None

let to_list = fun vector -> Vector.to_array vector |> Array.to_list

let find_nodes = fun predicate tree ->
  let found = Vector.with_capacity ~size:(Ast.Node.child_count tree + 1) in
  let hooks =
    {
      Syn.Visitor.empty_hooks with
      enter_node = Some (
        fun visitor node ->
          if predicate node then
            Vector.push found ~value:node;
          (visitor, Syn.Visitor.Continue)
      )
    }
  in
  Syn.Visitor.make ~ctx:() ~hooks |> fun visitor ->
    ignore (Syn.Visitor.visit_node visitor tree);
    to_list found

let find_by_kind = fun kind tree ->
  find_nodes
    (
      fun node -> Syn.SyntaxKind.equal (Ast.Node.kind node) kind
    )
    tree

let find_by_kinds = fun kinds tree ->
  find_nodes
    (
      fun node -> List.any kinds ~fn:(
        fun kind -> Syn.SyntaxKind.equal (Ast.Node.kind node) kind
      )
    )
    tree

let find_tokens = fun predicate tree ->
  let found = Vector.with_capacity ~size:(Ast.Node.token_width tree) in
  let hooks =
    {
      Syn.Visitor.empty_hooks with
      enter_token = Some (
        fun visitor token ->
          if predicate token then
            Vector.push found ~value:token;
          visitor
      )
    }
  in
  Syn.Visitor.make ~ctx:() ~hooks |> fun visitor ->
    ignore (Syn.Visitor.visit_node visitor tree);
    to_list found

let is_trivia = Syn.SyntaxKind.is_trivia

let first_non_trivia_child = fun node ->
  let result = ref None in
  Ast.Node.for_each_child node ~fn:(
    fun child ->
      match !result with
      | Some _ -> ()
      | None -> (
        match child_element node child with
        | None -> ()
        | Some (Node child_node) when is_trivia (Ast.Node.kind child_node) -> ()
        | Some (Token token) when is_trivia (Ast.Token.kind token) -> ()
        | Some element -> result := Some element
      )
  );
  !result

let first_non_trivia_token = fun node ->
  let result = ref None in
  Ast.Node.for_each_child node ~fn:(
    fun child ->
      match !result with
      | Some _ -> ()
      | None -> (
        match child_element node child with
        | Some (Token token) when not (is_trivia (Ast.Token.kind token)) -> result := Some token
        | _ -> ()
      )
  );
  !result

type 'acc visitor = { visit_node: red_node -> 'acc -> 'acc; visit_token: red_token -> 'acc -> 'acc }

let fold = fun visitor init tree ->
  let hooks =
    {
      Syn.Visitor.empty_hooks with
      enter_node = Some (
        fun state node -> (Syn.Visitor.with_ctx state (visitor.visit_node node (Syn.Visitor.ctx state)), Syn.Visitor.Continue)
      );
      enter_token = Some (
        fun state token -> Syn.Visitor.with_ctx state (visitor.visit_token token (Syn.Visitor.ctx state))
      )
    }
  in
  let state = Syn.Visitor.make ~ctx:init ~hooks in Syn.Visitor.visit_node state tree |> Syn.Visitor.ctx

let expressions_of_structure_item = fun item ->
  let expressions = Vector.with_capacity ~size:(Ast.Node.child_count item) in
  let hooks =
    {
      Syn.Visitor.empty_hooks with
      enter_expr = Some (
        fun visitor expr ->
          Vector.push expressions ~value:expr;
          (visitor, Syn.Visitor.Skip_subtree)
      )
    }
  in
  Syn.Visitor.make ~ctx:() ~hooks |> fun visitor ->
    ignore (Syn.Visitor.visit_node visitor item);
    to_list expressions

let let_bindings_of_structure_item = fun item ->
  let bindings = Vector.with_capacity ~size:(Ast.Node.child_count item) in
  let hooks =
    {
      Syn.Visitor.empty_hooks with
      enter_let_binding = Some (
        fun visitor binding ->
          Vector.push bindings ~value:binding;
          (visitor, Syn.Visitor.Skip_subtree)
      )
    }
  in
  Syn.Visitor.make ~ctx:() ~hooks |> fun visitor ->
    ignore (Syn.Visitor.visit_node visitor item);
    to_list bindings
