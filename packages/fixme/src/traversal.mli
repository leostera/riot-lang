open Std

(**
   Traversal helpers for Syn Ast views.

   Use this module inside rules when you want common Ast queries without
   rewriting the same syntax-tree traversal code each time.
*)
type syntax_tree = Syn.Ast.Node.t
type syntax_node = Syn.Ast.Node.t
type syntax_token = Syn.Ast.Token.t
type syntax_element =
  | Node of syntax_node
  | Token of syntax_token
type red_tree = syntax_tree
type red_node = syntax_node
type red_token = syntax_token
type red_element = syntax_element
(** {1 Finding Nodes} *)
(**
   Return all nodes in [tree] that satisfy [predicate].

   Use this when the selection logic is more specific than a syntax kind
   match.

   Example:
   ```ocaml
   let open_nodes =
     Traversal.find_nodes
       (fun node -> Syn.Ast.Node.kind node = OPEN_DECLARATION)
       tree
   ```
*)
val find_nodes: (red_node -> bool) -> red_tree -> red_node list

(**
   Return all nodes of the given syntax kind.

   Use this when a rule targets one specific Ast node kind.
*)
val find_by_kind: Syn.SyntaxKind.t -> red_tree -> red_node list

(**
   Return all nodes matching any of the given syntax kinds.

   Use this when one rule needs to cover a small family of related nodes.
*)
val find_by_kinds: Syn.SyntaxKind.t list -> red_tree -> red_node list

(** {1 Token Queries} *)

(** Return all tokens in [tree] that satisfy [predicate]. *)
val find_tokens: (red_token -> bool) -> red_tree -> red_token list

(**
   Return the first non-trivia child.

   Use this when you need the first meaningful child and want to ignore
   whitespace, comments, and docstrings.
*)
val first_non_trivia_child: red_node -> red_element option

(** Return the first non-trivia token child, if it exists. *)
val first_non_trivia_token: red_node -> red_token option

(** {1 Visitor Pattern} *)

(** Visitor used by [fold]. *)
type 'acc visitor = {
  visit_node: red_node -> 'acc -> 'acc;
  visit_token: red_token -> 'acc -> 'acc;
}

(**
   Fold over a tree in preorder.

   Use this when you need one pass that can see both nodes and tokens.

   Example:
   ```ocaml
   let count_identifiers =
     Traversal.fold
       {
         visit_node = (fun _ acc -> acc);
         visit_token =
           (fun tok acc ->
             if Syn.Ceibo.Red.SyntaxToken.kind tok = IDENT then acc + 1 else acc);
       }
       0
       tree
   ```
*)
val fold: 'acc visitor -> 'acc -> red_tree -> 'acc

(** {1 Utilities} *)

(** Return `true` if the syntax kind is trivia. *)
val is_trivia: Syn.SyntaxKind.t -> bool

(** {1 Typed Ast Helpers} *)

(**
   Return the expressions reachable from the structure item.

   Use this when a rule operates over expressions but starts from typed Ast
   structure items.
*)
val expressions_of_structure_item: Syn.Ast.StructureItem.t -> Syn.Ast.Expr.t list

(**
   Return the already-lifted let bindings reachable from the structure item.

   The returned list preserves item-local order.
*)
val let_bindings_of_structure_item: Syn.Ast.StructureItem.t -> Syn.Ast.LetBinding.t list
