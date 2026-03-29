open Std

(** Traversal Helpers for Syn CST
    
    This module provides utilities for traversing and querying Syn's
    Concrete Syntax Trees (CST), reducing boilerplate in lint rules.
*)
type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type red_node = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type red_token = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_token
type red_element = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_element
(** {1 Finding Nodes} *)

(** [find_nodes predicate tree] returns all nodes in [tree] that satisfy [predicate].
    
    Example:
    {[
      let open_nodes = find_nodes 
        (fun n -> SyntaxNode.kind n = OPEN_STMT) 
        tree
    ]}
*)
val find_nodes : (red_node -> bool) -> red_tree -> red_node list

(** [find_by_kind kind tree] returns all nodes of the given [kind].
    
    Example:
    {[
      let open_stmts = find_by_kind OPEN_STMT tree in
      List.iter check_open open_stmts
    ]}
*)
val find_by_kind : Syn.SyntaxKind.t -> red_tree -> red_node list

(** [find_by_kinds kinds tree] returns all nodes matching any of the given [kinds].
    
    Example:
    {[
      let exprs = find_by_kinds [PATH_EXPR; FIELD_ACCESS_EXPR] tree
    ]}
*)
val find_by_kinds : Syn.SyntaxKind.t list -> red_tree -> red_node list

(** {1 Token Queries} *)

(** [find_tokens predicate tree] returns all tokens that satisfy [predicate].
    
    Example:
    {[
      let comments = find_tokens
        (fun t -> SyntaxToken.kind t = COMMENT)
        tree
    ]}
*)
val find_tokens : (red_token -> bool) -> red_tree -> red_token list

(** [first_non_trivia_child node] returns the first child that is not whitespace,
    comment, or docstring.
    
    Useful for extracting meaningful tokens from nodes.
*)
val first_non_trivia_child : red_node -> red_element option

(** [first_non_trivia_token node] returns the first non-trivia token child.
    
    Returns [None] if the first non-trivia child is a node or doesn't exist.
*)
val first_non_trivia_token : red_node -> red_token option

(** {1 Visitor Pattern} *)

(** Visitor for folding over the tree.
    
    Example:
    {[
      let count_identifiers = fold
        {
          visit_node = (fun _ acc -> acc);
          visit_token = (fun tok acc ->
            if SyntaxToken.kind tok = IDENT then acc + 1 else acc
          );
        }
        0
        tree
    ]}
*)
(** [fold visitor init tree] folds over the tree with a visitor pattern.
    
    Traverses in pre-order: nodes before their children.
*)
type 'acc visitor = {
  visit_node : red_node -> 'acc -> 'acc;
  visit_token : red_token -> 'acc -> 'acc;
}
val fold : 'acc visitor -> 'acc -> red_tree -> 'acc

(** {1 Utilities} *)

(** [is_trivia kind] returns true if [kind] is whitespace, comment, or docstring. *)
val is_trivia : Syn.SyntaxKind.t -> bool

(** {1 Typed CST Helpers} *)

(** [expressions_of_structure_item item] returns the expressions reachable from
    [item] in the same pre-order traversal used by lint rules.

    The ordered structure-item list remains the canonical source-file body, so
    callers should iterate the file's items and concatenate this helper's
    results when they need recursive expression access.
*)
val expressions_of_structure_item : Syn.Cst.StructureItem.t -> Syn.Cst.Expression.t list

(** [let_bindings_of_structure_item item] returns the already-lifted
    [LetBinding.t] values reachable from [item], preserving item-local order.

    This does not synthesize a [LetBinding.t] for the primary binding inside
    [Expression.Let] or [ClassExpression.Let]; those stay represented on the
    enclosing expression nodes and must be inspected there when needed.
*)
val let_bindings_of_structure_item : Syn.Cst.StructureItem.t -> Syn.Cst.LetBinding.t list
