(** Positioned red-tree representation. *)
open Std

(** A syntax node with position information. *)
type ('kind, 'text) syntax_node
(** A syntax token with position information. *)
type ('kind, 'text) syntax_token
(** A trivia entry with position information. *)
type ('kind, 'text) syntax_trivia
(** A syntax element: either a node or a token. *)
type ('kind, 'text) syntax_element =
  | Node of ('kind, 'text) syntax_node
  | Token of ('kind, 'text) syntax_token

(** Create a standalone red token at the given span. *)
val new_token: ('kind, 'text) Green.token -> Span.t -> ('kind, 'text) syntax_token

module SyntaxNode: sig
  (** Return the underlying green node. *)
  val green: ('kind, 'text) syntax_node -> ('kind, 'text) Green.node

  (** Return the absolute offset. *)
  val offset: ('kind, 'text) syntax_node -> int

  (** Return the span. *)
  val span: ('kind, 'text) syntax_node -> Span.t

  (** Return the parent node, if one exists. *)
  val parent: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option

  (** Return the number of direct children. *)
  val child_count: ('kind, 'text) syntax_node -> int

  (** Fold over direct non-trivia children in source order. *)
  val fold_children:
    ('kind, 'text) syntax_node -> 'acc -> ('acc -> ('kind, 'text) syntax_element -> 'acc) -> 'acc

  (** Return the child at the given index, if it exists. *)
  val child: ('kind, 'text) syntax_node -> int -> ('kind, 'text) syntax_element option

  (** Get all non-trivia children.

      Trivia is attached to tokens and can be accessed through
      `SyntaxToken.leading_trivia`. *)
  val children: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_element list

  (** Alias for `children`. *)
  val children_list: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_element list

  (** Return only the direct non-trivia token children. *)
  val direct_tokens: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token list

  (** Return only the direct node children. *)
  val direct_nodes: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node list

  (** Return the syntax kind. *)
  val kind: ('kind, 'text) syntax_node -> 'kind

  (** Return the next sibling node, if one exists. *)
  val next_sibling: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option

  (** Return the previous sibling node, if one exists. *)
  val prev_sibling: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option

  (** Return the first token in the subtree, if one exists. *)
  val first_token: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token option

  (** Return the last token in the subtree, if one exists. *)
  val last_token: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token option

  (** Traverse the subtree in preorder. *)
  val preorder: ('kind, 'text) syntax_node -> (('kind, 'text) syntax_element -> unit) -> unit

  (** Traverse the subtree in postorder. *)
  val postorder: ('kind, 'text) syntax_node -> (('kind, 'text) syntax_element -> unit) -> unit

  (** Return every token in the subtree in source order. *)
  val tokens: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token list
end

module SyntaxTrivia: sig
  (** Return the underlying green trivia. *)
  val green: ('kind, 'text) syntax_trivia -> ('kind, 'text) Green.trivia

  (** Return the absolute offset. *)
  val offset: ('kind, 'text) syntax_trivia -> int

  (** Return the span. *)
  val span: ('kind, 'text) syntax_trivia -> Span.t

  (** Return the syntax kind. *)
  val kind: ('kind, 'text) syntax_trivia -> 'kind

  (** Return the trivia text. *)
  val text: ('kind, 'text) syntax_trivia -> 'text
end

module SyntaxToken: sig
  (** Return the underlying green token. *)
  val green: ('kind, 'text) syntax_token -> ('kind, 'text) Green.token

  (** Return the absolute offset. *)
  val offset: ('kind, 'text) syntax_token -> int

  (** Return the span. *)
  val span: ('kind, 'text) syntax_token -> Span.t

  (** Return the syntax kind. *)
  val kind: ('kind, 'text) syntax_token -> 'kind

  (** Return the token text. *)
  val text: ('kind, 'text) syntax_token -> 'text

  (** Get the leading trivia attached to this token.

      Each returned trivia entry has its own absolute offset/span and appears in
      source order immediately before the token body span. *)
  val leading_trivia: ('kind, 'text) syntax_token -> ('kind, 'text) syntax_trivia list
end

(** Create a red root node from a green root node. *)
val new_root: ('kind, 'text) Green.node -> ('kind, 'text) syntax_node

(** Encode a positioned syntax element as JSON. *)
val to_json:
  kind_to_json:('kind -> Data.Json.t) ->
  text_to_json:('text -> Data.Json.t) ->
  ('kind, 'text) syntax_element ->
  Data.Json.t
