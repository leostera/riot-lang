(** Red tree representation - positioned syntax trees *)
open Std

(** A syntax node with position information *)
(** A syntax token with position information *)
type ('kind, 'text) syntax_node
(** A syntax element is either a node or token *)
type ('kind, 'text) syntax_token
type ('kind, 'text) syntax_trivia
(** Create a standalone red token at the given span *)
type ('kind, 'text) syntax_element =
  | Node of ('kind, 'text) syntax_node
  | Token of ('kind, 'text) syntax_token
val new_token: ('kind, 'text) Green.token -> Span.t -> ('kind, 'text) syntax_token

module SyntaxNode: sig
  (** Get the underlying green node *)
  val green: ('kind, 'text) syntax_node -> ('kind, 'text) Green.node
  (** Get the absolute offset *)
  val offset: ('kind, 'text) syntax_node -> int
  (** Get the span *)
  val span: ('kind, 'text) syntax_node -> Span.t
  (** Get the parent node *)
  val parent: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option
  (** Get the number of children *)
  val child_count: ('kind, 'text) syntax_node -> int
  (** Get a child by index *)
  val child: ('kind, 'text) syntax_node -> int -> ('kind, 'text) syntax_element option
  (** Get all non-trivia children.

      Trivia is attached to tokens and can be accessed through
      `SyntaxToken.leading_trivia`. *)
  val children: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_element array
  (** Get all non-trivia children as a list. *)
  val children_list: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_element list
  (** Get only the direct non-trivia token children *)
  val direct_tokens: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token list
  (** Get only the direct node children *)
  val direct_nodes: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node list
  (** Get the syntax kind *)

  (** Get the next sibling *)
  val kind: ('kind, 'text) syntax_node -> 'kind

  val next_sibling: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option
  (** Get the previous sibling *)
  val prev_sibling: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option
  (** Get the first token *)
  val first_token: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token option
  (** Get the last token *)
  val last_token: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token option
  (** Traverse in preorder *)
  val preorder: ('kind, 'text) syntax_node -> (('kind, 'text) syntax_element -> unit) -> unit
  (** Traverse in postorder *)
  val postorder: ('kind, 'text) syntax_node -> (('kind, 'text) syntax_element -> unit) -> unit

  val tokens: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token list
  (** Get every token in the subtree in source order *)
end
(** Create a new root node from a green node *)
module SyntaxTrivia: sig
  (** Get the underlying green trivia *)
  val green: ('kind, 'text) syntax_trivia -> ('kind, 'text) Green.trivia
  (** Get the absolute offset *)
  val offset: ('kind, 'text) syntax_trivia -> int
  (** Get the span *)
  val span: ('kind, 'text) syntax_trivia -> Span.t
  (** Get the syntax kind *)
  val kind: ('kind, 'text) syntax_trivia -> 'kind
  (** Get the text *)
  val text: ('kind, 'text) syntax_trivia -> 'text
end

module SyntaxToken: sig
  (** Get the underlying green token *)
  val green: ('kind, 'text) syntax_token -> ('kind, 'text) Green.token
  (** Get the absolute offset *)
  val offset: ('kind, 'text) syntax_token -> int
  (** Get the span *)
  val span: ('kind, 'text) syntax_token -> Span.t
  (** Get the syntax kind *)

  (** Get the text *)
  val kind: ('kind, 'text) syntax_token -> 'kind

  val text: ('kind, 'text) syntax_token -> 'text
  (** Get the leading trivia attached to this token.

      Each returned trivia entry has its own absolute offset/span and appears in
      source order immediately before the token body span. *)
  val leading_trivia: ('kind, 'text) syntax_token -> ('kind, 'text) syntax_trivia list
end

val new_root: ('kind, 'text) Green.node -> ('kind, 'text) syntax_node
(** Convert to JSON *)
val to_json:
  kind_to_json:('kind -> Data.Json.t) ->
  text_to_json:('text -> Data.Json.t) ->
  ('kind, 'text) syntax_element ->
  Data.Json.t
