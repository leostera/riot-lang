(** Red tree representation - positioned syntax trees *)

open Std

type ('kind, 'text) syntax_node
(** A syntax node with position information *)

type ('kind, 'text) syntax_token
(** A syntax token with position information *)

type ('kind, 'text) syntax_element =
  | Node of ('kind, 'text) syntax_node
  | Token of ('kind, 'text) syntax_token
(** A syntax element is either a node or token *)

val new_token : ('kind, 'text) Green.token -> Span.t -> ('kind, 'text) syntax_token
(** Create a standalone red token at the given span *)

module SyntaxNode : sig
  val green : ('kind, 'text) syntax_node -> ('kind, 'text) Green.node
  (** Get the underlying green node *)

  val offset : ('kind, 'text) syntax_node -> int
  (** Get the absolute offset *)

  val span : ('kind, 'text) syntax_node -> Span.t
  (** Get the span *)

  val parent : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option
  (** Get the parent node *)

  val child_count : ('kind, 'text) syntax_node -> int
  (** Get the number of children *)

  val child : ('kind, 'text) syntax_node -> int -> ('kind, 'text) syntax_element option
  (** Get a child by index *)

  val children : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_element array
  (** Get all children *)

  val children_list : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_element list
  (** Get all children as a list *)

  val direct_tokens : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token list
  (** Get only the direct token children *)

  val direct_nodes : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node list
  (** Get only the direct node children *)

  val kind : ('kind, 'text) syntax_node -> 'kind
  (** Get the syntax kind *)

  val next_sibling : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option
  (** Get the next sibling *)

  val prev_sibling : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option
  (** Get the previous sibling *)

  val first_token : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token option
  (** Get the first token *)

  val last_token : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token option
  (** Get the last token *)

  val preorder : ('kind, 'text) syntax_node -> (('kind, 'text) syntax_element -> unit) -> unit
  (** Traverse in preorder *)

  val postorder : ('kind, 'text) syntax_node -> (('kind, 'text) syntax_element -> unit) -> unit
  (** Traverse in postorder *)

  val tokens : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token list
  (** Get every token in the subtree in source order *)
end

module SyntaxToken : sig
  val green : ('kind, 'text) syntax_token -> ('kind, 'text) Green.token
  (** Get the underlying green token *)

  val offset : ('kind, 'text) syntax_token -> int
  (** Get the absolute offset *)

  val span : ('kind, 'text) syntax_token -> Span.t
  (** Get the span *)

  val kind : ('kind, 'text) syntax_token -> 'kind
  (** Get the syntax kind *)

  val text : ('kind, 'text) syntax_token -> 'text
  (** Get the text *)
end

val new_root : ('kind, 'text) Green.node -> ('kind, 'text) syntax_node
(** Create a new root node from a green node *)

val to_json :
  kind_to_json:('kind -> Data.Json.t) ->
  text_to_json:('text -> Data.Json.t) ->
  ('kind, 'text) syntax_element ->
  Data.Json.t
(** Convert to JSON *)
