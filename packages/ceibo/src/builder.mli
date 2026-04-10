(** Stateful builder for constructing green trees incrementally. *)
open Std

(** Builder state for an in-progress green tree. *)
type ('kind, 'text) t

(** Create an empty builder. *)
val create: unit -> ('kind, 'text) t

(** Append a token to the current node. *)
val token: ('kind, 'text) t -> kind:'kind -> text:'text -> width:int -> ('kind, 'text) t

(** Append a token with explicit leading trivia. *)
val token_with_leading_trivia:
  ('kind, 'text) t ->
  leading_trivia:('kind, 'text) Green.trivia list ->
  kind:'kind ->
  text:'text ->
  width:int ->
  ('kind, 'text) t

(** Start a new node and make it the current construction target. *)
val start_node: ('kind, 'text) t -> kind:'kind -> ('kind, 'text) t

(** Finish the current node and attach it to its parent. *)
val finish_node: ('kind, 'text) t -> ('kind, 'text) t

(** Finish the builder and return the root green node. *)
val build: ('kind, 'text) t -> 'kind -> ('kind, 'text) Green.node

(** Construct a token element directly. *)
val make_token: kind:'kind -> text:'text -> width:int -> ('kind, 'text) Green.element

(** Construct a token element with explicit leading trivia. *)
val make_token_with_leading_trivia:
  leading_trivia:('kind, 'text) Green.trivia list ->
  kind:'kind ->
  text:'text ->
  width:int ->
  ('kind, 'text) Green.element

(** Construct a node element from child elements. *)
val make_node: kind:'kind -> ('kind, 'text) Green.element list -> ('kind, 'text) Green.element
