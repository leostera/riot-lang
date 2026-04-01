(** Builder for constructing green trees *)
open Std

(** A builder for constructing trees *)
(** Create a new builder *)
type ('kind, 'text) t
val create: unit -> ('kind, 'text) t

(** Add a token to the builder *)
val token: ('kind, 'text) t -> kind:'kind -> text:'text -> width:int -> ('kind, 'text) t

(** Add a token with explicit leading trivia to the builder *)
val token_with_leading_trivia:
  ('kind, 'text) t ->
  leading_trivia:('kind, 'text) Green.trivia list ->
  kind:'kind ->
  text:'text ->
  width:int ->
  ('kind, 'text) t

(** Start a new node *)
val start_node: ('kind, 'text) t -> kind:'kind -> ('kind, 'text) t

(** Finish the current node *)
val finish_node: ('kind, 'text) t -> ('kind, 'text) t

(** Build the final tree with a default root kind *)
val build: ('kind, 'text) t -> 'kind -> ('kind, 'text) Green.node

(** Make a token element *)
val make_token: kind:'kind -> text:'text -> width:int -> ('kind, 'text) Green.element

(** Make a token element with explicit leading trivia *)
val make_token_with_leading_trivia:
  leading_trivia:('kind, 'text) Green.trivia list ->
  kind:'kind ->
  text:'text ->
  width:int ->
  ('kind, 'text) Green.element

(** Make a node element from a list *)
val make_node: kind:'kind -> ('kind, 'text) Green.element list -> ('kind, 'text) Green.element
