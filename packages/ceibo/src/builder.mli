(** Stateful builder for constructing green trees incrementally. *)
open Std

(** Builder state for an in-progress green tree. *)
type ('kind, 'text) t

(** Create an empty builder. *)
val create: unit -> ('kind, 'text) t

(** Append a token to the current node. *)
val token:
  ('kind, 'text) t ->
  (** Token kind. *)
  kind:'kind ->
  (** Token text. *)
  text:'text ->
  (** Token width in source units. *)
  width:int ->
  ('kind, 'text) t

(** Append a token with explicit leading trivia. *)
val token_with_leading_trivia:
  ('kind, 'text) t ->
  (** Trivia that should appear immediately before the token body. *)
  leading_trivia:('kind, 'text) Green.trivia list ->
  (** Token kind. *)
  kind:'kind ->
  (** Token text. *)
  text:'text ->
  (** Token width in source units. *)
  width:int ->
  ('kind, 'text) t

(** Start a new node and make it the current construction target. *)
val start_node:
  ('kind, 'text) t ->
  (** Node kind. *)
  kind:'kind ->
  ('kind, 'text) t

(** Finish the current node and attach it to its parent. *)
val finish_node: ('kind, 'text) t -> ('kind, 'text) t

(** Finish the builder and return the root green node. *)
val build:
  ('kind, 'text) t ->
  (** Root kind to use for the completed tree. *)
  'kind ->
  ('kind, 'text) Green.node

(** Construct a token element directly. *)
val make_token:
  (** Token kind. *)
  kind:'kind ->
  (** Token text. *)
  text:'text ->
  (** Token width in source units. *)
  width:int ->
  ('kind, 'text) Green.element

(** Construct a token element with explicit leading trivia. *)
val make_token_with_leading_trivia:
  (** Trivia that should appear immediately before the token body. *)
  leading_trivia:('kind, 'text) Green.trivia list ->
  (** Token kind. *)
  kind:'kind ->
  (** Token text. *)
  text:'text ->
  (** Token width in source units. *)
  width:int ->
  ('kind, 'text) Green.element

(** Construct a node element from child elements. *)
val make_node:
  (** Node kind. *)
  kind:'kind ->
  (** Child elements for the new node. *)
  ('kind, 'text) Green.element list ->
  ('kind, 'text) Green.element
