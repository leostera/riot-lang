(** Builder for constructing green trees *)

open Std

type ('kind, 'text) t
(** A builder for constructing trees *)

val create : unit -> ('kind, 'text) t
(** Create a new builder *)

val token : ('kind, 'text) t -> kind:'kind -> text:'text -> width:int -> ('kind, 'text) t
(** Add a token to the builder *)

val start_node : ('kind, 'text) t -> kind:'kind -> ('kind, 'text) t
(** Start a new node *)

val finish_node : ('kind, 'text) t -> ('kind, 'text) t
(** Finish the current node *)

val build : ('kind, 'text) t -> 'kind -> ('kind, 'text) Green.node
(** Build the final tree with a default root kind *)

val make_token : kind:'kind -> text:'text -> width:int -> ('kind, 'text) Green.element
(** Make a token element *)

val make_node : kind:'kind -> ('kind, 'text) Green.element list -> ('kind, 'text) Green.element
(** Make a node element from a list *)
