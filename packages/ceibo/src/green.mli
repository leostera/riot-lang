(** Green tree representation - immutable, lossless syntax trees *)

open Std
open Std.Collections

type ('kind, 'text) token = { kind : 'kind; text : 'text; width : int }
(** A token with syntax kind, text content, and character width *)

type ('kind, 'text) node = {
  kind : 'kind;
  width : int;
  children : ('kind, 'text) element array;
}
(** A node with syntax kind, computed width, and child elements *)

and ('kind, 'text) element =
  | Token of ('kind, 'text) token
  | Node of ('kind, 'text) node
(** An element is either a token or a node *)

val make_token : kind:'kind -> text:'text -> width:int -> ('kind, 'text) token
(** Create a token with the given kind, text, and width *)

val make_node : kind:'kind -> children:('kind, 'text) element array -> ('kind, 'text) node
(** Create a node with the given kind and children. Width is computed automatically. *)

val make_node_list : kind:'kind -> ('kind, 'text) element list -> ('kind, 'text) node
(** Create a node from a list of elements *)

val width : ('kind, 'text) element -> int
(** Get the width of an element *)

val kind : ('kind, 'text) element -> 'kind
(** Get the kind of an element *)

val text : ('kind, 'text) element -> 'text option
(** Get the text of an element (only for tokens) *)

val is_token : ('kind, 'text) element -> bool
(** Check if an element is a token *)

val is_node : ('kind, 'text) element -> bool
(** Check if an element is a node *)

val replace_child : ('kind, 'text) node -> index:int -> child:('kind, 'text) element -> ('kind, 'text) node
(** Replace a child at the given index *)

val append_child : ('kind, 'text) node -> child:('kind, 'text) element -> ('kind, 'text) node
(** Append a child to the node *)

val child_count : ('kind, 'text) node -> int
(** Get the number of children in a node *)

val child : ('kind, 'text) node -> int -> ('kind, 'text) element option
(** Get the child at the given index *)

val children : ('kind, 'text) node -> ('kind, 'text) element array
(** Get all children of a node *)

val to_json :
  kind_to_json:('kind -> Data.Json.t) ->
  text_to_json:('text -> Data.Json.t) ->
  ('kind, 'text) element ->
  Data.Json.t
(** Convert an element to JSON *)
