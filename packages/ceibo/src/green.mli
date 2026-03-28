(** Green tree representation - immutable, lossless syntax trees *)

open Std
open Std.Collections

(** A token with syntax kind, text content, and character width *)
(** A node with syntax kind, computed width, and child elements *)
type ('kind, 'text) trivia = {
  kind : 'kind;
  text : 'text;
  width : int;
}
type ('kind, 'text) token = {
  kind : 'kind;
  text : 'text;
  width : int;
  leading_trivia : ('kind, 'text) trivia list;
}
type ('kind, 'text) node = {
  kind : 'kind;
  width : int;
  children : ('kind, 'text) element array;
}

(** Create a token with the given kind, text, and width *)
(** An element is either a token or a node *)
and ('kind, 'text) element =
  | Token of ('kind, 'text) token
  | Node of ('kind, 'text) node
val make_trivia : kind:'kind -> text:'text -> width:int -> ('kind, 'text) trivia
val make_token : leading_trivia:('kind, 'text) trivia list ->
kind:'kind ->
text:'text ->
width:int ->
('kind, 'text) token

(** Create a node with the given kind and children. Width is computed automatically. *)
val make_node : kind:'kind -> children:('kind, 'text) element array -> ('kind, 'text) node

(** Create a node from a list of elements *)
val make_node_list : kind:'kind -> ('kind, 'text) element list -> ('kind, 'text) node

(** Get the width of an element *)
val width : ('kind, 'text) element -> int
val trivia_width : ('kind, 'text) trivia -> int
val token_width : ('kind, 'text) token -> int
val token_full_width : ('kind, 'text) token -> int
val leading_trivia : ('kind, 'text) token -> ('kind, 'text) trivia list

(** Get the kind of an element *)
(** Get the text of an element (only for tokens) *)
val kind : ('kind, 'text) element -> 'kind

val text : ('kind, 'text) element -> 'text option

(** Check if an element is a token *)
val is_token : ('kind, 'text) element -> bool

(** Check if an element is a node *)
val is_node : ('kind, 'text) element -> bool

(** Replace a child at the given index *)
val replace_child : ('kind, 'text) node ->
index:int ->
child:('kind, 'text) element ->
('kind, 'text) node

(** Append a child to the node *)
val append_child : ('kind, 'text) node -> child:('kind, 'text) element -> ('kind, 'text) node

(** Get the number of children in a node *)
val child_count : ('kind, 'text) node -> int

(** Get the child at the given index *)
val child : ('kind, 'text) node -> int -> ('kind, 'text) element option

(** Get all children of a node *)
val children : ('kind, 'text) node -> ('kind, 'text) element array

(** Convert an element to JSON *)
val to_json : kind_to_json:('kind -> Data.Json.t) ->
text_to_json:('text -> Data.Json.t) ->
('kind, 'text) element ->
Data.Json.t
