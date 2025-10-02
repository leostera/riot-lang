(** Extended list utilities *)

include module type of Stdlib.List

val make : len:int -> fn:(int -> 'a) -> 'a list
(** Make a new list of length `len` using `fn` *)

val find_map : ('a -> 'b option) -> 'a list -> 'b option
(** Find and return the first element matching predicate *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** Filter and map in one pass *)

val split_at : int -> 'a list -> 'a list * 'a list
(** Split list at index n *)

val take : int -> 'a list -> 'a list
(** Take first n elements *)

val drop : int -> 'a list -> 'a list
(** Drop first n elements *)

val take_while : ('a -> bool) -> 'a list -> 'a list
(** Take elements while predicate is true *)

val drop_while : ('a -> bool) -> 'a list -> 'a list
(** Drop elements while predicate is true *)

val group : ('a -> 'a -> bool) -> 'a list -> 'a list list
(** Group consecutive equal elements *)

val uniq : ('a -> 'a -> bool) -> 'a list -> 'a list
(** Return list without duplicates *)

val intersperse : 'a -> 'a list -> 'a list
(** Intersperse element between list elements *)

val is_empty : 'a list -> bool
(** Check if list is empty *)

val last : 'a list -> 'a option
(** Get last element *)

val init : 'a list -> 'a list option
(** Get all but last element *)
