(** A contiguous growable array type similar to Rust's Vec *)

type 'a t
(** The type of vectors containing elements of type ['a] *)

(** {1 Creation} *)

val create : unit -> 'a t
(** Create an empty vector *)

val with_capacity : int -> 'a t
(** Create an empty vector with a given initial capacity *)

val of_list : 'a list -> 'a t
(** Create a vector from a list of elements *)

(** {1 Basic Operations} *)

val push : 'a t -> 'a -> unit
(** [push vector value] adds an element to the end of the vector *)

val pop : 'a t -> 'a option
(** [pop vector] removes and returns the last element. Returns [Some element] if
    the vector is not empty, [None] otherwise. *)

val insert : 'a t -> int -> 'a -> unit
(** [insert vector index value] inserts an element at the given index *)

val remove : 'a t -> int -> 'a option
(** [remove vector index] removes and returns the element at the given index.
    Returns [Some element] if the index is valid, [None] otherwise. *)

val get : 'a t -> int -> 'a option
(** [get vector index] returns the element at the given index. Returns
    [Some element] if the index is valid, [None] otherwise. *)

val set : 'a t -> int -> 'a -> unit
(** [set vector index value] sets the element at the given index *)

(** {1 Collection Information} *)

val len : 'a t -> int
(** [len vector] returns the number of elements in the vector *)

val is_empty : 'a t -> bool
(** [is_empty vector] returns [true] if the vector contains no elements *)

val capacity : 'a t -> int
(** [capacity vector] returns the current capacity of the vector *)

val clear : 'a t -> unit
(** [clear vector] removes all elements from the vector *)

(** {1 Iteration} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f vector] applies function [f] to each element *)

val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** [fold f vector acc] folds over all elements *)

val to_list : 'a t -> 'a list
(** [to_list vector] returns all elements as a list *)

(** {1 Additional Operations} *)

val contains : 'a t -> 'a -> bool
(** [contains vector value] returns [true] if the value exists in the vector *)

val append : 'a t -> 'a t -> unit
(** [append vector1 vector2] moves all elements from [vector2] into [vector1] *)

val split_off : 'a t -> int -> 'a t
(** [split_off vector index] splits the vector into two at the given index.
    Returns a new vector containing elements from [index] onwards. *)

val sort : 'a t -> unit
(** [sort vector] sorts the vector in-place using the default comparison *)

val sort_by : 'a t -> ('a -> 'a -> int) -> unit
(** [sort_by vector compare] sorts the vector in-place using a custom comparison
    function *)

val reverse : 'a t -> unit
(** [reverse vector] reverses the order of elements in-place *)

val first : 'a t -> 'a option
(** [first vector] returns the first element without removing it *)

val last : 'a t -> 'a option
(** [last vector] returns the last element without removing it *)
