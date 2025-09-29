(** A hash set implementation similar to Rust's HashSet *)

type 'a t
(** The type of hash sets containing elements of type ['a] *)

(** {1 Creation} *)

val create : unit -> 'a t
(** Create an empty hash set *)

val with_capacity : int -> 'a t
(** Create an empty hash set with a given initial capacity *)

val of_list : 'a list -> 'a t
(** Create a hash set from a list of elements *)

(** {1 Basic Operations} *)

val insert : 'a t -> 'a -> bool
(** [insert set value] adds a value to the set. Returns [true] if the value was
    newly inserted, [false] if it already existed. *)

val remove : 'a t -> 'a -> bool
(** [remove set value] removes a value from the set. Returns [true] if the value
    was present, [false] otherwise. *)

val contains : 'a t -> 'a -> bool
(** [contains set value] returns [true] if the value exists in the set *)

val len : 'a t -> int
(** [len set] returns the number of elements in the set *)

val is_empty : 'a t -> bool
(** [is_empty set] returns [true] if the set contains no elements *)

val clear : 'a t -> unit
(** [clear set] removes all elements from the set *)

(** {1 Iteration} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f set] applies function [f] to each element *)

val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** [fold f set acc] folds over all elements *)

val to_list : 'a t -> 'a list
(** [to_list set] returns all elements as a list *)

(** {1 Set Operations} *)

val union : 'a t -> 'a t -> 'a t
(** [union set1 set2] returns a new set containing elements from both sets *)

val intersection : 'a t -> 'a t -> 'a t
(** [intersection set1 set2] returns a new set containing elements in both sets
*)

val difference : 'a t -> 'a t -> 'a t
(** [difference set1 set2] returns a new set containing elements in [set1] but
    not in [set2] *)

val symmetric_difference : 'a t -> 'a t -> 'a t
(** [symmetric_difference set1 set2] returns a new set containing elements in
    either set, but not both *)

val is_subset : 'a t -> 'a t -> bool
(** [is_subset set1 set2] returns [true] if [set1] is a subset of [set2] *)

val is_superset : 'a t -> 'a t -> bool
(** [is_superset set1 set2] returns [true] if [set1] is a superset of [set2] *)

val is_disjoint : 'a t -> 'a t -> bool
(** [is_disjoint set1 set2] returns [true] if the sets have no common elements
*)
