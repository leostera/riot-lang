open Std

(** {1 Relation - Sorted Tuple Storage}
    
    A Relation is an immutable, sorted set of tuples with no duplicates.
    This is the core data structure for high-performance Datalog evaluation.
    
    Key properties:
    - Immutable: All operations return new relations
    - Sorted: Elements maintain sorted order
    - Deduplicated: No duplicate elements
    - Fast merge: O(n + m) union of sorted sets
    
    Inspired by Datafrog's Relation type.
*)

type 'a t
(** An immutable sorted set of elements *)

(** {2 Construction} *)

val empty : unit -> 'a t
(** Create an empty relation *)

val of_list : 'a list -> 'a t
(** Create relation from list. Automatically sorts and deduplicates.
    Time: O(n log n) *)

val of_vec : 'a Collections.Vector.t -> 'a t
(** Create relation from vector. Sorts and deduplicates.
    Time: O(n log n) *)

val singleton : 'a -> 'a t
(** Create relation with single element *)

(** {2 Access} *)

val to_list : 'a t -> 'a list
(** Convert to list (preserves sorted order) *)

val to_vec : 'a t -> 'a Collections.Vector.t
(** Get underlying vector (read-only view) *)

val length : 'a t -> int
(** Number of elements *)

val is_empty : 'a t -> bool
(** Check if relation is empty *)

(** {2 Set Operations} *)

val merge : 'a t -> 'a t -> 'a t
(** Union of two relations. Both inputs must be sorted.
    Time: O(n + m) - Fast sorted merge! *)

val diff : 'a t -> 'a t -> 'a t
(** Set difference: elements in first but not in second.
    Time: O(n + m) *)

val intersect : 'a t -> 'a t -> 'a t
(** Set intersection: elements in both relations.
    Time: O(n + m) *)

(** {2 Iteration} *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate over elements in sorted order *)

val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
(** Fold over elements *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map and create new relation. Result is sorted and deduplicated.
    Time: O(n log n) *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter elements. Result is still sorted.
    Time: O(n) *)

(** {2 Search} *)

val contains : 'a t -> 'a -> bool
(** Check if element exists. Uses binary search.
    Time: O(log n) *)

val find : ('a -> bool) -> 'a t -> 'a option
(** Find first element matching predicate *)
