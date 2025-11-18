open Std

(** {1 Relation - Lazy Sorted Iterator}
    
    A Relation is a lazy, sorted, deduplicated iterator over tuples.
    This is the core data structure for high-performance streaming Datalog evaluation.
    
    Key properties:
    - Lazy: Operations don't materialize unless explicitly requested
    - Sorted: Elements maintain sorted order (caller's responsibility for input)
    - Deduplicated: No duplicate elements
    - Streaming: O(1) memory for set operations
    
    INVARIANT: Input iterators MUST yield elements in sorted order.
    Violating this will cause incorrect results from set operations.
*)

type 'a t = 'a Iter.MutIterator.t
(** A lazy sorted, deduplicated iterator *)

(** {2 Construction} *)

val empty : unit -> 'a t
(** Create an empty relation *)

val of_iter : 'a Iter.MutIterator.t -> 'a t
(** Create relation from SORTED iterator. Deduplicates automatically.
    
    PRECONDITION: Iterator MUST yield elements in sorted order.
    This function only deduplicates - it does not sort.
    
    Time: O(1) to create, O(n) to consume
    Space: O(1) *)

val of_list : 'a list -> 'a t
(** Create relation from a list.
    
    Automatically sorts and deduplicates the list.
    Useful for tests and small datasets.
    
    For large datasets, prefer of_iter with pre-sorted data.
    Time: O(n log n) for sort, O(n) to consume
    Space: O(n) for materialized list *)

val singleton : 'a -> 'a t
(** Create relation with single element *)

(** {2 Access} *)

val to_list : 'a t -> 'a list
(** Materialize to list (preserves sorted order).
    Time: O(n), Space: O(n) *)

val length : 'a t -> int
(** Count elements (consumes iterator).
    Time: O(n), Space: O(1) *)

val is_empty : 'a t -> bool
(** Check if relation is empty (peeks at first element).
    Time: O(1) *)

(** {2 Set Operations - All Lazy}
    
    These operations stream through inputs without materializing.
    Time: O(1) to create, O(n + m) to consume
    Space: O(1) - just peek buffers *)

val merge : 'a t -> 'a t -> 'a t
(** Union of two sorted relations - lazy streaming merge *)

val diff : 'a t -> 'a t -> 'a t
(** Set difference: elements in first but not in second - lazy streaming diff *)

val intersect : 'a t -> 'a t -> 'a t
(** Set intersection: elements in both - lazy streaming intersect *)

(** {2 Iteration - All Lazy} *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate over elements in sorted order.
    Time: O(n), Space: O(1) *)

val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
(** Fold over elements.
    Time: O(n), Space: O(1) *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map over elements.
    WARNING: If mapping function doesn't preserve sort order, results will be incorrect!
    Time: O(1) to create, O(n) to consume *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter elements (preserves sorted order).
    Time: O(1) to create, O(n) to consume *)

(** {2 Search} *)

val contains : 'a t -> 'a -> bool
(** Check if element exists. Linear search through sorted sequence.
    Time: O(n) worst case, O(k) average (stops when passed target) *)

val find : ('a -> bool) -> 'a t -> 'a option
(** Find first element matching predicate.
    Time: O(k) where k is position of match *)
