(** A double-ended queue implementation similar to Rust's VecDeque *)

type 'a t
(** The type of deques containing elements of type ['a] *)

(** {1 Creation} *)

val create : unit -> 'a t
(** Create an empty deque *)

val with_capacity : int -> 'a t
(** Create an empty deque with a given initial capacity *)

val of_list : 'a list -> 'a t
(** Create a deque from a list of elements (first element becomes front) *)

(** {1 Adding Elements} *)

val push_front : 'a t -> 'a -> unit
(** [push_front deque value] adds an element to the front of the deque *)

val push_back : 'a t -> 'a -> unit
(** [push_back deque value] adds an element to the back of the deque *)

val insert : 'a t -> int -> 'a -> unit
(** [insert deque index value] inserts an element at the given index *)

(** {1 Removing Elements} *)

val pop_front : 'a t -> 'a option
(** [pop_front deque] removes and returns the front element. Returns
    [Some element] if the deque is not empty, [None] otherwise. *)

val pop_back : 'a t -> 'a option
(** [pop_back deque] removes and returns the back element. Returns
    [Some element] if the deque is not empty, [None] otherwise. *)

val remove : 'a t -> int -> 'a option
(** [remove deque index] removes and returns the element at the given index.
    Returns [Some element] if the index is valid, [None] otherwise. *)

val clear : 'a t -> unit
(** [clear deque] removes all elements from the deque *)

(** {1 Accessing Elements} *)

val front : 'a t -> 'a option
(** [front deque] returns the front element without removing it. Returns
    [Some element] if the deque is not empty, [None] otherwise. *)

val back : 'a t -> 'a option
(** [back deque] returns the back element without removing it. Returns
    [Some element] if the deque is not empty, [None] otherwise. *)

val get : 'a t -> int -> 'a option
(** [get deque index] returns the element at the given index. Returns
    [Some element] if the index is valid, [None] otherwise. *)

(** {1 Collection Information} *)

val len : 'a t -> int
(** [len deque] returns the number of elements in the deque *)

val is_empty : 'a t -> bool
(** [is_empty deque] returns [true] if the deque contains no elements *)

(** {1 Iteration} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f deque] applies function [f] to each element from front to back *)

val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** [fold f deque acc] folds over all elements from front to back *)

val to_list : 'a t -> 'a list
(** [to_list deque] returns all elements as a list (front to back order) *)

(** {1 Additional Operations} *)

val contains : 'a t -> 'a -> bool
(** [contains deque value] returns [true] if the value exists in the deque *)

val append : 'a t -> 'a t -> unit
(** [append deque1 deque2] moves all elements from [deque2] into [deque1] *)

val split_off : 'a t -> int -> 'a t
(** [split_off deque index] splits the deque into two at the given index.
    Returns a new deque containing elements from [index] onwards. *)
