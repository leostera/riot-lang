(** A FIFO (first-in, first-out) queue implementation *)

type 'a t
(** The type of queues containing elements of type ['a] *)

(** {1 Creation} *)

val create : unit -> 'a t
(** Create an empty queue *)

val with_capacity : int -> 'a t
(** Create an empty queue with a given initial capacity *)

val of_list : 'a list -> 'a t
(** Create a queue from a list of elements (first element becomes front) *)

(** {1 Basic Operations} *)

val enqueue : 'a t -> 'a -> unit
(** [enqueue queue value] adds an element to the back of the queue *)

val dequeue : 'a t -> 'a option
(** [dequeue queue] removes and returns the front element. Returns
    [Some element] if the queue is not empty, [None] otherwise. *)

val front : 'a t -> 'a option
(** [front queue] returns the front element without removing it. Returns
    [Some element] if the queue is not empty, [None] otherwise. *)

val len : 'a t -> int
(** [len queue] returns the number of elements in the queue *)

val is_empty : 'a t -> bool
(** [is_empty queue] returns [true] if the queue contains no elements *)

val clear : 'a t -> unit
(** [clear queue] removes all elements from the queue *)

(** {1 Iteration} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f queue] applies function [f] to each element from front to back *)

val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** [fold f queue acc] folds over all elements from front to back *)

val to_list : 'a t -> 'a list
(** [to_list queue] returns all elements as a list (front to back order) *)

(** {1 Additional Operations} *)

val contains : 'a t -> 'a -> bool
(** [contains queue value] returns [true] if the value exists in the queue *)

val append : 'a t -> 'a t -> unit
(** [append queue1 queue2] moves all elements from [queue2] into [queue1] *)
