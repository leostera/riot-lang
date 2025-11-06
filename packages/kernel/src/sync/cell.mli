(** Basic mutable cell for simple value storage. 
    
    A basic mutable cell containing a value of type 'a. Simple storage with no
    borrowing rules or initialization logic. *)

type 'a t
(** The type of cells containing values of type 'a *)

(** {1 Creation} *)

val create : 'a -> 'a t
(** Create a new cell with the given value *)

(** {1 Reading} *)

val get : 'a t -> 'a
(** Get the current value of the cell *)

val ( ! ) : 'a t -> 'a
(** Operator for getting the value, similar to ref *)

(** {1 Writing} *)

val set : 'a t -> 'a -> unit
(** Set the cell to a new value *)

val ( := ) : 'a t -> 'a -> unit
(** Operator for setting the value, similar to ref *)

(** {1 Updating} *)

val update : 'a t -> ('a -> 'a) -> unit
(** Update the cell value using a function *)

val replace : 'a t -> 'a -> 'a
(** Replace the value in the cell, returning the old value *)

val take : 'a t -> default:'a -> 'a
(** Take the value from the cell, replacing it with the default value *)

(** {1 Swapping} *)

val swap : 'a t -> 'a t -> unit
(** Swap the values of two cells *)

(** {1 Comparison} *)

val compare_and_swap : 'a t -> 'a -> 'a -> bool
(** Compare and swap: if the cell contains the expected value, set it to the new
    value and return true, otherwise return false *)

val equal : 'a t -> 'a t -> bool
(** Check if two cells contain equal values *)

val incr : int t -> unit
(** Increment an integer cell *)

val decr : int t -> unit
(** Decrement an integer cell *)