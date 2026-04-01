(** Basic mutable cell for simple value storage. 
    
    A basic mutable cell containing a value of type 'a. Simple storage with no
    borrowing rules or initialization logic. *)
(** The type of cells containing values of type 'a *)
type 'a t
(** {1 Creation} *)
(** Create a new cell with the given value *)
val create: 'a -> 'a t

(** {1 Reading} *)
(** Get the current value of the cell *)
(** Operator for getting the value, similar to ref *)
val get: 'a t -> 'a

val ( ! ): 'a t -> 'a

(** {1 Writing} *)
(** Set the cell to a new value *)
val set: 'a t -> 'a -> unit
(** Operator for setting the value, similar to ref *)
val ( := ): 'a t -> 'a -> unit

(** {1 Updating} *)
(** Update the cell value using a function *)
val update: 'a t -> ('a -> 'a) -> unit
(** Replace the value in the cell, returning the old value *)

(** Take the value from the cell, replacing it with the default value *)
val replace: 'a t -> 'a -> 'a

val take: 'a t -> default:'a -> 'a

(** {1 Swapping} *)
(** Swap the values of two cells *)
val swap: 'a t -> 'a t -> unit

(** {1 Comparison} *)
(** Compare and swap: if the cell contains the expected value, set it to the new
    value and return true, otherwise return false *)
val compare_and_swap: 'a t -> 'a -> 'a -> bool
(** Check if two cells contain equal values *)
val equal: 'a t -> 'a t -> bool
(** Increment an integer cell *)
val incr: int t -> unit
(** Decrement an integer cell *)
val decr: int t -> unit
