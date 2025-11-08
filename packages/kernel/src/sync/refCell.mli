(** A cell with runtime borrow checking.

    RefCell enforces borrowing rules at runtime:
    - Any number of immutable borrows OR one mutable borrow
    - Never both immutable and mutable borrows simultaneously
    - Violations cause runtime exceptions (or return Error with try_* functions)

    Perfect for:
    - Shared mutable state between actors
    - Detecting data races at runtime
    - Enforcing exclusive access during critical sections
    - Debugging concurrent access patterns

    Example:
    {[
      let shared = RefCell.create [1; 2; 3]

      (* Safe immutable access *)
      RefCell.with_borrow shared (fun list ->
        Printf.printf "Length: %d\n" (List.length list)
      )

      (* Safe mutable access *)
      RefCell.with_borrow_mut shared (fun get set ->
        let list = get () in
        set (0 :: list)
      )

      (* Manual borrowing (must release!) *)
      let borrow = RefCell.borrow shared in
      (* ... use borrow ... *)
      RefCell.release_borrow borrow

      (* Try borrowing to handle conflicts *)
      match RefCell.try_borrow_mut shared with
      | Ok borrow_mut ->
          RefCell.set_mut borrow_mut [42];
          RefCell.release_borrow_mut borrow_mut
      | Error msg ->
          Printf.printf "Cannot borrow: %s\n" msg
    ]} *)
open Types

type 'a t
(** A mutable cell with runtime borrow checking *)

exception BorrowError of string
(** Raised when borrowing rules are violated *)

exception BorrowMutError of string
(** Raised when mutable borrowing rules are violated *)

val create : 'a -> 'a t
(** Create a new RefCell with the given value *)

(** {2 Borrowing} *)

type 'a borrow
(** An immutable borrow handle *)

type 'a borrow_mut
(** A mutable borrow handle *)

val borrow : 'a t -> 'a borrow
(** Borrow the value immutably. Multiple immutable borrows are allowed. Raises
    BorrowError if the cell is mutably borrowed. *)

val release_borrow : 'a borrow -> unit
(** Release an immutable borrow *)

val borrow_mut : 'a t -> 'a borrow_mut
(** Borrow the value mutably. Only one mutable borrow is allowed. Raises
    BorrowMutError if the cell is already borrowed. *)

val get_mut : 'a borrow_mut -> 'a
(** Get the value from a mutable borrow *)

val set_mut : 'a borrow_mut -> 'a -> unit
(** Set the value through a mutable borrow *)

val release_borrow_mut : 'a borrow_mut -> unit
(** Release a mutable borrow *)

(** {2 Safe accessors} *)

val with_borrow : 'a t -> ('a -> 'b) -> 'b
(** Safely borrow the value for the duration of a function *)

val with_borrow_mut : 'a t -> ((unit -> 'a) -> ('a -> unit) -> 'b) -> 'b
(** Safely mutably borrow the value for the duration of a function. The
    function receives a getter and setter. *)

(** {2 Try variants} *)

val try_borrow : 'a t -> ('a borrow, string) result
(** Try to borrow immutably, returning Error instead of raising *)

val try_borrow_mut : 'a t -> ('a borrow_mut, string) result
(** Try to borrow mutably, returning Error instead of raising *)

(** {2 Unsafe access} *)

val get_unchecked : 'a t -> 'a
(** Get the value without borrow checking (unsafe) *)

val set_unchecked : 'a t -> 'a -> unit
(** Set the value without borrow checking (unsafe) *)

(** {2 Query} *)

val is_borrowed : 'a t -> bool
(** Check if the cell is currently borrowed *)

val borrow_count : 'a t -> int
(** Get the current number of borrows *)
