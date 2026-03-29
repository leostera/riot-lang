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

(** A mutable cell with runtime borrow checking *)
(** Raised when borrowing rules are violated *)
type 'a t
exception BorrowError of string

(** Raised when mutable borrowing rules are violated *)
exception BorrowMutError of string

(** Create a new RefCell with the given value *)
val create : 'a -> 'a t

(** {2 Borrowing} *)

(** An immutable borrow handle *)
(** A mutable borrow handle *)
type 'a borrow
(** Borrow the value immutably. Multiple immutable borrows are allowed. Raises
    BorrowError if the cell is mutably borrowed. *)
type 'a borrow_mut
val borrow : 'a t -> 'a borrow

(** Release an immutable borrow *)
val release_borrow : 'a borrow -> unit

(** Borrow the value mutably. Only one mutable borrow is allowed. Raises
    BorrowMutError if the cell is already borrowed. *)
val borrow_mut : 'a t -> 'a borrow_mut

(** Get the value from a mutable borrow *)
(** Set the value through a mutable borrow *)
val get_mut : 'a borrow_mut -> 'a

val set_mut : 'a borrow_mut -> 'a -> unit

(** Release a mutable borrow *)
val release_borrow_mut : 'a borrow_mut -> unit

(** {2 Safe accessors} *)

(** Safely borrow the value for the duration of a function *)
(** Safely mutably borrow the value for the duration of a function. The
    function receives a getter and setter. *)
val with_borrow : 'a t -> ('a -> 'b) -> 'b

val with_borrow_mut : 'a t -> ((unit -> 'a) -> ('a -> unit) -> 'b) -> 'b

(** {2 Try variants} *)

(** Try to borrow immutably, returning Error instead of raising *)
val try_borrow : 'a t -> ('a borrow, string) result

(** Try to borrow mutably, returning Error instead of raising *)
val try_borrow_mut : 'a t -> ('a borrow_mut, string) result

(** {2 Unsafe access} *)

(** Get the value without borrow checking (unsafe) *)
(** Set the value without borrow checking (unsafe) *)
val get_unchecked : 'a t -> 'a

val set_unchecked : 'a t -> 'a -> unit

(** {2 Query} *)

(** Check if the cell is currently borrowed *)
val is_borrowed : 'a t -> bool

(** Get the current number of borrows *)
val borrow_count : 'a t -> int
