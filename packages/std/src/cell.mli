(** # Cell - Mutable cell types for interior mutability

    This module provides several cell types for different use cases:

    - {b Cell.t}: Basic mutable cell for simple value storage
    - {b OnceCell}: Can only be set once, useful for configuration
    - {b LazyCell}: Automatically initializes on first access
    - {b RefCell}: Runtime-checked borrowing for safe shared access

    {2 When to use each cell type}

    - Use {b Cell.t} when you need simple mutable storage without sharing
      concerns
    - Use {b OnceCell} when you need to initialize a value once (e.g.,
      configuration, singletons)
    - Use {b LazyCell} for expensive computations that should be deferred until
      needed
    - Use {b RefCell} when multiple actors need coordinated access to shared
      data *)

type 'a t
(** A basic mutable cell containing a value of type 'a. Simple storage with no
    borrowing rules or initialization logic. *)

(** {1 Creation} *)

val create : 'a -> 'a t
(** Create a new cell with the given value *)

(** {1 Reading} *)

val get : 'a t -> 'a
(** Get the current value of the cell *)

val ( ! ) : 'a t -> 'a

(** {1 Writing} *)

val set : 'a t -> 'a -> unit
(** Set the cell to a new value *)

val ( := ) : 'a t -> 'a -> unit

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
val decr : int t -> unit

(** {1 OnceCell} *)

(** A cell that can only be set once - useful for one-time initialization.

    OnceCell is perfect for:
    - Global configuration that should only be set once
    - Singletons that need lazy initialization
    - Caching values that won't change after first computation

    Example:
    {[
      let config = OnceCell.create ()

      (* Later, in initialization code *)
      match OnceCell.set config (load_config ()) with
      | Ok () -> print_endline "Config loaded"
      | Error `AlreadyInitialized -> print_endline "Config already set"

      (* Access the config *)
      let cfg = OnceCell.get_or_init config (fun () -> default_config ())
    ]} *)
module OnceCell : sig
  type 'a t
  (** A cell that holds an optional value and can only be set once *)

  val create : unit -> 'a t
  (** Create an empty OnceCell *)

  val get : 'a t -> 'a option
  (** Get the value if initialized *)

  val get_or_init : 'a t -> (unit -> 'a) -> 'a
  (** Get the value, initializing it if necessary *)

  val get_or_try_init : 'a t -> (unit -> ('a, 'e) result) -> ('a, 'e) result
  (** Try to get the value, initializing it if necessary, propagating errors *)

  val set : 'a t -> 'a -> (unit, [ `AlreadyInitialized ]) result
  (** Set the value if not already set, returns error if already initialized *)

  val is_initialized : 'a t -> bool
  (** Check if the cell has been initialized *)

  val take : 'a t -> 'a option
  (** Take the value out of the cell, leaving it uninitialized *)
end

(** {1 LazyCell} *)

(** A cell that lazily initializes its value on first access.

    LazyCell differs from OnceCell in that the initialization function is
    provided at creation time and automatically called on first access.

    Perfect for:
    - Expensive computations that should be deferred
    - Resources that may not be needed
    - Circular dependencies that need lazy evaluation

    Example:
    {[
      (* The expensive computation is not run yet *)
      let data =
        LazyCell.create (fun () ->
            print_endline "Loading data...";
            expensive_data_load ())

      (* First access triggers computation *)
      let value = LazyCell.get data (* Prints "Loading data..." *)

      (* Subsequent accesses return cached value *)
      let value2 = LazyCell.get data (* No print, returns cached *)
    ]} *)
module LazyCell : sig
  type 'a t
  (** A lazy cell that computes its value on first access *)

  val create : (unit -> 'a) -> 'a t
  (** Create a lazy cell with the given initialization function *)

  val get : 'a t -> 'a
  (** Get the value, computing it if necessary (alias for force) *)

  val force : 'a t -> 'a
  (** Force the computation and get the value *)

  val is_initialized : 'a t -> bool
  (** Check if the value has been computed *)

  val take : 'a t -> 'a option
  (** Take the value out if initialized, leaving it uninitialized *)
end

(** {1 RefCell} *)

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
module RefCell : sig
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
end
