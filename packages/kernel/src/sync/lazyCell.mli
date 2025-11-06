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