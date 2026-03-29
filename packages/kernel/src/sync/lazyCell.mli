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

(** A lazy cell that computes its value on first access *)
(** Create a lazy cell with the given initialization function *)
type 'a t
val create : (unit -> 'a) -> 'a t

(** Get the value, computing it if necessary (alias for force) *)
(** Force the computation and get the value *)
val get : 'a t -> 'a

(** Check if the value has been computed *)
val force : 'a t -> 'a

val is_initialized : 'a t -> bool

(** Take the value out if initialized, leaving it uninitialized *)
val take : 'a t -> 'a option