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


open Global0

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
