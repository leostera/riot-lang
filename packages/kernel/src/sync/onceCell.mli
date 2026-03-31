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

(** A cell that holds an optional value and can only be set once *)
(** Create an empty OnceCell *)
type 'a t
val create : unit -> 'a t

(** Get the value if initialized *)
val get : 'a t -> 'a option

(** Get the value, initializing it if necessary *)

(** Try to get the value, initializing it if necessary, propagating errors *)
val get_or_init : 'a t -> (unit -> 'a) -> 'a

val get_or_try_init : 'a t -> (unit -> ('a, 'e) result) -> ('a, 'e) result

(** Set the value if not already set, returns error if already initialized *)
val set : 'a t -> 'a -> (unit, [
  | `AlreadyInitialized
]) result

(** Check if the cell has been initialized *)
val is_initialized : 'a t -> bool

(** Take the value out of the cell, leaving it uninitialized *)
val take : 'a t -> 'a option
