(** Event Handler Registry
    
    Manages mapping between handler IDs and event handler functions.
    Each LiveView instance has its own registry. *)

type 'msg t
(** Registry that maps handler IDs to event handler functions *)

val create : unit -> 'msg t
(** Create a new empty registry *)

val register : 'msg t -> (string -> 'msg) -> string
(** Register a handler and get a unique ID.
    
    Example:
    {[
      let id = register registry (fun _ -> Increment) in
      (* id = "lv-0" *)
    ]} *)

val find : 'msg t -> string -> (string -> 'msg) option
(** Find a handler by ID *)

val clear : 'msg t -> unit
(** Clear all handlers (useful for re-renders) *)

val size : 'msg t -> int
(** Get number of registered handlers *)
