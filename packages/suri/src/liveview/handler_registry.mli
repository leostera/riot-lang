(**
   Event Handler Registry

   Manages mapping between handler IDs and event handler functions.
   Each LiveView instance has its own registry.
*)
(** Registry that maps handler IDs to event handler functions *)
(** Create a new empty registry *)
type 'msg t

val create: unit -> 'msg t

(**
   Register a handler and get a unique ID.

   Example:
   {[
     let id = register registry (fun _ -> Increment) in
     (* id = "lv-0" *)
   ]}
*)
val register: 'msg t -> (string -> 'msg) -> string

(** Find a handler by ID *)
val find: 'msg t -> string -> (string -> 'msg) option

(** Clear all handlers (useful for re-renders) *)
val clear: 'msg t -> unit

(** Get number of registered handlers *)
val size: 'msg t -> int
