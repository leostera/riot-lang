(** Log handler management *)

type id = string
(** Handler identifier *)

type t = { id : id; fn : Event.t -> unit }
(** A handler with an ID and a callback function *)

val emit : Event.t -> unit
(** Emit an event to all registered handlers.
    Handlers are called synchronously in the caller process.
    If a handler crashes, the exception is caught and ignored. *)

val attach : id -> (Event.t -> unit) -> unit
(** Attach a handler with the given ID and callback function *)

val detach : id -> unit
(** Detach a handler by ID *)

val detach_all : unit -> unit
(** Detach all handlers *)

val list : unit -> id list
(** List all registered handler IDs *)
