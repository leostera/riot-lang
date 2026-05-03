(** Log handler management *)

(** Handler identifier *)
type id = string
(** A handler with an ID and a callback function *)
type t = {
  id: id;
  fn: Event.t -> unit;
}

(**
   Emit an event to all registered handlers.
   Handlers are called synchronously in the caller process.
   If a handler crashes, the exception is caught and ignored.
*)
val emit: Event.t -> unit

(** Attach a handler with the given ID and callback function *)
val attach: id -> (Event.t -> unit) -> unit

(** Detach a handler by ID *)
val detach: id -> unit

(** Detach all handlers *)
val detach_all: unit -> unit

(** List all registered handler IDs *)
val list: unit -> id list
