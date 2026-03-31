(** Opaque timer identifier *)
(** An opaque identifier for a timer. Can be used to cancel timers. *)
(** Generate a new unique timer ID *)
type t
val make : unit -> t

(** Test equality of two timer IDs *)
val equal : t -> t -> bool

(** Compare two timer IDs *)
val compare : t -> t -> int
