(** Opaque timer identifier *)

type t
(** An opaque identifier for a timer. Can be used to cancel timers. *)

val make : unit -> t
(** Generate a new unique timer ID *)

val equal : t -> t -> bool
(** Test equality of two timer IDs *)

val compare : t -> t -> int
(** Compare two timer IDs *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a timer ID *)
