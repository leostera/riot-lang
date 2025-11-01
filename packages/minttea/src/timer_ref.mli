(** Timer reference for unique timer identification *)

type t
(** An opaque timer reference *)

val make : unit -> t
(** Create a new unique timer reference *)

val equal : t -> t -> bool
(** Test equality of two timer references *)
