(** Single source of truth for build node identifiers. *)

(** Abstract node identifier type *)

(** Create a node ID from a package *)
type t

val of_package: Package.t -> t

(** Convert to string representation *)
val to_string: t -> string

(** Compare two node IDs *)
val compare: t -> t -> Std.Order.t

(** Check if two node IDs are equal *)
val equal: t -> t -> bool
