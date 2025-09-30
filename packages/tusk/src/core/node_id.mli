open Model
(** Node identifier - ensures single source of truth for build nodes *)

type t
(** Abstract node identifier type *)

val of_package : Workspace.package -> t
(** Create a node ID from a package *)

val to_string : t -> string
(** Convert to string representation *)

val compare : t -> t -> int
(** Compare two node IDs *)

val equal : t -> t -> bool
(** Check if two node IDs are equal *)

val pp : Format.formatter -> t -> unit
(** Pretty printer for node IDs *)
