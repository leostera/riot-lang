type t

val next : unit -> t
(** Get the next unique node ID *)

val eq : t -> t -> bool
(** Check if two node IDs are equal *)

val to_string : t -> string
(** Convert node ID to string for display/serialization *)

val to_int : t -> int
(** Convert to int for hashtable keys - internal use only *)