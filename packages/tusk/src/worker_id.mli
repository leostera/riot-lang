(** Opaque worker ID type for type safety *)

type t
(** Opaque type representing a worker ID *)

val make : int -> t
(** Create a worker ID from an integer *)

val to_string : t -> string
(** Convert worker ID to string for display *)

val to_int : t -> int
(** Convert worker ID back to integer (for internal use) *)

val equal : t -> t -> bool
(** Check if two worker IDs are equal *)

val compare : t -> t -> int
(** Compare two worker IDs *)
