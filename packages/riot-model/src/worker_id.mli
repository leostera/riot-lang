(** Opaque worker ID type for type safety *)

(** Opaque type representing a worker ID *)

(** Create a worker ID from an integer *)
type t
val make: int -> t

(** Convert worker ID to string for display *)
val to_string: t -> string

(** Convert worker ID back to integer (for internal use) *)
val to_int: t -> int

(** Check if two worker IDs are equal *)
val equal: t -> t -> bool

(** Compare two worker IDs *)
val compare: t -> t -> Std.Order.t
