(** Core hash type and operations *)

type t

val of_bytes : bytes -> t
(** Create a hash from bytes *)

val to_bytes : t -> bytes
(** Get the raw bytes of a hash *)

val length : t -> int
(** Get the length of a hash in bytes *)

val equal : t -> t -> bool
(** Check if two hashes are equal *)

val compare : t -> t -> int
(** Compare two hashes *)
