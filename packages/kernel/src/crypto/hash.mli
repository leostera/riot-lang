(** Core hash type and operations *)
(** Create a hash from bytes *)
type t
val of_bytes: bytes -> t
(** Get the raw bytes of a hash *)
val to_bytes: t -> bytes
(** Get the length of a hash in bytes *)
val length: t -> int
(** Check if two hashes are equal *)
val equal: t -> t -> bool
(** Compare two hashes *)
val compare: t -> t -> int
