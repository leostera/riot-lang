type t = string
val empty: t

val length: t -> int

val get: t -> int -> char

val init: int -> (int -> char) -> t

val make: int -> char -> t

val append: t -> t -> t

val concat: t -> t list -> t

val equal: t -> t -> bool

val compare: t -> t -> int

(** Use `of_bytes value` to copy `value` into a fresh immutable string. *)
val of_bytes: bytes -> t

(** Use `to_bytes value` to copy `value` into fresh mutable bytes. *)
val to_bytes: t -> bytes
