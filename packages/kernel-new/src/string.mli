type t = string
val empty: t

val length: t -> int

val get: t -> int -> char

(** Use `init length builder` to construct a fresh string by calling `builder` for each index from
    left to right. *)
val init: int -> (int -> char) -> t

(** Use `make length char` to fill a fresh string with repeated copies of `char`. *)
val make: int -> char -> t

(** Use `append left right` to concatenate two strings into a fresh result. *)
val append: t -> t -> t

(** Use `concat separator values` to join `values` with `separator` into a fresh result. *)
val concat: t -> t list -> t

val equal: t -> t -> bool

val compare: t -> t -> int

(** Use `of_bytes value` to copy `value` into a fresh immutable string. *)
val of_bytes: bytes -> t

(** Use `to_bytes value` to copy `value` into fresh mutable bytes. *)
val to_bytes: t -> bytes
