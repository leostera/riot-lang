type t = string
type utf_decode = Unicode.Rune.utf_decode
val empty: t

val is_empty: t -> bool

val length: t -> int

val get: t -> at:int -> char option

val get_unchecked: t -> at:int -> char

val sub: t -> offset:int -> len:int -> t

(** Use `init length builder` to construct a fresh string by calling `builder` for each index from
    left to right. *)
val init: len:int -> fn:(int -> char) -> t

(** Use `make length char` to fill a fresh string with repeated copies of `char`. *)
val make: len:int -> char:char -> t

(** Use `append left right` to concatenate two strings into a fresh result. *)
val append: t -> t -> t

(** Use `concat separator values` to join `values` with `separator` into a fresh result. *)
val concat: t -> t list -> t

val contains: t -> t -> bool

val starts_with: prefix:t -> t -> bool

val ends_with: suffix:t -> t -> bool

val equal: t -> t -> bool

val compare: t -> t -> int

val index_of: t -> char:char -> int option

val last_index: t -> char -> int option

val trim: t -> t

val split: by:t -> t -> t list

val lowercase_ascii: t -> t

val capitalize_ascii: t -> t

val uppercase_ascii: t -> t

val map: fn:(char -> char) -> t -> t

val for_each: fn:(char -> unit) -> t -> unit

val exists: fn:(char -> bool) -> t -> bool

val for_all: fn:(char -> bool) -> t -> bool

val fold_left: fn:('acc -> char -> 'acc) -> acc:'acc -> t -> 'acc

val escaped: t -> t

val get_utf_8_rune: t -> at:int -> utf_decode option

(** Use `from_bytes value` to copy `value` into a fresh immutable string. *)
val from_bytes: bytes -> t

(** Use `unsafe_from_bytes value` is basically a cast *)
val unsafe_from_bytes: bytes -> t

(** Use `to_bytes value` to copy `value` into fresh mutable bytes. *)
val to_bytes: t -> bytes
