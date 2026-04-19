(** # Cursor - Immutable string cursor for parsing

    `Cursor` now uses an internal `StringView`-backed representation so
    navigation is cheap, while the public extraction APIs still materialize
    owned strings for compatibility. *)

type t

val create: string -> t

val from_string: string -> t

val source: t -> string

val position: t -> int

val length_remaining: t -> int

val is_eof: t -> bool

val peek: t -> char option

val peek_n: t -> int -> char option

val advance: t -> t option

val advance_by: t -> int -> t option

val take_while: t -> (char -> bool) -> string * t

val skip_while: t -> (char -> bool) -> t

val take_until: t -> (char -> bool) -> (string * t) option

val take_n: t -> int -> (string * t) option

val remaining: t -> string
