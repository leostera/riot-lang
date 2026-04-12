(** # MutCursor - Mutable string cursor for parsing *)

type t
val create: string -> t

val source: t -> string

val position: t -> int

val length_remaining: t -> int

val is_eof: t -> bool

val peek: t -> char option

val peek_n: t -> int -> char option

val advance: t -> unit

val advance_by: t -> int -> unit

val take_while: t -> (char -> bool) -> string

val skip_while: t -> (char -> bool) -> unit

val take_until: t -> (char -> bool) -> string option

val take_n: t -> int -> string option

val remaining: t -> string
