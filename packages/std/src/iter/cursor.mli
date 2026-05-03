(** Immutable slice cursor for parsing. *)
module IoSlice = Kernel.IO.IoVec.IoSlice

type t

val create: string -> t

val from_string: string -> t

val from_slice: IoSlice.t -> t

val source: t -> IoSlice.t

val source_string: t -> string

val position: t -> int

val length_remaining: t -> int

val is_eof: t -> bool

val peek: t -> char option

val peek_n: t -> int -> char option

val advance: t -> t option

val advance_by: t -> int -> t option

val take_while: t -> (char -> bool) -> IoSlice.t * t

val take_while_string: t -> (char -> bool) -> string * t

val skip_while: t -> (char -> bool) -> t

val take_until: t -> (char -> bool) -> (IoSlice.t * t) option

val take_until_string: t -> (char -> bool) -> (string * t) option

val take_until_char: t -> char -> (IoSlice.t * t) option

val take_until_char_string: t -> char -> (string * t) option

val take_n: t -> int -> (IoSlice.t * t) option

val take_n_string: t -> int -> (string * t) option

val remaining: t -> IoSlice.t

val remaining_string: t -> string
