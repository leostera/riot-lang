(** Mutable slice cursor for parsing. *)
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

val advance: t -> unit

val advance_by: t -> int -> unit

val take_while: t -> (char -> bool) -> IoSlice.t

val take_while_string: t -> (char -> bool) -> string

val skip_while: t -> (char -> bool) -> unit

val take_until: t -> (char -> bool) -> IoSlice.t option

val take_until_string: t -> (char -> bool) -> string option

val take_until_char: t -> char -> IoSlice.t option

val take_until_char_string: t -> char -> string option

val take_n: t -> int -> IoSlice.t option

val take_n_string: t -> int -> string option

val remaining: t -> IoSlice.t

val remaining_string: t -> string
