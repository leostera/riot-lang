open Std

type t
val create: string -> t

val from_slice: IO.IoVec.IoSlice.t -> t

val source: t -> IO.IoVec.IoSlice.t

val position: t -> int

val is_eof: t -> bool

val peek: t -> char option

val peek_n: t -> int -> char option

val advance: t -> unit

val skip_while: t -> (char -> bool) -> unit

val take_slice: t -> (char -> bool) -> IO.IoVec.IoSlice.t

val take_while: t -> (char -> bool) -> string

val slice_view: t -> int -> int -> IO.IoVec.IoSlice.t

val slice: t -> int -> int -> string
