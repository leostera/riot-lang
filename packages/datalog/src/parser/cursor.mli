type t

val create : string -> t
val position : t -> int
val is_eof : t -> bool
val peek : t -> char option
val peek_n : t -> int -> char option
val advance : t -> unit
val skip_while : t -> (char -> bool) -> unit
val take_while : t -> (char -> bool) -> string
val slice : t -> int -> int -> string
val view : t -> Ceibo.Span.t -> string
