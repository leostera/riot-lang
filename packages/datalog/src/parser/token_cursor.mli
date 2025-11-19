type t

val create : source:string -> Token.located list -> t
val position : t -> int
val is_eof : t -> bool
val peek : t -> Token.located
val peek_n : t -> int -> Token.located
val advance : t -> unit
val last_token : t -> Token.located
val view : t -> Ceibo.Span.t -> string
val set_position : t -> int -> unit
