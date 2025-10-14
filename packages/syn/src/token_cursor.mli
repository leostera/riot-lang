type t

val create : source:string -> Token.t list -> t
val position : t -> int
val is_eof : t -> bool
val peek : t -> Token.t option
val peek_n : t -> int -> Token.t option
val advance : t -> unit
val skip_while : t -> (Token.t -> bool) -> unit
val take_while : t -> (Token.t -> bool) -> Token.t list
val slice : t -> int -> int -> Token.t list

val view : t -> Ceibo.Span.t -> string
(** Get a substring view of the source at the given span *)
