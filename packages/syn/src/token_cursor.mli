type t

val create : source:string -> Token.t list -> t
val position : t -> int
val is_eof : t -> bool

(* NOTE: if there are no more tokens you will always get Token.EOF *)
val peek : t -> Token.t

(* NOTE: if there are no more tokens you will always get Token.EOF *)
val peek_n : t -> int -> Token.t
val advance : t -> unit
val skip_while : t -> (Token.t -> bool) -> unit
val take_while : t -> (Token.t -> bool) -> Token.t list
val slice : t -> int -> int -> Token.t list

val view : t -> Ceibo.Span.t -> string
(** Get a substring view of the source at the given span *)
