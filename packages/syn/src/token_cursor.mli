type t
val create : source:string -> Token.t list -> t

val position : t -> int

val set_position : t -> int -> unit

val is_eof : t -> bool

(* NOTE: if there are no more tokens you will always get Token.EOF *)
val peek : t -> Token.t

(* NOTE: if there are no more tokens you will always get Token.EOF *)
val peek_n : t -> int -> Token.t

val advance : t -> unit

val skip_while : t -> (Token.t -> bool) -> unit

val take_while : t -> (Token.t -> bool) -> Token.t list

val slice : t -> int -> int -> Token.t list

(** Get a substring view of the source at the given span *)
val view : t -> Ceibo.Span.t -> string

(** Peek at the current token's unconsumed leading trivia. *)
val peek_leading_trivia : t -> Token.trivia list

(** Consume the current token's leading trivia as synthetic trivia tokens. *)
val consume_leading_trivia : t -> Token.t list

(** Get the last consumed non-trivia token. Returns first token if at start. *)
val last_token : t -> Token.t
