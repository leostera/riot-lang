type t

val create : string -> t
val next : t -> Token.t
val tokenize : string -> Token.t list
