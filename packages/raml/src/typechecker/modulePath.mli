type t = Identifier of Identifier.t | Dot of t * string | Apply of t * t

val same : t -> t -> bool
val compare : t -> t -> int
val scope : t -> int
val name : ?paren:(string -> bool) -> t -> string
val head : t -> Identifier.t
val to_string : t -> string
