type t = Token of Token.t | Tree of Token.delimiter * t list

val of_tokens : Token.t list -> t list
val to_string : t -> string
val list_to_string : t list -> string
