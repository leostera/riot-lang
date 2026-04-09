type t = bytes
val create: int -> t

val length: t -> int

val get: t -> int -> char

val set: t -> int -> char -> unit

val blit: t -> int -> t -> int -> int -> unit

val fill: t -> int -> int -> char -> unit

val of_string: string -> t

val to_string: t -> string

val unsafe_of_string: string -> t

val unsafe_to_string: t -> string

val sub: t -> int -> int -> t

val sub_string: t -> int -> int -> string
