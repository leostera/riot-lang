type t
val local: stamp:int -> name:Surface_path.t -> t

val name: t -> Surface_path.t

val stamp: t -> int

val equal: t -> t -> bool

val compare: t -> t -> Std.Order.t

val to_string: t -> string

val serializer: t Serde.Ser.t
