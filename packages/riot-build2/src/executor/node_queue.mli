type t

val create: unit -> t

val push: t -> Work_node.t -> unit

val pop: t -> Work_node.t option

val is_empty: t -> bool
