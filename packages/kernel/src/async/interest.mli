type t = Non_zero_int.t

val readable: t

val writable: t

val priority: t

val add: t -> t -> t

val remove: t -> t -> t option

val is_readable: t -> bool

val is_writable: t -> bool

val is_priority: t -> bool
