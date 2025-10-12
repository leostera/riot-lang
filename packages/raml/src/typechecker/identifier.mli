type t =
  | Local of { name : string; stamp : int }
  | Scoped of { name : string; stamp : int; scope : int }
  | Global of string
  | Predef of { name : string; stamp : int }

type context = { stamp_counter : int }

val create_context : unit -> context
val create_scoped : ctx:context -> scope:int -> string -> t * context
val create_local : ctx:context -> string -> t * context
val create_predef : ctx:context -> string -> t * context
val create_persistent : string -> t
val name : t -> string
val rename : ctx:context -> t -> t * context
val unique_name : t -> string
val persistent : t -> bool
val equal : t -> t -> bool
val same : t -> t -> bool
val compare : t -> t -> int
val scope : t -> int
val pp : Format.formatter -> t -> unit
