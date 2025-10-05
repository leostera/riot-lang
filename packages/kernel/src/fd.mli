type t = Unix.file_descr

val to_int : t -> int
val make : Unix.file_descr -> t
val close : t -> unit
val equal : t -> t -> bool
val to_string : t -> string
