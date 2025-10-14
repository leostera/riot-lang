type t

val create : bytes -> t
val remaining : t -> int
val is_eof : t -> bool
val read_byte : t -> int option
val read_int32 : t -> int option
val read_int16 : t -> int option
val read_string : t -> string option
val read_bytes : t -> int -> bytes option
val read_cstring : t -> int -> string option
val position : t -> int
val set_position : t -> int -> unit
