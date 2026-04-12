type t
val create: size:int -> t

val clear: t -> unit

val length: t -> int

val get: t -> at:int -> char option

val get_unchecked: t -> at:int -> char

val add_char: t -> char -> unit

val add_string: t -> string -> unit

val add_bytes: t -> Bytes.t -> unit

val add_subbytes: t -> Bytes.t -> int -> int -> unit

val add_substring: t -> string -> int -> int -> unit

val add_utf_8_uchar: t -> Kernel.Unicode.Rune.t -> unit

val contents: t -> string
