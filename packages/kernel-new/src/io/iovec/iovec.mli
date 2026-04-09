type segment = {
  buffer: bytes;
  offset: int;
  length: int;
}

type t

val create: ?count:int -> size:int -> unit -> t

val with_capacity: int -> t

val of_bytes: bytes -> t

val of_string: string -> t

val of_bytes_array: bytes array -> t

val of_string_array: string array -> t

val length: t -> int

val iter: (segment -> unit) -> t -> unit

val sub: ?pos:int -> len:int -> t -> t

val into_bytes: t -> bytes

val into_string: t -> string
