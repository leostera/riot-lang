type t

val create: size:int -> t

val length: t -> int

val blit_from_bytes:
  bytes ->
  src_offset:int ->
  dst:t ->
  dst_offset:int ->
  len:int ->
  unit

val blit_to_bytes: t -> dst:bytes -> dst_offset:int -> unit

val blit_from_string:
  string ->
  src_offset:int ->
  dst:t ->
  dst_offset:int ->
  len:int ->
  unit

val sub: t -> offset:int -> len:int -> t

val to_string: t -> string
