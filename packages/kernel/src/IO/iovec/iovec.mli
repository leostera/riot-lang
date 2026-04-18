module IoSlice: sig
  type t

  val create: size:int -> t

  val length: t -> int

  val get: t -> at:int -> char

  val blit_from_bytes:
    bytes ->
    src_offset:int ->
    dst:t ->
    dst_offset:int ->
    len:int ->
    unit

  val blit:
    src:t ->
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
end

(** Scatter/gather byte slices for narrow kernel I/O paths. *)
type segment = IoSlice.t
type t
val create: ?count:int -> size:int -> unit -> t

val with_capacity: int -> t

val from_slices: segment array -> t

val from_bytes: bytes -> t

val from_string: string -> t

val from_bytes_array: bytes array -> t

val from_string_array: string array -> t

val length: t -> int

val for_each: fn:(segment -> unit) -> t -> unit

val sub: ?pos:int -> len:int -> t -> t

val to_bytes: t -> bytes

val to_string: t -> string
