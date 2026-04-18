module IoSlice = Iovec.IoSlice

type t

val create: ?size:int -> unit -> t

val length: t -> int

val capacity: t -> int

val clear: t -> unit

val append_string: t -> string -> unit

val append_bytes: t -> bytes -> unit

val append_slice: t -> IoSlice.t -> unit

val writable_slice: ?size:int -> t -> IoSlice.t

val commit_write: t -> len:int -> unit

val consume: t -> len:int -> unit

val readable_slice: t -> IoSlice.t

val to_iovec: t -> Iovec.t

val to_bytes: t -> bytes

val to_string: t -> string
