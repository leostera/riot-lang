module IoSlice = IoVec.IoSlice

type t

type error = Error.t

val create: ?size:int -> unit -> (t, error) Result.t

val length: t -> int

val readable_bytes: t -> int

val capacity: t -> int

val writable_bytes: t -> int

val clear: t -> unit

val compact: t -> unit

val ensure_free: t -> int -> (unit, error) Result.t

val readable: t -> IoSlice.t

val writable: t -> IoSlice.t

val commit: t -> int -> (unit, error) Result.t

val consume: t -> len:int -> (unit, error) Result.t

val append_string: t -> string -> (unit, error) Result.t

val append_bytes: t -> bytes -> (unit, error) Result.t

val append_subbytes: t -> bytes -> off:int -> len:int -> (unit, error) Result.t

val append_substring: t -> string -> off:int -> len:int -> (unit, error) Result.t

val append_slice: t -> IoSlice.t -> (unit, error) Result.t

val append_subslice: t -> IoSlice.t -> off:int -> len:int -> (unit, error) Result.t

val get: t -> at:int -> (char, error) Result.t

val get_unchecked: t -> at:int -> char

val to_iovec: t -> IoVec.t

val to_bytes: t -> bytes

val to_string: t -> string
