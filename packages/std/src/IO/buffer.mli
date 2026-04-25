module IoSlice = IoSlice

module IoVec = IoVec

type t

type error = Kernel.IO.Error.t

val create: size:int -> t

val from_string: string -> t

val from_bytes: Kernel.Bytes.t -> t

val from_slice: IoSlice.t -> t

val create_result: ?size:int -> unit -> (t, error) Result.t

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

val append_bytes: t -> Kernel.Bytes.t -> (unit, error) Result.t

val append_subbytes: t -> Kernel.Bytes.t -> off:int -> len:int -> (unit, error) Result.t

val append_substring: t -> string -> off:int -> len:int -> (unit, error) Result.t

val append_slice: t -> IoSlice.t -> (unit, error) Result.t

val append_subslice: t -> IoSlice.t -> off:int -> len:int -> (unit, error) Result.t

val to_iovec: t -> IoVec.t

val to_bytes: t -> Kernel.Bytes.t

val to_string: t -> string

val contents: t -> string

val get: t -> at:int -> char option

val get_unchecked: t -> at:int -> char

val add_char: t -> char -> unit

val add_string: t -> string -> unit

val add_bytes: t -> Kernel.Bytes.t -> unit

val add_subbytes: t -> Kernel.Bytes.t -> int -> int -> unit

val add_substring: t -> string -> int -> int -> unit

val add_utf_8_uchar: t -> Kernel.Unicode.Rune.t -> unit
