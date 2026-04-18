module IoSlice = Iovec.IoSlice

type t
type error = Error.t

val empty: t

val from_slice: IoSlice.t -> t

val from_string: string -> (t, error) Result.t

val from_buffer: Buffer.t -> t

val to_slice: t -> IoSlice.t

val length: t -> int

val get: t -> at:int -> (char, error) Result.t

val get_unchecked: t -> at:int -> char

val sub: t -> off:int -> len:int -> (t, error) Result.t

val shift: t -> int -> (t, error) Result.t

val split_at: t -> int -> ((t * t), error) Result.t

val starts_with: t -> prefix:string -> bool

val equal_string: t -> string -> bool

val index_char: t -> char -> int option

val index_string: t -> string -> int option

val to_string: t -> string
