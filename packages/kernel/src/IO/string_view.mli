module IoSlice = Iovec.IoSlice

type t

val empty: t

val of_slice: IoSlice.t -> t

val of_string: string -> t

val of_buffer: Buffer.t -> t

val length: t -> int

val get: t -> at:int -> char

val sub: t -> offset:int -> len:int -> t

val advance: t -> by:int -> t

val starts_with: t -> prefix:string -> bool

val index_of_char: t -> char -> int option

val index_of_string: t -> string -> int option

val to_string: t -> string
