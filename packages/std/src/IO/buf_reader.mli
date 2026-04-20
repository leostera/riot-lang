open Prelude
open Types

module IoSlice = IoSlice

type 'value result = ('value, Error.t) Result.t

type t

val from_reader: ?size:int -> Reader.t -> t

val to_reader: t -> Reader.t

val read: t -> into:Buffer.t -> int result

val read_byte: t -> u8 result

val size: t -> int

val reset: t -> reader:Reader.t -> unit

val fill: t -> int result

val peek: t -> len:int -> IoSlice.t result

val consume: t -> len:int -> int result

val read_rune: t -> Kernel.Unicode.Rune.t result

val read_slice: t -> until:u8 -> IoSlice.t result

val read_line: t -> IoSlice.t result

val read_string: t -> until:u8 -> string result
