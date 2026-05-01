open Prelude
open Types

module IoVec = IoVec

type 'value result = ('value, Error.t) Result.t

module type Read = sig
  type t

  val read: t -> into:Buffer.t -> int result

  val read_vectored: t -> into:IoVec.t -> int result

  val is_read_vectored: t -> bool
end

type 'src source = (module Read with type t = 'src)
type t

val from_source: 'src source -> 'src -> t

val read: t -> into:Buffer.t -> int result

val read_vectored: t -> into:IoVec.t -> int result

val is_read_vectored: t -> bool

val read_to_end: t -> into:Buffer.t -> int result

val read_to_string: t -> into:StringBuilder.t -> int result

val read_exact: t -> into:Buffer.t -> len:int -> unit result

val bytes: t -> u8 result Iter.Iterator.t

val chain: t -> t -> t

val take: t -> limit:int -> t

val empty: t

val from_bytes: bytes -> t

val from_string: string -> t
