open Prelude
open Types

module IoVec = IoVec

module type Read = sig
  type t
  type err

  val read: t -> into:Buffer.t -> (int, err) result

  val read_vectored: t -> into:IoVec.t -> (int, err) result

  val is_read_vectored: t -> bool
end

type ('src, 'err) source = (module Read with type t = 'src and type err = 'err)
type 'err t

type 'err exact_error =
  | Source_error of 'err
  | Unexpected_end_of_file

type 'err byte_result = (u8, 'err) result

val from_source: ('src, 'err) source -> 'src -> 'err t

val read: 'err t -> into:Buffer.t -> (int, 'err) result

val read_vectored: 'err t -> into:IoVec.t -> (int, 'err) result

val is_read_vectored: 'err t -> bool

val read_to_end: 'err t -> into:Buffer.t -> (int, 'err) result

val read_to_string: 'err t -> into:StringBuilder.t -> (int, 'err) result

val read_exact: 'err t -> into:Buffer.t -> len:int -> (unit, 'err exact_error) result

val bytes: 'err t -> 'err byte_result Iter.Iterator.t

val chain: 'err t -> 'err t -> 'err t

val take: 'err t -> limit:int -> 'err t

val map_err: 'a t -> fn:('a -> 'b) -> 'b t

val empty: unit t

val from_bytes: bytes -> unit t

val from_string: string -> unit t
