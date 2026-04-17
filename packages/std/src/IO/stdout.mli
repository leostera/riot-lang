open Prelude

module Bytes = Bytes
module Iovec = Kernel.IO.Iovec

type t = unit
type error = Error.t

val write: ?offset:int -> ?len:int -> Bytes.t -> (int, error) result

val write_vectored: Iovec.t -> (int, error) result

val flush: unit -> (unit, error) result

val to_writer: unit -> (t, error) Writer.t
