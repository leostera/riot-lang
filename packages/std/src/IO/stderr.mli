open Prelude

module Bytes = Bytes
module Iovec = Kernel.IO.Iovec

type error = Error.t

val write: ?offset:int -> ?len:int -> Bytes.t -> (int, error) result

val write_vectored: Iovec.t -> (int, error) result

val flush: unit -> (unit, error) result
