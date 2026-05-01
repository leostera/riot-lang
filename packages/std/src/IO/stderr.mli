open Prelude

module Buffer = Buffer

module IoVec = IoVec

type t = unit
type error = Error.t
type 'value result = ('value, error) Result.t

val write: from:Buffer.t -> int result

val write_vectored: from:IoVec.t -> int result

val flush: unit -> unit result

val to_writer: unit -> Writer.t
