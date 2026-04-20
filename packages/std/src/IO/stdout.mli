open Prelude

module Buffer = Buffer
module IoVec = IoVec

type t = unit
type error = Error.t

val write: from:Buffer.t -> (int, error) result

val write_vectored: from:IoVec.t -> (int, error) result

val flush: unit -> (unit, error) result

val to_writer: unit -> error Writer.t
