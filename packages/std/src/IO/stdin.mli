open Prelude

module Buffer = Buffer
module IoVec = IoVec
module Reader = Reader

type error = Error.t
type t

val open_: ?chunk_size:int -> unit -> t

val read: t -> into:Buffer.t -> (int, error) result

val read_vectored: t -> into:IoVec.t -> (int, error) result

val to_reader: t -> error Reader.t
