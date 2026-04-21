open Prelude

type 'value result = ('value, Error.t) Result.t
module type Write = sig
  type t
  val write: t -> from:Buffer.t -> int result

  val write_vectored: t -> from:IoVec.t -> int result

  val flush: t -> unit result
end

type 'dst sink = (module Write with type t = 'dst)
type t
val from_sink: 'dst sink -> 'dst -> t

val write: t -> from:Buffer.t -> int result

val write_all: t -> from:Buffer.t -> unit result

val write_vectored: t -> from:IoVec.t -> int result

val write_all_vectored: t -> from:IoVec.t -> unit result

val flush: t -> unit result
