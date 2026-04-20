open Prelude

module IoVec = IoVec

module type Write = sig
  type t
  type err

  val write: t -> from:Buffer.t -> (int, err) result

  val write_vectored: t -> from:IoVec.t -> (int, err) result

  val flush: t -> (unit, err) result
end

type ('dst, 'err) sink = (module Write with type t = 'dst and type err = 'err)
type 'err t

val from_sink: ('dst, 'err) sink -> 'dst -> 'err t

val write: 'err t -> from:Buffer.t -> (int, 'err) result

val write_all: 'err t -> from:Buffer.t -> (unit, 'err) result

val write_vectored: 'err t -> from:IoVec.t -> (int, 'err) result

val write_all_vectored: 'err t -> from:IoVec.t -> (unit, 'err) result

val map_err: 'a t -> fn:('a -> 'b) -> 'b t

val flush: 'err t -> (unit, 'err) result
