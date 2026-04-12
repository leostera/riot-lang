open Kernel

module type Write = sig
  type t
  type err
  val write: t -> buf:string -> (int, err) result

  val write_owned_vectored: t -> bufs:Kernel.IO.Iovec.t -> (int, err) result

  val flush: t -> (unit, err) result
end

type ('dst, 'err) write = (module Write with type t = 'dst and type err = 'err)
type ('dst, 'err) t
val of_write_src: ('dst, 'err) write -> 'dst -> ('dst, 'err) t

val write: ('dst, 'err) t -> buf:string -> (int, 'err) result

val write_all: ('dst, 'err) t -> buf:string -> (unit, 'err) result

val write_owned_vectored: ('dst, 'err) t -> bufs:Kernel.IO.Iovec.t -> (int, 'err) result

val write_all_vectored: ('dst, 'err) t -> bufs:Kernel.IO.Iovec.t -> (unit, 'err) result

val map_err: ('dst, 'a) t -> fn:('a -> 'b) -> ('dst, 'b) t

val flush: ('dst, 'err) t -> (unit, 'err) result
