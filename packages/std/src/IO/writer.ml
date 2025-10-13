(** Writer abstraction for writable destinations *)

open Global

let ( let* ) = Result.and_then

module type Write = sig
  type t
  type err

  val write : t -> buf:string -> (int, err) result
  val write_owned_vectored : t -> bufs:Iovec.t -> (int, err) result
  val flush : t -> (unit, err) result
end

type ('dst, 'err) write = (module Write with type t = 'dst and type err = 'err)
type ('dst, 'err) t = Writer of (('dst, 'err) write * 'dst)

let of_write_src : type dst err. (dst, err) write -> dst -> (dst, err) t =
 fun write dst -> Writer (write, dst)

let write : type dst err. (dst, err) t -> buf:string -> (int, err) result =
 fun (Writer ((module W), dst)) ~buf -> W.write dst ~buf

let write_owned_vectored : type dst err.
    (dst, err) t -> bufs:Iovec.t -> (int, err) result =
 fun (Writer ((module W), dst)) ~bufs -> W.write_owned_vectored dst ~bufs

let flush : type dst err. (dst, err) t -> (unit, err) result =
 fun (Writer ((module W), dst)) -> W.flush dst

let write_all : type dst err. (dst, err) t -> buf:string -> (unit, err) result =
 fun (Writer ((module W), dst)) ~buf ->
  let total = String.length buf in
  let rec write_loop buf len =
    if String.length buf > 0 then
      let* n = W.write dst ~buf in
      let rest = len - n in
      write_loop (String.sub buf n (len - n)) rest
    else Ok ()
  in
  write_loop buf total

let write_all_vectored : type dst err.
    (dst, err) t -> bufs:Iovec.t -> (unit, err) result =
 fun (Writer ((module W), dst)) ~bufs ->
  let total = Iovec.length bufs in
  let rec write_loop bufs len =
    if Iovec.length bufs > 0 then
      let* n = W.write_owned_vectored dst ~bufs in
      let rest = len - n in
      write_loop (Iovec.sub bufs ~pos:n ~len:(len - n)) rest
    else Ok ()
  in
  write_loop bufs total
