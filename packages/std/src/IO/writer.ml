open Kernel
module Iovec = Kernel.IO.Iovec
module String = Stdlib.String

module type Write = sig
  type t
  type err
  val write: t -> buf:string -> (int, err) result

  val write_owned_vectored: t -> bufs:Iovec.t -> (int, err) result

  val flush: t -> (unit, err) result
end

type ('dst, 'err) write = (module Write with type t = 'dst and type err = 'err)

type ('dst, 'err) t =
  | Writer of (('dst, 'err) write * 'dst)

let of_write_src = fun write dst -> Writer (write, dst)

let write: type dst err. (dst, err) t -> buf:string -> (int, err) result = fun (Writer ((module W), dst)) ~buf ->
  W.write dst ~buf

let write_owned_vectored: type dst err. (dst, err) t -> bufs:Iovec.t -> (int, err) result = fun (Writer ((module W), dst)) ~bufs ->
  W.write_owned_vectored dst ~bufs

let flush: type dst err. (dst, err) t -> (unit, err) result = fun (Writer ((module W), dst)) ->
  W.flush dst

let write_all: type dst err. (dst, err) t -> buf:string -> (unit, err) result = fun (Writer ((module W), dst)) ~buf ->
  let rec loop remaining =
    if String.length remaining = 0 then
      Ok ()
    else
      match W.write dst ~buf:remaining with
      | Ok written -> loop (String.sub remaining written (String.length remaining - written))
      | Error err -> Error err
  in
  loop buf

let write_all_vectored: type dst err. (dst, err) t -> bufs:Iovec.t -> (unit, err) result = fun (Writer ((module W), dst)) ~bufs ->
  let rec loop remaining =
    if Iovec.length remaining = 0 then
      Ok ()
    else
      match W.write_owned_vectored dst ~bufs:remaining with
      | Ok written -> loop (Iovec.sub remaining ~pos:written ~len:(Iovec.length remaining - written))
      | Error err -> Error err
  in
  loop bufs

let map_err: type dst a b. (dst, a) t -> fn:(a -> b) -> (dst, b) t = fun (Writer ((module W), dst)) ~fn ->
  let module Mapped = struct
    type t = W.t

    type err = b

    let write = fun value ~buf ->
      match W.write value ~buf with
      | Ok written -> Ok written
      | Error err -> Error (fn err)

    let write_owned_vectored = fun value ~bufs ->
      match W.write_owned_vectored value ~bufs with
      | Ok written -> Ok written
      | Error err -> Error (fn err)

    let flush = fun value ->
      match W.flush value with
      | Ok () -> Ok ()
      | Error err -> Error (fn err)
  end in
  Writer ((module Mapped), dst)
