open Prelude

module IoVec = IoVec

module type Write = sig
  type t
  type err

  val write: t -> from:Buffer.t -> (int, err) result

  val write_vectored: t -> from:Kernel.IO.IoVec.t -> (int, err) result

  val flush: t -> (unit, err) result
end

type ('dst, 'err) sink = (module Write with type t = 'dst and type err = 'err)

type 'err t =
  | Writer: (('dst, 'err) sink * 'dst) -> 'err t

let from_sink = fun sink dst ->
  Writer (sink, dst)

let write: type err. err t -> from:Buffer.t -> (int, err) result =
 fun (Writer (((module Sink) as sink), dst)) ~from ->
  let _ = sink in
  Sink.write dst ~from

let write_vectored: type err. err t -> from:Kernel.IO.IoVec.t -> (int, err) result =
 fun (Writer (((module Sink) as sink), dst)) ~from ->
  let _ = sink in
  Sink.write_vectored dst ~from

let flush: type err. err t -> (unit, err) result =
 fun (Writer (((module Sink) as sink), dst)) ->
  let _ = sink in
  Sink.flush dst

let write_all_vectored: type err. err t -> from:Kernel.IO.IoVec.t -> (unit, err) result =
 fun writer ~from ->
  let rec loop remaining =
    if Kernel.IO.IoVec.length remaining = 0 then
      Ok ()
    else
      match write_vectored writer ~from:remaining with
      | Ok written -> (
          match Kernel.IO.IoVec.sub remaining ~pos:written ~len:(Kernel.IO.IoVec.length remaining - written) with
          | Ok next -> loop next
          | Error error ->
              Kernel.SystemError.panic
                ("IO.Writer.write_all_vectored: " ^ Kernel.IO.Error.message error)
        )
      | Error _ as error ->
          error
  in
  loop from

let write_all = fun writer ~from ->
  write_all_vectored writer ~from:(Buffer.to_iovec from)

let map_err: type a b. a t -> fn:(a -> b) -> b t =
 fun (Writer (((module Sink) as sink), dst)) ~fn ->
  let _ = sink in
  let module Mapped = struct
    type t = Sink.t
    type err = b

    let write = fun value ~from ->
      match Sink.write value ~from with
      | Ok count -> Ok count
      | Error err -> Error (fn err)

    let write_vectored = fun value ~from ->
      match Sink.write_vectored value ~from with
      | Ok count -> Ok count
      | Error err -> Error (fn err)

    let flush = fun value ->
      match Sink.flush value with
      | Ok () -> Ok ()
      | Error err -> Error (fn err)
  end in
  from_sink (module Mapped) dst
