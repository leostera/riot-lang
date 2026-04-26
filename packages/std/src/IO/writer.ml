open Prelude

module IoVec = IoVec

type 'value result = ('value, Error.t) Result.t

module type Write = sig
  type t
  val write: t -> from:Buffer.t -> int result

  val write_vectored: t -> from:IoVec.t -> int result

  val flush: t -> unit result
end

type 'dst sink = (module Write with type t = 'dst)

type t =
  | Writer: ('dst sink * 'dst) -> t

let from_sink = fun sink dst -> Writer (sink, dst)

let write: t -> from:Buffer.t -> int result = fun (Writer (((module Sink) as sink), dst)) ~from ->
  let _ = sink in
  Sink.write dst ~from

let write_vectored: t -> from:IoVec.t -> int result = fun (Writer (((module Sink) as sink), dst)) ~from ->
  let _ = sink in
  Sink.write_vectored dst ~from

let flush: t -> unit result = fun (Writer (((module Sink) as sink), dst)) ->
  let _ = sink in
  Sink.flush dst

let write_all_vectored: t -> from:IoVec.t -> unit result = fun writer ~from ->
  let rec loop remaining =
    if IoVec.length remaining = 0 then
      Ok ()
    else
      match write_vectored writer ~from:remaining with
      | Ok written -> (
          match IoVec.sub remaining ~pos:written ~len:(IoVec.length remaining - written) with
          | Ok next -> loop next
          | Error error ->
              Kernel.SystemError.panic
                ("IO.Writer.write_all_vectored: " ^ Kernel.IO.Error.message error)
        )
      | Error _ as error -> error
  in
  loop from

let write_all = fun writer ~from -> write_all_vectored writer ~from:(Buffer.to_iovec from)
