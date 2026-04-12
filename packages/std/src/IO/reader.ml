open Kernel
module Bytes = Stdlib.Bytes
module Buffer = Stdlib.Buffer
module Iovec = Kernel.IO.Iovec

module type Read = sig
  type t
  type err
  val read: t -> ?timeout:int64 -> bytes -> (int, err) result

  val read_vectored: t -> Iovec.t -> (int, err) result
end

type ('src, 'err) read = (module Read with type t = 'src and type err = 'err)

type ('src, 'err) t =
  | Reader of (('src, 'err) read * 'src)

let of_read_src = fun read src -> Reader (read, src)

let read: type src err. (src, err) t -> ?timeout:int64 -> bytes -> (int, err) result = fun (Reader ((module R), src)) ?timeout buf ->
  R.read src ?timeout buf

let read_vectored: type src err. (src, err) t -> Iovec.t -> (int, err) result = fun (Reader ((module R), src)) bufs ->
  R.read_vectored src bufs

let read_to_end: type src err. (src, err) t -> buf:Buffer.t -> (int, err) result = fun (Reader ((module R), src)) ~buf:out ->
  let chunk = Bytes.create 1_024 in
  let rec loop total =
    match R.read src chunk with
    | Ok 0 ->
        Ok total
    | Ok len ->
        Buffer.add_bytes out (Bytes.sub chunk 0 len);
        loop (total + len)
    | Error err ->
        Error err
  in
  loop 0

let map_err: type src a b. (src, a) t -> fn:(a -> b) -> (src, b) t = fun (Reader ((module R), src)) ~fn ->
  let module Mapped = struct
    type t = R.t

    type err = b

    let read = fun value ?timeout buf ->
      match R.read value ?timeout buf with
      | Ok read -> Ok read
      | Error err -> Error (fn err)

    let read_vectored = fun value bufs ->
      match R.read_vectored value bufs with
      | Ok read -> Ok read
      | Error err -> Error (fn err)
  end in
  Reader ((module Mapped), src)

let empty =
  let module Empty = struct
    type t = unit

    type err = unit

    let read = fun () ?timeout:_ _ -> Ok 0

    let read_vectored = fun () _ -> Ok 0
  end in
  of_read_src (module Empty) ()

type offset_state = {
  mutable offset: int;
}

type read_state = {
  mutable total: int;
  mutable continue: bool;
}

let from_bytes = fun data ->
  let state = { offset = 0 } in
  let module Bytes_read = struct
    type t = bytes

    type err = unit

    let read = fun source ?timeout:_ buf ->
      let remaining = Bytes.length source - state.offset in
      if remaining = 0 then
        Ok 0
      else
        let to_read = min (Bytes.length buf) remaining in
        Bytes.blit source state.offset buf 0 to_read;
        state.offset <- state.offset + to_read;
        Ok to_read

    let read_vectored = fun source iov ->
      let progress = { total = 0; continue = true } in
      Iovec.iter
        (fun { Kernel.IO.Iovec.buffer; offset; length } ->
          if progress.continue then
            let remaining = Bytes.length source - state.offset in
            if remaining = 0 then
              progress.continue <- false
            else
              let to_read = min length remaining in
              Bytes.blit source state.offset buffer offset to_read;
              state.offset <- state.offset + to_read;
              progress.total <- progress.total + to_read)
        iov;
      Ok progress.total
  end in
  of_read_src (module Bytes_read) data

let from_string = fun source ->
  let state = { offset = 0 } in
  let module String_read = struct
    type t = string

    type err = unit

    let read = fun value ?timeout:_ buf ->
      let remaining = Kernel.String.length value - state.offset in
      if remaining = 0 then
        Ok 0
      else
        let to_read = min (Bytes.length buf) remaining in
        Bytes.blit_string value state.offset buf 0 to_read;
        state.offset <- state.offset + to_read;
        Ok to_read

    let read_vectored = fun value iov ->
      let progress = { total = 0; continue = true } in
      Iovec.iter
        (fun { Kernel.IO.Iovec.buffer; offset; length } ->
          if progress.continue then
            let remaining = Kernel.String.length value - state.offset in
            if remaining = 0 then
              progress.continue <- false
            else
              let to_read = min length remaining in
              Bytes.blit_string value state.offset buffer offset to_read;
              state.offset <- state.offset + to_read;
              progress.total <- progress.total + to_read)
        iov;
      Ok progress.total
  end in
  of_read_src (module String_read) source
