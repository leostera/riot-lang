(** Reader abstraction for readable sources *)
open Global0

module type Read = sig
  type t
  type err
  val read: t -> ?timeout:int64 -> bytes -> (int, err) result

  val read_vectored: t -> Iovec.t -> (int, err) result
end

type ('src, 'err) read = (module Read with type t = 'src and type err = 'err)

type ('src, 'err) t =
  Reader of (('src, 'err) read * 'src)

let of_read_src : type src err. (src, err) read -> src -> (src, err) t = fun read src -> Reader (
  read,
  src
)

let read : type src err. (src, err) t -> ?timeout:int64 -> bytes -> (int, err) result = fun (Reader ((module R), src)) ?timeout buf -> R.read
src
?timeout
buf

let read_vectored : type src err. (src, err) t -> Iovec.t -> (int, err) result = fun (Reader ((module R), src)) bufs ->
  R.read_vectored src bufs

let read_to_end : type src err. (src, err) t -> buf:Buffer.t -> (int, err) result = fun (Reader ((module R), src)) ~buf:out ->
  let buf = Bytes.create 1_024 in
  let rec read_loop = fun total ->
    match R.read src buf with
    | Ok 0 ->
        Ok total
    | Ok len ->
        Buffer.add_bytes out (Bytes.sub buf 0 len);
        read_loop (len + total)
    | Error err ->
        Error err
  in
  read_loop 0

let map_err : type src a b. (src, a) t -> fn:(a -> b) -> (src, b) t = fun (Reader ((module R), src)) ~fn ->
  let module Mapped = struct
    type t = R.t

    type err = b

    let read = fun t ?timeout buf ->
      match R.read t ?timeout buf with
      | Ok n -> Ok n
      | Error e -> Error (fn e)

    let read_vectored = fun t bufs ->
      match R.read_vectored t bufs with
      | Ok n -> Ok n
      | Error e -> Error (fn e)
  end in
  Reader ((module Mapped), src)

let empty =
  let module EmptyRead = struct
    type t = unit

    type err = unit

    let read = fun () ?timeout:_ _buf -> Ok 0

    let read_vectored = fun () _bufs -> Ok 0
  end in
  of_read_src (module EmptyRead) ()

type offset_state = {
  mutable offset: int;
}

type read_state = {
  mutable total: int;
  mutable continue: bool;
}

let from_bytes = fun data ->
  (* Create a stateful reader that tracks offset *)
  let state = {offset = 0} in
  let module BytesRead = struct
    type t = bytes

    type err = unit

    let read = fun data ?timeout:_ buf ->
      let buf_len = Bytes.length buf in
      let data_len = Bytes.length data in
      let remaining = data_len - state.offset in
      if remaining = 0 then
        Ok 0
      else
        let to_read = min buf_len remaining in
        Bytes.blit data state.offset buf 0 to_read;
        state.offset <- state.offset + to_read;
        Ok to_read

    let read_vectored = fun data iov ->
      (* Simple implementation: iterate through buffers *)
      let read_state = {total = 0; continue = true} in
      Iovec.iter iov
        (fun ({ ba; off; len }) ->
          if read_state.continue then
            let buf = Bytes.sub ba off len in
            match read data buf with
            | Ok 0 ->
                read_state.continue <- false
            | Ok n ->
                Bytes.blit buf 0 ba off n;
                read_state.total <- read_state.total + n
            | Error _ ->
                read_state.continue <- false);
      Ok read_state.total
  end in
  of_read_src (module BytesRead) data

let from_string = fun str ->
  let state = {offset = 0} in
  let data = Bytes.of_string str in
  let module StringRead = struct
    type t = string

    type err = unit

    let read = fun _str ?timeout:_ buf ->
      let buf_len = Bytes.length buf in
      let data_len = Bytes.length data in
      let remaining = data_len - state.offset in
      if remaining = 0 then
        Ok 0
      else
        let to_read = min buf_len remaining in
        Bytes.blit data state.offset buf 0 to_read;
        state.offset <- state.offset + to_read;
        Ok to_read

    let read_vectored = fun _str iov ->
      (* Simple implementation: iterate through buffers *)
      let read_state = {total = 0; continue = true} in
      Iovec.iter iov
        (fun ({ ba; off; len }) ->
          if read_state.continue then
            let buf = Bytes.sub ba off len in
            match read _str buf with
            | Ok 0 ->
                read_state.continue <- false
            | Ok n ->
                Bytes.blit buf 0 ba off n;
                read_state.total <- read_state.total + n
            | Error _ ->
                read_state.continue <- false);
      Ok read_state.total
  end in
  of_read_src (module StringRead) str
