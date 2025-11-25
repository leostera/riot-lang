(** Reader abstraction for readable sources *)
  open Global0

module type Read = sig
  type t
  type err

  val read : t -> ?timeout:int64 -> bytes -> (int, err) result
  val read_vectored : t -> Iovec.t -> (int, err) result
end

type ('src, 'err) read = (module Read with type t = 'src and type err = 'err)
type ('src, 'err) t = Reader of (('src, 'err) read * 'src)

let of_read_src : type src err. (src, err) read -> src -> (src, err) t =
 fun read src -> Reader (read, src)

let read : type src err.
    (src, err) t -> ?timeout:int64 -> bytes -> (int, err) result =
 fun (Reader ((module R), src)) ?timeout buf -> R.read src ?timeout buf

let read_vectored : type src err. (src, err) t -> Iovec.t -> (int, err) result =
 fun (Reader ((module R), src)) bufs -> R.read_vectored src bufs

let read_to_end : type src err.
    (src, err) t -> buf:Buffer.t -> (int, err) result =
 fun (Reader ((module R), src)) ~buf:out ->
  let buf = Bytes.create 1024 in
  let rec read_loop total =
    match R.read src buf with
    | Ok 0 -> Ok total
    | Ok len ->
        Buffer.add_bytes out (Bytes.sub buf 0 len);
        read_loop (len + total)
    | Error err -> Error err
  in
  read_loop 0

let map_err : type src a b. (src, a) t -> fn:(a -> b) -> (src, b) t =
 fun (Reader ((module R), src)) ~fn ->
  let module Mapped = struct
    type t = R.t
    type err = b

    let read t ?timeout buf =
      match R.read t ?timeout buf with
      | Ok n -> Ok n
      | Error e -> Error (fn e)

    let read_vectored t bufs =
      match R.read_vectored t bufs with
      | Ok n -> Ok n
      | Error e -> Error (fn e)
  end in
  Reader ((module Mapped), src)

let empty =
  let module EmptyRead = struct
    type t = unit
    type err = unit

    let read () ?timeout:_ _buf = Ok 0
    let read_vectored () _bufs = Ok 0
  end in
  of_read_src (module EmptyRead) ()
