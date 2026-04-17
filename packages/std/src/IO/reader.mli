open Kernel

module type Read = sig
  type t
  type err
  val read: t -> ?timeout:int64 -> bytes -> (int, err) result

  val read_vectored: t -> Kernel.IO.Iovec.t -> (int, err) result
end

type ('src, 'err) read = (module Read with type t = 'src and type err = 'err)
type ('src, 'err) buffered
type ('src, 'err) t

val make:
  read:('src -> ?timeout:int64 -> bytes -> (int, 'err) result) ->
  read_vectored:('src -> Kernel.IO.Iovec.t -> (int, 'err) result) ->
  'src ->
  ('src, 'err) t

val of_read_src: ('src, 'err) read -> 'src -> ('src, 'err) t

val buffered:
  ?chunk_size:int ->
  ('src, 'err) t ->
  (('src, 'err) buffered, 'err) t

val read: ('src, 'err) t -> ?timeout:int64 -> bytes -> (int, 'err) result

val read_vectored: ('src, 'err) t -> Kernel.IO.Iovec.t -> (int, 'err) result

val read_char: ('src, 'err) t -> (char option, 'err) result

val read_line: ('src, 'err) t -> (string, 'err) result

val read_to_string: ('src, 'err) t -> len:int -> (string, 'err) result

val read_to_end: ('src, 'err) t -> buf:Buffer.t -> (int, 'err) result

val map_err: ('src, 'a) t -> fn:('a -> 'b) -> ('src, 'b) t

val empty: (unit, unit) t

val from_bytes: bytes -> (bytes, unit) t

val from_string: string -> (string, unit) t
