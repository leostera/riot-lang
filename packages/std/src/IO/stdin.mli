open Prelude

module Bytes = Bytes
module Iovec = Kernel.IO.Iovec
module Reader = Reader

type error = Error.t
type t

val open_: ?chunk_size:int -> unit -> t

val read: t -> ?offset:int -> ?len:int -> Bytes.t -> (int, error) result

val read_vectored: t -> Iovec.t -> (int, error) result

val read_line: t -> (string, error) result

val read_to_string: t -> len:int -> (string, error) result

val to_reader: t -> (t, error) Reader.t
