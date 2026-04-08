type t
type error = Error.t
type connect_result =
  | Connected of t
  | In_progress of t

val connect: Socket_addr.t -> (connect_result, error) Result.t

val close: t -> (unit, error) Result.t

val read: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

val write: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

val read_vectored: t -> IO.Iovec.t -> (int, error) Result.t

val write_vectored: t -> IO.Iovec.t -> (int, error) Result.t

val local_addr: t -> (Socket_addr.t, error) Result.t

val peer_addr: t -> (Socket_addr.t, error) Result.t

val to_source: t -> Async.Source.t
