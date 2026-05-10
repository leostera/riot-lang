type t
type shutdown =
  | Read
  | Write
  | ReadWrite
type error =
  | InvalidSlice of { pos: int; len: int; buffer_len: int }
  | InvalidConnectState of { state: int }
  | WouldBlock
  | ConnectionRefused
  | ConnectionReset
  | TimedOut
  | BrokenPipe
  | NotConnected
  | ConnectionAborted
  | NetworkUnreachable
  | System of System_error.t

val error_to_string: error -> string

type connect_result =
  | Connected of t
  | InProgress of t

val connect: string -> (connect_result, error) Result.t

val close: t -> (unit, error) Result.t

val finish_connect: t -> (unit, error) Result.t

val shutdown: t -> shutdown -> (unit, error) Result.t

val read: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

val write: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

val read_vectored: t -> IO.IoVec.t -> (int, error) Result.t

val write_vectored: t -> IO.IoVec.t -> (int, error) Result.t

val to_source: t -> Async.Source.t
