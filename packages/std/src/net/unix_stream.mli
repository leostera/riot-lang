(** Unix-domain stream for connected sockets. *)
open Global

type t = Kernel.Net.UnixStream.t
type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

val connect: Path.t -> (t, error) Kernel.result

val read:
  t ->
  bytes ->
  ?pos:int ->
  ?len:int ->
  ?timeout:Time.Duration.t ->
  unit ->
  (int, error) Kernel.result

val write: t -> bytes -> ?pos:int -> ?len:int -> unit -> (int, error) Kernel.result

val close: t -> unit

val to_reader: t -> IO.Reader.t

val to_writer: t -> IO.Writer.t
