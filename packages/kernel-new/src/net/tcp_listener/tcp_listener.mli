type t
type error = Error.t
val bind:
  ?reuse_addr:bool -> ?reuse_port:bool -> ?backlog:int -> Socket_addr.t -> (t, error) Result.t

val accept: t -> ((Tcp_stream.t * Socket_addr.t), error) Result.t

val close: t -> (unit, error) Result.t

val local_addr: t -> (Socket_addr.t, error) Result.t

val to_source: t -> Async.Source.t
