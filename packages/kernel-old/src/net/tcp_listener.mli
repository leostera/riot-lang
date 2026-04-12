open Global0

type t
val close: t -> unit

val bind: ?reuse_addr:bool -> ?reuse_port:bool -> ?backlog:int -> Addr.stream_addr -> (t, IO.error) result

val accept: t -> (Tcp_stream.t * Addr.stream_addr, IO.error) result

val local_addr: t -> (Addr.stream_addr, IO.error) result

val to_source: t -> Async.Source.t
