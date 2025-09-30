type t

val pp : Format.formatter -> t -> unit
val close : t -> unit

val bind :
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  ?backlog:int ->
  Addr.stream_addr ->
  (t, [> Async.io_error ]) Async.io_result

val accept :
  t -> (Tcp_stream.t * Addr.stream_addr, [> Async.io_error ]) Async.io_result

val to_source : t -> Async.Source.t
