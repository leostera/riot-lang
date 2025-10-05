type t
type connect_result = [ `Connected of t | `In_progress of t ]

val of_fd : Fd.t -> t
val to_string : t -> string
val close : t -> unit

val connect :
  Addr.stream_addr -> (connect_result, [> Async.io_error ]) Async.io_result

val read :
  t ->
  ?pos:int ->
  ?len:int ->
  bytes ->
  (int, [> Async.io_error ]) Async.io_result

val write :
  t ->
  ?pos:int ->
  ?len:int ->
  bytes ->
  (int, [> Async.io_error ]) Async.io_result

val read_vectored :
  t -> Async.Iovec.t -> (int, [> Async.io_error ]) Async.io_result

val write_vectored :
  t -> Async.Iovec.t -> (int, [> Async.io_error ]) Async.io_result

val sendfile :
  t ->
  file:Fd.t ->
  off:int ->
  len:int ->
  (int, [> Async.io_error ]) Async.io_result

val to_source : t -> Async.Source.t
