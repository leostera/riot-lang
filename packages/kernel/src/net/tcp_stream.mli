open IO

type t
type connect_result = [ `Connected of t | `In_progress of t ]

val of_fd : Fd.t -> t
val to_string : t -> string
val close : t -> unit

val connect :
  Addr.stream_addr -> (connect_result, IO.error) result

val read :
  t ->
  ?pos:int ->
  ?len:int ->
  bytes ->
  (int, IO.error) result

val write :
  t ->
  ?pos:int ->
  ?len:int ->
  bytes ->
  (int, IO.error) result

val read_vectored :
  t -> Iovec.t -> (int, IO.error) result

val write_vectored :
  t -> Iovec.t -> (int, IO.error) result

val sendfile :
  t ->
  file:Fd.t ->
  off:int ->
  len:int ->
  (int, IO.error) result

val to_source : t -> Async.Source.t
