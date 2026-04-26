type t
type error =
  | InvalidBacklog of { backlog: int }
  | InvalidSocketAddr of { ip: string; port: int }
  | WouldBlock
  | AddressInUse
  | AddressNotAvailable
  | ConnectionAborted
  | System of System_error.t
val error_to_string: error -> string

val bind:
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  ?backlog:int ->
  Socket_addr.t ->
  (t, error) Result.t

val accept: t -> ((Tcp_stream.t * Socket_addr.t), error) Result.t

val close: t -> (unit, error) Result.t

val local_addr: t -> (Socket_addr.t, error) Result.t

val to_source: t -> Async.Source.t
