type error =
  | InvalidPort of { port: int }
  | HostNotFound of { host: string }
  | TemporaryFailure of { host: string }
  | NoAddressesFound of { host: string; port: int }
  | InvalidSocketAddr of { ip: string; port: int }
  | ResolutionFailed of { host: string }
  | System of System_error.t

val error_to_string: error -> string

val resolve_stream: host:string -> port:int -> (Socket_addr.t array, error) Result.t

val resolve_first_stream: host:string -> port:int -> (Socket_addr.t, error) Result.t

val resolve_datagram: host:string -> port:int -> (Socket_addr.t array, error) Result.t

val resolve_first_datagram: host:string -> port:int -> (Socket_addr.t, error) Result.t
