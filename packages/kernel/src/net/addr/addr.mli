type error =
  | InvalidPort of { port: int }
  | HostNotFound of { host: string }
  | TemporaryFailure of { host: string }
  | NoAddressesFound of { host: string; port: int }
  | InvalidSocketAddr of { ip: string; port: int }
  | ResolutionFailed of { host: string }
  | System of System_error.t

val error_to_string: error -> string

(**
   Use `resolve_stream ~host ~port` to resolve TCP-capable concrete socket addresses.

   IP literals stay local and skip name resolution. Hostnames return concrete `SocketAddr.t`
   values in resolver order.
*)
val resolve_stream: host:string -> port:int -> (Socket_addr.t array, error) Result.t

(** Use `resolve_first_stream ~host ~port` to take the first TCP-capable result. *)
val resolve_first_stream: host:string -> port:int -> (Socket_addr.t, error) Result.t

(**
   Use `resolve_datagram ~host ~port` to resolve UDP-capable concrete socket addresses.

   IP literals stay local and skip name resolution. Hostnames return concrete `SocketAddr.t`
   values in resolver order.
*)
val resolve_datagram: host:string -> port:int -> (Socket_addr.t array, error) Result.t

(** Use `resolve_first_datagram ~host ~port` to take the first UDP-capable result. *)
val resolve_first_datagram: host:string -> port:int -> (Socket_addr.t, error) Result.t
