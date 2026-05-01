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

(**
   Use `bind addr` to create a nonblocking listener immediately.

   Waiting for inbound peers stays separate through `accept` plus `to_source`.
*)
val bind:
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  ?backlog:int ->
  Socket_addr.t ->
  (t, error) Result.t

(**
   Use `accept listener` for one nonblocking accept attempt.

   It reports `WouldBlock` instead of waiting when no peer is ready.
*)
val accept: t -> (Tcp_stream.t * Socket_addr.t, error) Result.t

val close: t -> (unit, error) Result.t

(** Use `local_addr listener` to inspect the bound local address immediately. *)
val local_addr: t -> (Socket_addr.t, error) Result.t

(** Use `to_source listener` to expose accept readiness for `accept`. *)
val to_source: t -> Async.Source.t
