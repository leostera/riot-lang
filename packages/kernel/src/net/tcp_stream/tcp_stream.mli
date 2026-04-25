type t

type shutdown =
  | Read
  | Write
  | ReadWrite

type error =
  | InvalidSlice of { pos: int; len: int; buffer_len: int }
  | InvalidSocketAddr of { ip: string; port: int }
  | InvalidConnectState of { state: int }
  | WouldBlock
  | ConnectionRefused
  | ConnectionReset
  | TimedOut
  | BrokenPipe
  | NotConnected
  | ConnectionAborted
  | NetworkUnreachable
  | System of System_error.t

val error_to_string: error -> string

type connect_result =
  | Connected of t
  | InProgress of t

(**
   Use `connect addr` to start a nonblocking TCP connect.

   `Connected stream` means the socket is ready immediately. `InProgress stream` means callers
   should wait for writability and retry `finish_connect` until it succeeds or returns a
   non-`WouldBlock` error. 
*)
val connect: Socket_addr.t -> (connect_result, error) Result.t

(** Use `close stream` to close the socket immediately. *)
val close: t -> (unit, error) Result.t

(**
   Use `finish_connect stream` to complete a previously in-progress nonblocking connect.

   Once it succeeds, later calls remain successful and act as an idempotent no-op. 
*)
val finish_connect: t -> (unit, error) Result.t

(**
   Use `shutdown stream how` to apply TCP half-close semantics.

   - `Write` surfaces EOF to the peer and rejects later local writes.
   - `Read` disables the local read half while preserving the local write half.
   - `ReadWrite` disables both local halves and surfaces EOF to the peer.
   - Repeating the same local shutdown is an idempotent no-op.

   If the peer shuts down its write half first, local reads observe EOF while the local write
   half remains usable. 
*)
val shutdown: t -> shutdown -> (unit, error) Result.t

(**
   Use `read stream buf` for one nonblocking read attempt.

   Readiness waiting stays separate through `to_source`. 
*)
val read: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

(**
   Use `write stream buf` for one nonblocking write attempt.

   Readiness waiting stays separate through `to_source`. 
*)
val write: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

(** Use `read_vectored stream iov` for one nonblocking vectored read attempt. *)
val read_vectored: t -> IO.IoVec.t -> (int, error) Result.t

(** Use `write_vectored stream iov` for one nonblocking vectored write attempt. *)
val write_vectored: t -> IO.IoVec.t -> (int, error) Result.t

(** Use `local_addr stream` to inspect the bound local socket address immediately. *)
val local_addr: t -> (Socket_addr.t, error) Result.t

(** Use `peer_addr stream` to inspect the connected peer address immediately. *)
val peer_addr: t -> (Socket_addr.t, error) Result.t

(** Use `to_source stream` to expose readiness for `finish_connect`, `read`, and `write`. *)
val to_source: t -> Async.Source.t
