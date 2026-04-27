open Std

(**
   # Protocol Detection Handler

   Sniffs the initial bytes of a connection to detect HTTP/1.1 vs HTTP/2,
   then switches to the appropriate protocol handler.

   Detection logic:
   - HTTP/2: Starts with `PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n` (24 bytes)
   - HTTP/1.1: Starts with HTTP method (GET, POST, etc.)

   This handler reads the first few bytes, detects the protocol, and uses
   Socket_pool.Handler.Switch to delegate to the appropriate handler.
*)

type state
type error =
  | HandlerCalledAfterDetection
  | UnknownProtocol
  | DetectionLimitExceeded of { limit: int }
val to_string_error: error -> string

(**
   Create protocol detector

   @param config Server configuration
   @param handler Request handler (used by both HTTP/1.1 and HTTP/2)
   @return Initial state
*)
val make_handler: config:Super.Config.t -> handler:Http_handler.t -> unit -> state

val handle_close: Socket_pool.Connection.t -> state -> unit

val handle_connection:
  Socket_pool.Connection.t ->
  state ->
  (state, error) Socket_pool.Handler.handler_result

val handle_data:
  string ->
  Socket_pool.Connection.t ->
  state ->
  (state, error) Socket_pool.Handler.handler_result

val handle_error:
  error ->
  Socket_pool.Connection.t ->
  state ->
  (state, error) Socket_pool.Handler.handler_result

val handle_shutdown:
  Socket_pool.Connection.t ->
  state ->
  (state, error) Socket_pool.Handler.handler_result

val handle_message:
  Std.Message.t ->
  Socket_pool.Connection.t ->
  state ->
  (state, error) Socket_pool.Handler.handler_result
