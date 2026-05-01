(**
   HTTP server configuration.

   Controls limits and behavior for HTTP request parsing and handling.
*)
type t = {
  max_request_line_length: int;
  (**
     Maximum length of HTTP request line (method + URI + version) in bytes
  *)
  max_header_count: int;
  (** Maximum number of HTTP headers allowed *)
  max_header_length: int;
  (** Maximum length of a single header in bytes *)
  max_body_size: int;
  (** Maximum request body size in bytes *)
  max_keep_alive_requests: int;
  (** Maximum requests allowed per keep-alive connection *)
  max_websocket_frame_size: int;
  (** Maximum WebSocket frame payload size in bytes *)
  max_websocket_message_size: int;
  (** Maximum reassembled WebSocket message size in bytes *)
  read_header_timeout_ms: int;
  (** Maximum time to wait for request headers in milliseconds *)
  read_body_timeout_ms: int;
  (** Maximum time to wait for request bodies in milliseconds *)
  idle_timeout_ms: int;
  (** Maximum idle keep-alive time in milliseconds *)
  write_timeout_ms: int;
  (** Maximum time to wait for response writes in milliseconds *)
  buffer_size: int;
  (** Size of read buffer for connections *)
}

val make:
  ?max_request_line_length:int ->
  ?max_header_count:int ->
  ?max_header_length:int ->
  ?max_body_size:int ->
  ?max_keep_alive_requests:int ->
  ?max_websocket_frame_size:int ->
  ?max_websocket_message_size:int ->
  ?read_header_timeout_ms:int ->
  ?read_body_timeout_ms:int ->
  ?idle_timeout_ms:int ->
  ?write_timeout_ms:int ->
  ?buffer_size:int ->
  unit ->
  t

(**
   [make ()] creates a new configuration with optional overrides.

   Defaults:
   - [max_request_line_length] = 8192 bytes
   - [max_header_count] = 100 headers
   - [max_header_length] = 8192 bytes
   - [max_body_size] = 10485760 bytes
   - [max_keep_alive_requests] = 100 requests
   - [max_websocket_frame_size] = 1048576 bytes
   - [max_websocket_message_size] = 16777216 bytes
   - [read_header_timeout_ms] = 5000 milliseconds
   - [read_body_timeout_ms] = 30000 milliseconds
   - [idle_timeout_ms] = 60000 milliseconds
   - [write_timeout_ms] = 30000 milliseconds
   - [buffer_size] = 4096 bytes
*)
val default: t

(** Default configuration with standard limits *)
