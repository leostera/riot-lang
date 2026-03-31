(** HTTP server configuration.

    Controls limits and behavior for HTTP request parsing and handling. *)
type t = {
  max_request_line_length : int;
  (** Maximum length of HTTP request line (method + URI + version) in bytes
      *)
  max_header_count : int;  (** Maximum number of HTTP headers allowed *)
  max_header_length : int;  (** Maximum length of a single header in bytes *)
  buffer_size : int;  (** Size of read buffer for connections *)
}
val make : ?max_request_line_length:int ->
?max_header_count:int ->
?max_header_length:int ->
?buffer_size:int ->
unit ->
t

(** [make ()] creates a new configuration with optional overrides.

    Defaults:
    - [max_request_line_length] = 8192 bytes
    - [max_header_count] = 100 headers
    - [max_header_length] = 8192 bytes
    - [buffer_size] = 4096 bytes *)
val default : t

(** Default configuration with standard limits *)
