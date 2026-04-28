(** HTTP/1.1 Request Parser *)
open Std
open Std.Iter
open Common

(**
   Parses an HTTP/1.1 request.

   @param max_request_line Maximum length of request line (default: 8192)
   @param max_headers Maximum number of headers (default: 100)
   @param max_header_length Maximum length of header name+value (default: 8192)
   @param max_header_block_length Maximum total header block length (default: 65536)

   Returns [Done request] on success, [Need_more] if more data needed, or
   [Error error] if parsing fails.
*)
val parse_slice:
  ?max_request_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  ?max_header_block_length:int ->
  IO.IoVec.IoSlice.t ->
  Std.Net.Http.Request.t parse_result

val parse:
  ?max_request_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  ?max_header_block_length:int ->
  string ->
  Std.Net.Http.Request.t parse_result

(**
   Parses only the request line and headers.

   The returned request never contains a body. Bytes after the header
   terminator are returned as [remaining] for callers that want to decide body
   framing separately.
*)
val parse_head_slice:
  ?max_request_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  ?max_header_block_length:int ->
  IO.IoVec.IoSlice.t ->
  Std.Net.Http.Request.t parse_result

val parse_head:
  ?max_request_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  ?max_header_block_length:int ->
  string ->
  Std.Net.Http.Request.t parse_result

(** Parses HTTP headers. Internal function exposed for testing. *)
val parse_headers:
  ?max_count:int ->
  ?max_length:int ->
  ?max_total_length:int ->
  ?acc:(string * string) list ->
  Cursor.t ->
  ((string * string) list * string) Common.parse_result
