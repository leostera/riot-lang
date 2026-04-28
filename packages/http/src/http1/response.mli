(** HTTP/1.1 Response Parser *)
open Std
open Common

(**
   Parses an HTTP/1.1 response.

   Returns [Done response] on success, [Need_more] if more data needed, or
   [Error error] if parsing fails.
*)
type t = Std.Net.Http.Response.t
val parse_slice:
  ?max_status_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  ?max_header_block_length:int ->
  ?max_body_size:int ->
  ?max_chunk_size:int ->
  ?max_trailers:int ->
  ?max_trailer_length:int ->
  IO.IoVec.IoSlice.t ->
  t parse_result

val parse:
  ?max_status_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  ?max_header_block_length:int ->
  ?max_body_size:int ->
  ?max_chunk_size:int ->
  ?max_trailers:int ->
  ?max_trailer_length:int ->
  string ->
  t parse_result

(**
   Parses only the status line and headers.

   The returned response never contains a body. Bytes after the header
   terminator are returned as [remaining] for callers that want to decide body
   framing separately.
*)
val parse_head_slice:
  ?max_status_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  ?max_header_block_length:int ->
  IO.IoVec.IoSlice.t ->
  t parse_result

val parse_head:
  ?max_status_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  ?max_header_block_length:int ->
  string ->
  t parse_result
