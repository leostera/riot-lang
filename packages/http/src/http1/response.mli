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
  ?max_headers:int ->
  ?max_header_length:int ->
  ?max_header_block_length:int ->
  IO.IoVec.IoSlice.t ->
  t parse_result

val parse:
  ?max_headers:int ->
  ?max_header_length:int ->
  ?max_header_block_length:int ->
  string ->
  t parse_result
