(** HTTP/1.1 Request Parser *)
open Std
open Std.Iter
open Common

type request_slices = {
  method_: IO.IoVec.IoSlice.t;
  path: IO.IoVec.IoSlice.t;
  version: IO.IoVec.IoSlice.t;
  headers: (IO.IoVec.IoSlice.t * IO.IoVec.IoSlice.t) list;
  body: IO.IoVec.IoSlice.t;
}

type 'a borrowed_parse_result =
  | Borrowed_done of { value: 'a; remaining: IO.IoVec.IoSlice.t }
  | Borrowed_need_more
  | Borrowed_error of string

(** Parses an HTTP/1.1 request.

    @param max_request_line Maximum length of request line (default: 8192)
    @param max_headers Maximum number of headers (default: 100)
    @param max_header_length Maximum length of header name+value (default: 8192)

    Returns [Done request] on success, [Need_more] if more data needed, or
    [Error msg] if parsing fails. *)
val parse_slices:
  ?max_request_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  IO.IoVec.IoSlice.t ->
  request_slices borrowed_parse_result

val parse_slice:
  ?max_request_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  IO.IoVec.IoSlice.t ->
  Std.Net.Http.Request.t parse_result

val parse:
  ?max_request_line:int ->
  ?max_headers:int ->
  ?max_header_length:int ->
  string ->
  Std.Net.Http.Request.t parse_result

(** Parses HTTP headers. Internal function exposed for testing. *)
val parse_headers:
  ?max_count:int ->
  ?max_length:int ->
  ?acc:(string * string) list ->
  Cursor.t ->
  ((string * string) list * string) Common.parse_result
