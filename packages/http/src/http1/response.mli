(** HTTP/1.1 Response Parser *)
open Std
open Common

(** Parses an HTTP/1.1 response.

    Returns [Done response] on success, [Need_more] if more data needed, or
    [Error msg] if parsing fails. *)
type t = Std.Net.Http.Response.t
val parse_slice: IO.Iovec.IoSlice.t -> t parse_result
val parse: string -> t parse_result
