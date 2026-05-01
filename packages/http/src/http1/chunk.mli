(** Chunked Transfer Encoding Parser *)
open Std
open Common

(**
   Parse a single chunk from chunked encoding. Returns chunk data and remaining
   input
*)
type chunk_result = { data: string; remaining: string }

val parse_slice: ?max_chunk_size_line:int -> IO.IoVec.IoSlice.t -> chunk_result parse_result

val parse: ?max_chunk_size_line:int -> string -> chunk_result parse_result

(** Fully decode a chunked body, including the final zero-size chunk and trailers. *)
type body_result = {
  body: string;
  trailers: (string * string) list;
  remaining: string;
}

val decode_slice:
  ?max_chunk_size:int ->
  ?max_chunk_size_line:int ->
  ?max_body_size:int ->
  ?max_trailers:int ->
  ?max_trailer_length:int ->
  IO.IoVec.IoSlice.t ->
  body_result parse_result

val decode:
  ?max_chunk_size:int ->
  ?max_chunk_size_line:int ->
  ?max_body_size:int ->
  ?max_trailers:int ->
  ?max_trailer_length:int ->
  string ->
  body_result parse_result
