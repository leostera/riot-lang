(** Chunked Transfer Encoding Parser *)

open Std
open Common

type chunk_result = { data : string; remaining : string }

val parse : string -> chunk_result parse_result
(** Parse a single chunk from chunked encoding. Returns chunk data and remaining
    input *)
