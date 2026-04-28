open Std

(**
   HPACK Decoder using IO.Reader (Reentrant)

   This decoder is designed for streaming header decompression:
   - Uses IO.Reader.t instead of bytes directly
   - Maintains state between calls (reentrant)
   - Decodes headers incrementally as data arrives
   - Compatible with HTTP/2 frame boundaries
*)

(** Decoder state *)
type decoder

(** Create a new decoder with optional dynamic table size *)
val create: ?max_dynamic_table_size:int -> unit -> decoder

(** Decode errors *)
type decode_error =
  | ReadFailed of IO.error
  (** Reader returned an IO error before a header block could be decoded. *)
  | HpackDecodeFailed of Hpack.decode_error
  (** Canonical HPACK decoder rejected the buffered header block. *)

val decode_error_to_string: decode_error -> string

(** Decode result *)
type decode_result =
  | Headers of Hpack.header list
  (** Successfully decoded complete header block *)
  | Need_more
  (** Need more data - call again with more bytes *)
  | Error of decode_error

(** Decode error *)

(**
   Decode headers from reader.

   This is reentrant - you can call it multiple times as data arrives.
   The decoder automatically handles:
   - Multi-frame header blocks (HEADERS + CONTINUATION)
   - Dynamic table updates
   - Partial reads

   Example usage:
   {[
     let decoder = Hpack_reader.create () in
     let reader = IO.Reader.from_source source stream in

     match Hpack_reader.decode decoder reader with
     | Headers headers -> handle_headers headers
     | Need_more -> (* Wait for more data *)
     | Error e -> handle_error e
   ]}

   @param decoder The decoder state
   @param reader The IO reader
   @return Decode result
*)
val decode: decoder -> IO.Reader.t -> decode_result

(** Update maximum dynamic table size (from SETTINGS frame) *)
val update_max_table_size: decoder -> int -> (unit, Hpack.table_size_error) Result.t

(** Reset decoder state for new connection *)
val reset: decoder -> unit

(** Get dynamic table size *)
val dynamic_table_size: decoder -> int
