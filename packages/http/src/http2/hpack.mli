open Std

(**
   HPACK header compression for HTTP/2.

   This module implements the HPACK header compression algorithm used by HTTP/2
   to reduce the size of HTTP headers transmitted over the wire.

   HPACK uses:
   - A static table of common header fields (defined in RFC 7541 Appendix A)
   - A dynamic table that evolves based on headers seen in the connection
   - Plain HPACK string literals. Huffman-encoded strings are rejected until
     the RFC 7541 Appendix B decoder is implemented.
*)

(** A header field is a name-value pair. *)
type header = { name: string; value: string }
type table_size_error =
  | InvalidTableSize of { size: int }

val table_size_error_to_string: table_size_error -> string

type decode_error =
  | IncompleteIntegerEncoding
  | IntegerEncodingOverflow of { accumulator: int; multiplier: int; value: int }
  | IncompleteStringEncoding
  | StringDataTruncated of { length: int; available: int }
  | UnsupportedHuffmanStringEncoding
  | InvalidHeaderIndex of int
  | InvalidNameIndex of int
  | DynamicTableSizeUpdateFailed of table_size_error
  | DynamicTableSizeUpdateAfterHeaders

val decode_error_to_string: decode_error -> string

type encode_error =
  | HeaderNotIndexed of header

val encode_error_to_string: encode_error -> string

(** Encoding representation for a header field. *)
type encoding_type =
  (** Fully indexed; both name and value come from a table. *)
  | Indexed
  (** Literal with indexing; add to dynamic table after encoding. *)
  | LiteralWithIndexing
  (** Literal without indexing; do not add to table. *)
  | LiteralWithoutIndexing
  (** Literal never indexed; must not be added to a table. *)
  | LiteralNeverIndexed
(** Encoder context maintains the dynamic table state. *)
type encoder
(** Decoder context maintains the dynamic table state. *)
type decoder

(**
   Create a new encoder with the given dynamic table size limit.
   Default is 4096 bytes per RFC 7541.
*)
val create_encoder: ?max_dynamic_table_size:int -> unit -> encoder

(**
   Encode a list of headers into HPACK wire format.

   The encoder will automatically choose the best encoding strategy:
   - Use indexed representation if the header exists in static/dynamic table
   - Use literal with indexing for new headers that should be cached
   - Use literal never indexed for sensitive headers (e.g., authorization)

   @param encoder The encoder context
   @param headers List of headers to encode
   @param sensitive_headers Optional set of header names that should never be indexed
   @return Encoded bytes or a structured encoder error
*)
val encode:
  encoder ->
  ?sensitive_headers:string list ->
  unit ->
  headers:header list ->
  (bytes, encode_error) Result.t

(** Encode a single header field *)
val encode_header:
  encoder ->
  header ->
  encoding_type:encoding_type ->
  (bytes, encode_error) Result.t

(**
   Update the dynamic table size limit.
   This is used when receiving SETTINGS_HEADER_TABLE_SIZE from the peer.
*)
val update_encoder_max_table_size: encoder -> int -> (unit, table_size_error) Result.t

(** Current encoder dynamic table byte size. *)
val encoder_dynamic_table_size: encoder -> int

(** Current encoder dynamic table byte limit. *)
val encoder_dynamic_table_max_size: encoder -> int

(**
   Create a new decoder with the given dynamic table size limit.
   Default is 4096 bytes per RFC 7541.
*)
val create_decoder: ?max_dynamic_table_size:int -> unit -> decoder

(**
   Decode HPACK-encoded bytes into a list of headers.

   @param decoder The decoder context
   @param data The HPACK-encoded bytes to decode
   @return Either the decoded headers or a structured decode error
*)
val decode: decoder -> bytes -> (header list, decode_error) Result.t

(**
   Update the dynamic table size limit.
   This is used when receiving SETTINGS_HEADER_TABLE_SIZE from the peer.
*)
val update_decoder_max_table_size: decoder -> int -> (unit, table_size_error) Result.t

val update_max_table_size: decoder -> int -> (unit, table_size_error) Result.t

(** Current decoder dynamic table byte size. *)
val decoder_dynamic_table_size: decoder -> int

(** Current decoder dynamic table byte limit. *)
val decoder_dynamic_table_max_size: decoder -> int

(**
   Lookup a header in the static table by index (1-61).
   Returns None if index is out of range.
*)
val static_table_lookup: int -> header option

(**
   Find the index of a header in the static table.
   Returns None if not found.
*)
val static_table_find: name:string -> value:string -> int option

(**
   Find the index of a header name in the static table (value may differ).
   Returns None if not found.
*)
val static_table_find_name: string -> int option

(**
   Check if a header name should never be indexed (security-sensitive).
   Examples: authorization, cookie, set-cookie
*)
val is_sensitive_header: string -> bool

(**
   Calculate the size of a header for dynamic table accounting.
   Per RFC 7541: size = length(name) + length(value) + 32
*)
val header_size: header -> int
