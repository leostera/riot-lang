(** Key Encoding - Fixed-width 41-byte keys *)

open Std

(* Get Bytes from Kernel *)
module Bytes = Kernel.IO.Bytes

let key_size = 41

(** {2 Binary I/O Helpers} *)

let write_i64_be buf off v = Bytes.set_int64_be buf off v
let read_i64_be buf off = Bytes.get_int64_be buf off
let write_u8 buf off v = Bytes.set_uint8 buf off v
let read_u8 buf off = Bytes.get_uint8 buf off

(** {2 EAVT Index Keys} *)

type eavt_key = {
  entity_id : int64;
  attr_id : int64;
  value_kind : Encoding.value_kind;
  value_repr : int64;
  tx_id : int64;
  fact_id : int64;
}

let encode_eavt k =
  let buf = Bytes.make key_size '\x00' in
  write_i64_be buf 0 k.entity_id;
  write_i64_be buf 8 k.attr_id;
  write_u8 buf 16 (Encoding.value_kind_to_byte k.value_kind);
  write_i64_be buf 17 k.value_repr;
  write_i64_be buf 25 k.tx_id;
  write_i64_be buf 33 k.fact_id;
  buf

let decode_eavt buf =
  {
    entity_id = read_i64_be buf 0;
    attr_id = read_i64_be buf 8;
    value_kind = Encoding.value_kind_of_byte (read_u8 buf 16);
    value_repr = read_i64_be buf 17;
    tx_id = read_i64_be buf 25;
    fact_id = read_i64_be buf 33;
  }

(** {2 AVET Index Keys} *)

type avet_key = {
  attr_id : int64;
  value_kind : Encoding.value_kind;
  value_repr : int64;
  entity_id : int64;
  tx_id : int64;
  fact_id : int64;
}

let encode_avet k =
  let buf = Bytes.make key_size '\x00' in
  write_i64_be buf 0 k.attr_id;
  write_u8 buf 8 (Encoding.value_kind_to_byte k.value_kind);
  write_i64_be buf 9 k.value_repr;
  write_i64_be buf 17 k.entity_id;
  write_i64_be buf 25 k.tx_id;
  write_i64_be buf 33 k.fact_id;
  buf

let decode_avet buf =
  {
    attr_id = read_i64_be buf 0;
    value_kind = Encoding.value_kind_of_byte (read_u8 buf 8);
    value_repr = read_i64_be buf 9;
    entity_id = read_i64_be buf 17;
    tx_id = read_i64_be buf 25;
    fact_id = read_i64_be buf 33;
  }

(** {2 SOURCE Index Keys} *)

type source_key = {
  source_id : int64;
  entity_id : int64;
  attr_id : int64;
  tx_id : int64;
  fact_id : int64;
}

let encode_source k =
  let buf = Bytes.make key_size '\x00' in
  write_i64_be buf 0 k.source_id;
  write_i64_be buf 8 k.entity_id;
  write_i64_be buf 16 k.attr_id;
  write_i64_be buf 24 k.tx_id;
  write_i64_be buf 32 k.fact_id;
  (* Byte 40 is padding (zero-initialized) *)
  buf

let decode_source buf =
  {
    source_id = read_i64_be buf 0;
    entity_id = read_i64_be buf 8;
    attr_id = read_i64_be buf 16;
    tx_id = read_i64_be buf 24;
    fact_id = read_i64_be buf 32;
  }

(** {2 FACT Index Keys} *)

type fact_key = { fact_id : int64; tx_id : int64 }

let encode_fact k =
  let buf = Bytes.make key_size '\x00' in
  write_i64_be buf 0 k.fact_id;
  write_i64_be buf 8 k.tx_id;
  (* Bytes 16-40 are padding (zero-initialized) *)
  buf

let decode_fact buf =
  { fact_id = read_i64_be buf 0; tx_id = read_i64_be buf 8 }

(** {2 URI Hashing} *)

(** URI Normalization - FROZEN RULES
    
    WARNING: These rules can NEVER change! Changing them corrupts the database.
    See: poneglyph/docs/URI_NORMALIZATION.md *)
let normalize_uri uri_str =
  (* 1. Lowercase scheme and host *)
  let with_lowercase_scheme =
    match String.index_opt uri_str ':' with
    | None -> uri_str
    | Some idx ->
        let scheme = String.sub uri_str 0 idx in
        let rest = String.sub uri_str idx (String.length uri_str - idx) in
        String.lowercase_ascii scheme ^ rest
  in
  
  (* 2. Remove fragment (after #) *)
  let without_fragment =
    match String.index_opt with_lowercase_scheme '#' with
    | None -> with_lowercase_scheme
    | Some idx -> String.sub with_lowercase_scheme 0 idx
  in
  
  (* 3. Normalize trailing slash *)
  let normalized =
    if String.length without_fragment > 1 && 
       String.ends_with ~suffix:"/" without_fragment then
      String.sub without_fragment 0 (String.length without_fragment - 1)
    else
      without_fragment
  in
  
  normalized

(** Convert a URI to a stable int64 ID using SHA-256.
    
    CRITICAL: Uses frozen URI normalization rules to ensure stability!
    The URI already contains its precomputed SHA-256 hash (32 bytes).
    We extract the first 8 bytes as an int64 for index keys.
    
    This ensures:
    - Same URI -> Same ID (always)
    - Different URIs -> Different IDs (high probability, 2^64 space)
    - Ordering is stable across restarts *)
let uri_to_id uri =
  (* Extract first 8 bytes of SHA-256 hash as int64 *)
  Bytes.get_int64_be uri.Model.Uri.sha256 0
