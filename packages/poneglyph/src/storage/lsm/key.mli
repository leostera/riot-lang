(** Key Encoding - Fixed-width 41-byte keys for LSM indices
    
    All keys are EXACTLY 41 bytes and sortable by lexicographic byte comparison.
    
    Layout:
    - EAVT: [entity:8][attr:8][v_kind:1][v_repr:8][tx:8][fact:8] = 41 bytes
    - AVET: [attr:8][v_kind:1][v_repr:8][entity:8][tx:8][fact:8] = 41 bytes
    - SOURCE: [source:8][entity:8][attr:8][tx:8][fact:8][pad:3] = 41 bytes
    - FACT: [fact:8][tx:8][pad:25] = 41 bytes
    
    Critical property:
      Bytes.compare key1 key2 = semantic_compare key1 key2
*)

open Std

val key_size : int
(** All keys are exactly 41 bytes *)

(** {2 EAVT Index Keys} *)

type eavt_key = {
  entity_id : int64;
  attr_id : int64;
  value_kind : Encoding.value_kind;
  value_repr : int64;  (** Always 8 bytes (from value_repr_to_int64) *)
  tx_id : int64;
  fact_id : int64;
}

val encode_eavt : eavt_key -> bytes
(** Encode EAVT key to 41 bytes *)

val decode_eavt : bytes -> eavt_key
(** Decode 41 bytes to EAVT key *)

(** {2 AVET Index Keys} *)

type avet_key = {
  attr_id : int64;
  value_kind : Encoding.value_kind;
  value_repr : int64;
  entity_id : int64;
  tx_id : int64;
  fact_id : int64;
}

val encode_avet : avet_key -> bytes
(** Encode AVET key to 41 bytes *)

val decode_avet : bytes -> avet_key
(** Decode 41 bytes to AVET key *)

(** {2 SOURCE Index Keys} *)

type source_key = {
  source_id : int64;
  entity_id : int64;
  attr_id : int64;
  tx_id : int64;
  fact_id : int64;
}

val encode_source : source_key -> bytes
(** Encode SOURCE key to 41 bytes (padded) *)

val decode_source : bytes -> source_key
(** Decode 41 bytes to SOURCE key *)

(** {2 FACT Index Keys} *)

type fact_key = {
  fact_id : int64;
  tx_id : int64;
}

val encode_fact : fact_key -> bytes
(** Encode FACT key to 41 bytes (heavily padded) *)

val decode_fact : bytes -> fact_key
(** Decode 41 bytes to FACT key *)
