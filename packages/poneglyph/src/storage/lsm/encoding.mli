(** Encoding - Binary serialization for LSM storage
    
    All encoding functions produce sortable binary representations:
    - Lexicographic byte order = semantic order
    - Fixed-width when possible
    - Big-endian for multi-byte integers
    
    Example:
    {[
      (* Float encoding preserves order *)
      let e1 = encode_float (-100.0) in
      let e2 = encode_float 0.0 in
      let e3 = encode_float 100.0 in
      assert (Bytes.compare e1 e2 < 0);
      assert (Bytes.compare e2 e3 < 0);
      
      (* Round-trip works *)
      assert (decode_float (encode_float 3.14) = 3.14)
    ]}
*)

open Std
open Model

(** {2 Basic Types} *)

val encode_id : int64 -> bytes
(** Encode an int64 ID as 8 bytes (big-endian) *)

val decode_id : bytes -> int64
(** Decode an int64 ID from 8 bytes *)

(** {2 Float Encoding (Sortable)} *)

val encode_float : float -> int64
(** Encode a float to sortable int64 representation.
    
    Transformation ensures:
    - Negative floats < 0.0 < Positive floats
    - -1000.0 < -1.0 < 0.0 < 1.0 < 1000.0
    - -0.0 is normalized to +0.0
    
    Implementation:
    - Positive: flip sign bit (0x8000... XOR bits)
    - Negative: flip all bits (NOT bits)
*)

val decode_float : int64 -> float
(** Decode a sortable int64 back to float *)

(** {2 DateTime Encoding} *)

val encode_datetime : Datetime.t -> int64
(** Encode datetime as UTC microseconds since epoch *)

val decode_datetime : int64 -> Datetime.t
(** Decode UTC microseconds back to datetime *)

(** {2 String Encoding} *)

val hash_string : string -> int64
(** Hash a string to a stable int64 ID using SHA-256.
    
    The hash is used in index keys for sorting/filtering.
    The actual string is stored in the FACT value blob for reconstruction.
    Same approach as URI encoding. *)

(** {2 Value Encoding} *)

type value_kind =
  | VK_String  (** String values - hash in keys, full string in FACT blob *)
  | VK_Int
  | VK_Bool
  | VK_Float
  | VK_Uri
  | VK_DateTime

val value_kind_to_byte : value_kind -> int
(** Convert value_kind to single byte tag (0-5) *)

val value_kind_of_byte : int -> value_kind
(** Convert byte tag back to value_kind *)

type value_repr =
  | VString of int64  (** Interned ID or hash64 *)
  | VInt of int64
  | VBool of bool
  | VFloat of int64  (** Encoded sortable bits *)
  | VUri of int64  (** URI SHA-256 hash *)
  | VDatetime of int64  (** UTC micros *)

val encode_value : Fact.value -> value_kind * value_repr
val decode_value : value_kind -> value_repr -> Fact.value
val value_repr_to_int64 : value_repr -> int64
val int64_to_value_repr : value_kind -> int64 -> value_repr
(** Convert any value_repr to int64 for key encoding.
    Bool: 0/1, others: direct int64 *)
