(** Encoding - Binary serialization for LSM storage *)

open Std
open Std.IO
open Model

(** {2 Basic Types} *)

let encode_id id =
  let buf = Bytes.create 8 in
  Bytes.set_int64_be buf 0 id;
  buf

let decode_id buf = Bytes.get_int64_be buf 0

(** {2 Float Encoding (Sortable)} *)

let encode_float f =
  (* Normalize -0.0 to +0.0 *)
  let f = if f = -0.0 then 0.0 else f in
  let bits = Int64.bits_of_float f in
  
  (* Check if negative (sign bit = 1) *)
  if Int64.shift_right_logical bits 63 = 1L then
    (* Negative: flip all bits to reverse order *)
    Int64.lognot bits
  else
    (* Positive: flip only sign bit to make it sort after negative *)
    Int64.logxor bits 0x8000000000000000L

let decode_float bits =
  (* Check if was negative (now has highest bit = 0 after lognot in encoding) *)
  let original =
    if Int64.shift_right_logical bits 63 = 1L then
      (* Was positive: flip sign bit back *)
      Int64.logxor bits 0x8000000000000000L
    else
      (* Was negative: flip all bits back *)
      Int64.lognot bits
  in
  Int64.float_of_bits original

(** {2 DateTime Encoding} *)

let encode_datetime dt =
  (* Use exact int64 microseconds - no float rounding errors *)
  Datetime.to_unix_micros dt

let decode_datetime micros =
  (* Convert int64 microseconds back to datetime *)
  Datetime.from_unix_micros micros

(** {2 String Encoding} *)

(** Hash a string to a stable int64 ID using SHA-256.
    
    This uses the same approach as Key.uri_to_id - the hash is used
    in index keys for sorting/filtering, while the actual string is
    stored in the FACT value blob for reconstruction. *)
let hash_string s =
  (* SHA-256 hash *)
  let hash = Crypto.Sha256.hash_string s in
  (* Extract first 8 bytes as int64 *)
  let hash_bytes = Crypto.Digest.bytes hash in
  Bytes.get_int64_be hash_bytes 0

(** {2 Value Encoding} *)

type value_kind =
  | VK_String  (** String values - hash stored in keys, full string in FACT blob *)
  | VK_Int
  | VK_Bool
  | VK_Float
  | VK_Uri
  | VK_DateTime

let value_kind_to_byte = function
  | VK_Uri -> 0
  | VK_String -> 1
  | VK_Int -> 2
  | VK_Bool -> 3
  | VK_Float -> 4
  | VK_DateTime -> 5

let value_kind_of_byte = function
  | 0 -> VK_Uri
  | 1 -> VK_String
  | 2 -> VK_Int
  | 3 -> VK_Bool
  | 4 -> VK_Float
  | 5 -> VK_DateTime
  | n -> panic ("Invalid value_kind byte: " ^ string_of_int n)

type value_repr =
  | VString of int64
  | VInt of int64
  | VBool of bool
  | VFloat of int64
  | VUri of int64  (* SHA-256 hash *)
  | VDatetime of int64

let encode_value = function
  | Fact.String s ->
      (* Generate hash for key use - actual string stored in FACT blob *)
      let id = hash_string s in
      (VK_String, VString id)
  | Fact.Int i -> (VK_Int, VInt (Int64.of_int i))
  | Fact.Bool b -> (VK_Bool, VBool b)
  | Fact.Float f -> (VK_Float, VFloat (encode_float f))
  | Fact.Uri u -> 
      (* Extract first 8 bytes of SHA-256 hash as int64 for index keys *)
      let id = Bytes.get_int64_be u.Uri.sha256 0 in
      (VK_Uri, VUri id)
  | Fact.DateTime dt -> (VK_DateTime, VDatetime (encode_datetime dt))

let decode_value kind repr =
  match (kind, repr) with
  | VK_String, _ ->
      (* String values cannot be decoded from hash - must use decode_fact_value 
         which reads the full string from FACT blob. This function is only used
         for key comparison, not fact reconstruction. *)
      panic "String values cannot be decoded from hash - use decode_fact_value from FACT blob"
  | VK_Int, VInt i -> Fact.Int (Int64.to_int i)
  | VK_Bool, VBool b -> Fact.Bool b
  | VK_Float, VFloat bits -> Fact.Float (decode_float bits)
  | VK_Uri, VUri _sha256 -> 
      (* URI values cannot be fully reconstructed from SHA-256 hash alone.
         Must use decode_fact_value which looks up the string from URIS index.
         This function is only used for key comparison, not fact reconstruction. *)
      panic "URI values cannot be decoded from hash - use decode_fact_value from FACT blob"
  | VK_DateTime, VDatetime micros -> Fact.DateTime (decode_datetime micros)
  | _ -> panic "Mismatched value_kind and value_repr in decode_value"

let value_repr_to_int64 = function
  | VString id -> id
  | VInt i -> i
  | VBool b -> if b then 1L else 0L
  | VFloat bits -> bits
  | VUri sha256 -> sha256
  | VDatetime micros -> micros

let int64_to_value_repr kind i64 =
  match kind with
  | VK_String -> VString i64
  | VK_Int -> VInt i64
  | VK_Bool -> VBool (i64 != 0L)
  | VK_Float -> VFloat i64
  | VK_Uri -> VUri i64
  | VK_DateTime -> VDatetime i64
