(** Encoding - Binary serialization for LSM storage *)

open Std
open Model

(* Get Bytes from Kernel *)
module Bytes = Kernel.IO.Bytes

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
  (* Convert to UTC timestamp (seconds) then to microseconds *)
  let timestamp = Datetime.to_timestamp dt in
  Int64.of_float (timestamp *. 1_000_000.0)

let decode_datetime micros =
  (* Convert microseconds back to seconds *)
  let seconds = Int64.to_float micros /. 1_000_000.0 in
  Datetime.from_unix_time seconds

(** {2 Value Encoding} *)

type value_kind =
  | VK_String
  | VK_Int
  | VK_Bool
  | VK_Float
  | VK_Uri
  | VK_DateTime

let value_kind_to_byte = function
  | VK_String -> 0
  | VK_Int -> 1
  | VK_Bool -> 2
  | VK_Float -> 3
  | VK_Uri -> 4
  | VK_DateTime -> 5

let value_kind_of_byte = function
  | 0 -> VK_String
  | 1 -> VK_Int
  | 2 -> VK_Bool
  | 3 -> VK_Float
  | 4 -> VK_Uri
  | 5 -> VK_DateTime
  | n -> panic ("Invalid value_kind byte: " ^ string_of_int n)

type value_repr =
  | VString of int64
  | VInt of int64
  | VBool of bool
  | VFloat of int64
  | VUri of int
  | VDatetime of int64

let encode_value = function
  | Fact.String s ->
      (* For now: simple hash (TODO: interning/hash64 for large strings) *)
      let h = ref 0L in
      String.iter (fun c -> 
        h := Int64.add (Int64.mul !h 31L) (Int64.of_int (Char.code c))
      ) s;
      (VK_String, VString !h)
  | Fact.Int i -> (VK_Int, VInt (Int64.of_int i))
  | Fact.Bool b -> (VK_Bool, VBool b)
  | Fact.Float f -> (VK_Float, VFloat (encode_float f))
  | Fact.Uri u -> (VK_Uri, VUri u)  (* Uri.t is already int *)
  | Fact.DateTime dt -> (VK_DateTime, VDatetime (encode_datetime dt))

let decode_value kind repr =
  match (kind, repr) with
  | VK_String, VString h ->
      (* For now: can't decode string from hash - this is a limitation *)
      (* TODO: proper string interning/storage *)
      Fact.String ("<hash:" ^ Int64.to_string h ^ ">")
  | VK_Int, VInt i -> Fact.Int (Int64.to_int i)
  | VK_Bool, VBool b -> Fact.Bool b
  | VK_Float, VFloat bits -> Fact.Float (decode_float bits)
  | VK_Uri, VUri u -> Fact.Uri u
  | VK_DateTime, VDatetime micros -> Fact.DateTime (decode_datetime micros)
  | _ -> panic "Mismatched value_kind and value_repr in decode_value"

let value_repr_to_int64 = function
  | VString id -> id
  | VInt i -> i
  | VBool b -> if b then 1L else 0L
  | VFloat bits -> bits
  | VUri u -> Int64.of_int u
  | VDatetime micros -> micros
