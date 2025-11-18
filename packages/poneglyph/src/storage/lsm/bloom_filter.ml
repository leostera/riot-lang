(** Bloom Filter - Probabilistic membership test *)

open Std

module Bytes = Kernel.IO.Bytes

(** Bloom filter structure *)
type t = {
  bits: bytes;              (* Bit array for the filter *)
  num_bits: int;            (* Total number of bits *)
  num_hashes: int;          (* Number of hash functions to use *)
  mutable keys_added: int;  (* Statistics: how many keys added *)
}

(** Calculate optimal number of hash functions
    
    Formula: k = (m/n) * ln(2)
    where m = num_bits, n = num_keys
    
    For bits_per_key, this simplifies to: k = bits_per_key * 0.69315
    
    Reference: "Less Hashing, Same Performance: Building a Better Bloom Filter"
    by Kirsch & Mitzenmacher (2006)
*)
let optimal_num_hashes bits_per_key =
  max 1 (int_of_float (float bits_per_key *. 0.69315))

(** Create a new bloom filter *)
let create ~num_keys ~bits_per_key =
  let num_bits = num_keys * bits_per_key in
  let num_bytes = (num_bits + 7) / 8 in  (* Round up to nearest byte *)
  let num_hashes = optimal_num_hashes bits_per_key in
  {
    bits = Bytes.make num_bytes '\x00';
    num_bits;
    num_hashes;
    keys_added = 0;
  }

(** Hash a key to get two independent hash values
    
    We use double hashing: h_i(x) = h1(x) + i * h2(x) mod m
    This lets us compute k different hash values from just 2 actual hashes.
    
    We use SHA-256 and extract:
    - h1: first 8 bytes as int64
    - h2: next 8 bytes as int64
*)
let hash_key (key : bytes) =
  (* Convert bytes to string for hashing *)
  let key_string = Bytes.to_string key in
  let hash = Crypto.Sha256.hash_string key_string in
  let hash_bytes = Crypto.Digest.bytes hash in
  let h1 = Bytes.get_int64_be hash_bytes 0 in
  let h2 = Bytes.get_int64_be hash_bytes 8 in
  (h1, h2)

(** Set a bit in the bit array *)
let set_bit bits bit_pos =
  let byte_pos = bit_pos / 8 in
  let bit_offset = bit_pos mod 8 in
  let current = Bytes.get_uint8 bits byte_pos in
  let mask = 1 lsl bit_offset in
  Bytes.set_uint8 bits byte_pos (current lor mask)

(** Get a bit from the bit array *)
let get_bit bits bit_pos =
  let byte_pos = bit_pos / 8 in
  let bit_offset = bit_pos mod 8 in
  let current = Bytes.get_uint8 bits byte_pos in
  let mask = 1 lsl bit_offset in
  (current land mask) != 0

(** Add a key to the bloom filter *)
let add t ~key =
  let (h1, h2) = hash_key key in
  
  (* Compute k hash values using double hashing *)
  for i = 0 to t.num_hashes - 1 do
    (* h_i = h1 + i*h2 *)
    let hash = Int64.add h1 (Int64.mul (Int64.of_int i) h2) in
    (* Map to bit position (mod num_bits) *)
    let bit_pos = Int64.to_int (Int64.unsigned_rem hash (Int64.of_int t.num_bits)) in
    set_bit t.bits bit_pos
  done;
  
  t.keys_added <- t.keys_added + 1

(** Check if a key might be in the bloom filter *)
let might_contain t ~key =
  let (h1, h2) = hash_key key in
  
  let rec check i =
    if i >= t.num_hashes then true  (* All bits are set *)
    else
      let hash = Int64.add h1 (Int64.mul (Int64.of_int i) h2) in
      let bit_pos = Int64.to_int (Int64.unsigned_rem hash (Int64.of_int t.num_bits)) in
      if not (get_bit t.bits bit_pos) then false  (* Found a 0 bit - definitely not present *)
      else check (i + 1)
  in
  check 0

(** Serialize to bytes
    
    Format:
    [num_bits:4][num_hashes:4][keys_added:4][bits:variable]
*)
let to_bytes t =
  let header_size = 12 in
  let total_size = header_size + Bytes.length t.bits in
  let buf = Bytes.create total_size in
  
  Bytes.set_int32_be buf 0 (Int32.of_int t.num_bits);
  Bytes.set_int32_be buf 4 (Int32.of_int t.num_hashes);
  Bytes.set_int32_be buf 8 (Int32.of_int t.keys_added);
  Bytes.blit t.bits 0 buf header_size (Bytes.length t.bits);
  
  buf

(** Deserialize from bytes *)
let from_bytes buf =
  if Bytes.length buf < 12 then
    Error "Bloom filter too small (need at least 12 bytes for header)"
  else
    let num_bits = Int32.to_int (Bytes.get_int32_be buf 0) in
    let num_hashes = Int32.to_int (Bytes.get_int32_be buf 4) in
    let keys_added = Int32.to_int (Bytes.get_int32_be buf 8) in
    
    let num_bytes = (num_bits + 7) / 8 in
    let expected_size = 12 + num_bytes in
    
    if Bytes.length buf != expected_size then
      Error ("Bloom filter size mismatch: expected " ^ string_of_int expected_size ^ 
             " bytes, got " ^ string_of_int (Bytes.length buf))
    else
      let bits = Bytes.sub buf 12 num_bytes in
      Ok { bits; num_bits; num_hashes; keys_added }

(** Get byte size *)
let byte_size t = 12 + Bytes.length t.bits

(** Statistics type - must match .mli *)
type stats = {
  num_bits: int;
  num_hashes: int;
  bits_set: int;
  fill_ratio: float;
}

(** Get statistics *)
let stats t =
  (* Count set bits using Brian Kernighan's algorithm *)
  let count_set_bits () =
    let count = ref 0 in
    for byte_pos = 0 to Bytes.length t.bits - 1 do
      let byte = Bytes.get_uint8 t.bits byte_pos in
      (* Count bits: keep clearing lowest set bit *)
      let rec count_bits b c =
        if b = 0 then c
        else count_bits (b land (b - 1)) (c + 1)
      in
      count := !count + count_bits byte 0
    done;
    !count
  in
  
  let bits_set = count_set_bits () in
  let fill_ratio = float bits_set /. float t.num_bits in
  
  {
    num_bits = t.num_bits;
    num_hashes = t.num_hashes;
    bits_set;
    fill_ratio;
  }
