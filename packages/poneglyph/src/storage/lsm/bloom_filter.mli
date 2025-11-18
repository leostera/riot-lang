(** Bloom Filter - Probabilistic membership test for fast negative lookups
    
    A Bloom filter is a space-efficient probabilistic data structure that
    tests whether an element is a member of a set. False positives are possible,
    but false negatives are not.
    
    This is critical for LSM storage: before reading a block from disk, check
    the bloom filter. If it says "definitely not present", skip the expensive
    disk I/O. This can eliminate 90%+ of unnecessary disk reads.
    
    Design:
    - 10 bits per key (default, configurable)
    - ~1% false positive rate with optimal hash functions
    - Double hashing technique (h1 + i*h2) for efficiency
    - Stored in SSTable footer for fast loading
    
    Usage:
    ```ocaml
    (* Building a bloom filter *)
    let bloom = BloomFilter.create ~num_keys:1000 ~bits_per_key:10 in
    List.iter (fun key -> BloomFilter.add bloom ~key) keys;
    let bytes = BloomFilter.to_bytes bloom in
    
    (* Querying *)
    if not (BloomFilter.might_contain bloom ~key) then
      None  (* Definitely not present - skip disk read! *)
    else
      read_from_disk key  (* Maybe present, do the expensive lookup *)
    ```
*)

open Std

(** A bloom filter instance *)
type t

(** Create a new bloom filter
    
    @param num_keys Expected number of keys to be added
    @param bits_per_key Bits allocated per key (higher = fewer false positives)
                        Recommended: 10 (1% FP rate), 12 (0.5% FP rate)
*)
val create : num_keys:int -> bits_per_key:int -> t

(** Add a key to the bloom filter
    
    This sets multiple bits in the filter based on the key's hash.
    Keys can be added but never removed.
    
    @param key The bytes to add (typically a 41-byte index key)
*)
val add : t -> key:bytes -> unit

(** Test if a key might be in the set
    
    Returns:
    - false: Key is DEFINITELY NOT in the set (no false negatives)
    - true: Key MIGHT be in the set (possible false positive)
    
    @param key The bytes to test
*)
val might_contain : t -> key:bytes -> bool

(** Serialize the bloom filter to bytes for storage
    
    Format: [num_bits:4][num_hashes:4][keys_added:4][bits:variable]
*)
val to_bytes : t -> bytes

(** Deserialize a bloom filter from bytes
    
    @return Error if the bytes are malformed
*)
val from_bytes : bytes -> (t, string) result

(** Get the size of the bloom filter in bytes *)
val byte_size : t -> int

(** Statistics about the bloom filter *)
type stats = {
  num_bits: int;
  num_hashes: int;
  bits_set: int;
  fill_ratio: float;
}

(** Get statistics about the bloom filter (for debugging/tuning)
    
    Returns:
    - num_bits: Total bits in the filter
    - num_hashes: Number of hash functions used
    - bits_set: Number of bits currently set to 1
    - fill_ratio: Fraction of bits set (bits_set / num_bits)
*)
val stats : t -> stats
