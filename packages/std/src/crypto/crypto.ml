(** Cryptographic hashing module

    Provides a unified interface for hash algorithms with support for:
    - Multiple hash algorithms (SHA256, SHA512, MD5, etc.)
    - Consistent digest formats (hex, base64, etc.)
    - Extensible design for adding new algorithms
    - DoS-resistant hashing for HashMap/HashSet *)
type hash = Kernel.Crypto.hash

module Hash = Kernel.Crypto.Hash
module Hasher = Hasher
module Digest = Digest

(* Algorithm implementations - from algo/ subdirectory *)

module Sha1 = Algo.Sha1
module Sha256 = Algo.Sha256
module Sha512 = Algo.Sha512
module Md5 = Algo.Md5

(* Re-export the default hasher for convenience *)

module DefaultHasher = Default.DefaultHasher
module RandomState = Default.RandomState

(* Quick hash functions using the default hasher *)

let hash_string = DefaultHasher.hash_string

let hash_bytes = DefaultHasher.hash_bytes

let hash_unit = DefaultHasher.hash_unit

let hash_int = DefaultHasher.hash_int

let hash_int32 = DefaultHasher.hash_int32

let hash_int64 = DefaultHasher.hash_int64

let hash_float = DefaultHasher.hash_float

let hash_bool = DefaultHasher.hash_bool

let hash_list = DefaultHasher.hash_list

let hash_array = DefaultHasher.hash_array
