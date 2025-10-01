(** Cryptographic hashing module

    Provides a unified interface for hash algorithms with support for:
    - Multiple hash algorithms (SHA256, SHA512, MD5, etc.)
    - Consistent digest formats (hex, base64, etc.)
    - Extensible design for adding new algorithms
    - DoS-resistant hashing for HashMap/HashSet *)

(** {1 Core Types} *)

type hash = Kernel.Crypto.hash
(** The universal hash type produced by all hash algorithms *)

(** {1 Modules} *)

module Hasher = Hasher
(** Hasher interface and utilities *)

module Digest = Digest
(** Digest formatting functions *)

(** {1 Algorithms} *)

module Sha256 : Hasher.Intf
module Sha512 : Hasher.Intf
module Md5 : Hasher.Intf

(** {1 Defaults} *)

module DefaultHasher = Default.DefaultHasher
(** Default hasher for general use *)

module RandomState = Default.RandomState
(** Random state for HashMap/HashSet *)

val hash_string : string -> Kernel.Crypto.hash
(** {1 Convenience functions to hash values directly} *)

val hash_bytes : bytes -> Kernel.Crypto.hash
val hash_unit : unit -> Kernel.Crypto.hash
val hash_int : int -> Kernel.Crypto.hash
val hash_int32 : int32 -> Kernel.Crypto.hash
val hash_int64 : int64 -> Kernel.Crypto.hash
val hash_float : float -> Kernel.Crypto.hash
val hash_bool : bool -> Kernel.Crypto.hash
val hash_list : ('a -> Kernel.Crypto.hash) -> 'a list -> Kernel.Crypto.hash
val hash_array : ('a -> Kernel.Crypto.hash) -> 'a array -> Kernel.Crypto.hash
