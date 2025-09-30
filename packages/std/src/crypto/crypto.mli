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

val hash_string : string -> hash
(** Quick hash functions using the default hasher *)

val hash_bytes : bytes -> hash
