(**
   # Crypto - Cryptographic hashing

   Unified interface for cryptographic hash algorithms with support for
   multiple algorithms, digest formats, and DoS-resistant hashing.

   ## Examples

   Basic hashing:

   ```ocaml open Std

   let hash = Crypto.hash_string "Hello, World!" in let hex_digest =
   Crypto.Digest.hex hash in (*
   "315f5bdb76d078c43b8ac0064e4a0164612b1fce77c869345bfc94c75894edd3" *)

   let hash = Crypto.hash_int 42 in let b64_digest = Crypto.Digest.base64 hash
   ```

   Using specific algorithms:

   ```ocaml module H = Crypto.Sha256 in let state = H.create () in
   H.write state "Hello";
   H.write state ", World!";
   let hash = H.finish state in
   Crypto.Digest.hex hash ```

   ## Available Algorithms

   - **SHA-256**: Secure, widely used, 256-bit output
   - **SHA-512**: More secure, 512-bit output
   - **MD5**: Legacy, not cryptographically secure (use for checksums only)

   ## Use Cases

   - Content-addressed storage
   - Data integrity verification
   - Non-cryptographic hashing for HashMap/HashSet
   - Password hashing (use proper KDFs like Argon2, not these!)
*)

(** ## Core Types *)

(** Universal hash type produced by all hash algorithms. *)
type hash = Hash.t
(** ## Modules *)
module Hash = Hash

module Hasher = Hasher

(** Hasher interface and utilities *)
module Digest = Digest

(** Digest formatting functions *)
(** ## Algorithms *)

module Sha1: Hasher.Intf

module Sha256: Hasher.Intf

module Sha512: Hasher.Intf

module Md5: Hasher.Intf

(** ## Defaults *)

module DefaultHasher = Default.DefaultHasher

(** Default hasher for general use *)
module RandomState = Default.RandomState

(** Random state for HashMap/HashSet *)
(** ## Convenience Functions *)

val hash_string: string -> hash

(** Hash a string directly. *)
val hash_bytes: bytes -> hash

val hash_unit: unit -> hash

val hash_int: int -> hash

val hash_int32: int32 -> hash

val hash_int64: int64 -> hash

val hash_float: float -> hash

val hash_bool: bool -> hash

val hash_list: ('a -> hash) -> 'a list -> hash

val hash_array: ('a -> hash) -> 'a array -> hash

(** HMAC-SHA256 over [data] using [key], returned as raw digest bytes. *)
val hmac_sha256: key:string -> data:string -> string
