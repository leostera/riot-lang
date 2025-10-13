(** C stub bindings for cryptographic functions *)

external sha1_bytes : bytes -> bytes = "kernel_crypto_sha1"
external sha256_bytes : bytes -> bytes = "kernel_crypto_sha256"
external sha512_bytes : bytes -> bytes = "kernel_crypto_sha512"
external simple_hash_bytes : bytes -> bytes = "kernel_crypto_simple_hash"

(* High-level wrappers that return raw bytes - crypto.ml will wrap them as hashes *)
let sha1 data =
  let bytes = Bytes.unsafe_of_string data in
  sha1_bytes bytes

let sha256 data =
  let bytes = Bytes.unsafe_of_string data in
  sha256_bytes bytes

let sha512 data =
  let bytes = Bytes.unsafe_of_string data in
  sha512_bytes bytes

let default_hash data =
  let bytes = Bytes.unsafe_of_string data in
  simple_hash_bytes bytes
