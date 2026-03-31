(** C stub bindings for cryptographic functions *)
open IO

external sha1_bytes: bytes -> bytes = "kernel_crypto_sha1"

external sha256_bytes: bytes -> bytes = "kernel_crypto_sha256"

external sha512_bytes: bytes -> bytes = "kernel_crypto_sha512"

external md5_bytes: bytes -> bytes = "kernel_crypto_md5"

external simple_hash_bytes: bytes -> bytes = "kernel_crypto_simple_hash"

external hmac_sha256_bytes: string -> string -> bytes = "kernel_crypto_hmac_sha256"

(* High-level wrappers that return raw bytes - crypto.ml will wrap them as hashes *)

let sha1 = fun data ->
    let bytes = Bytes.unsafe_of_string data in
    sha1_bytes bytes

let sha256 = fun data ->
    let bytes = Bytes.unsafe_of_string data in
    sha256_bytes bytes

let sha512 = fun data ->
    let bytes = Bytes.unsafe_of_string data in
    sha512_bytes bytes

let md5 = fun data ->
    let bytes = Bytes.unsafe_of_string data in
    md5_bytes bytes

let default_hash = fun data ->
    let bytes = Bytes.unsafe_of_string data in
    simple_hash_bytes bytes

let hmac_sha256 = fun ~key ~data -> hmac_sha256_bytes key data
