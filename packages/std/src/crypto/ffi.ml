open Kernel

external md5_bytes: bytes -> bytes = "std_crypto_md5"

external md5_iovec_bytes: IO.IoVec.t -> bytes = "std_crypto_md5_iovec"

external sha1_bytes: bytes -> bytes = "std_crypto_sha1"

external sha1_iovec_bytes: IO.IoVec.t -> bytes = "std_crypto_sha1_iovec"

external sha256_bytes: bytes -> bytes = "std_crypto_sha256"

external sha256_iovec_bytes: IO.IoVec.t -> bytes = "std_crypto_sha256_iovec"

external sha512_bytes: bytes -> bytes = "std_crypto_sha512"

external sha512_iovec_bytes: IO.IoVec.t -> bytes = "std_crypto_sha512_iovec"

external simple_hash_bytes: bytes -> bytes = "std_crypto_simple_hash"

external simple_hash_iovec_bytes: IO.IoVec.t -> bytes = "std_crypto_simple_hash_iovec"

external hmac_sha256_bytes: string -> string -> bytes = "std_crypto_hmac_sha256"

let md5 = fun data -> Hash.from_bytes (md5_bytes (Bytes.from_string data))

let md5_iovec = fun iov -> Hash.from_bytes (md5_iovec_bytes iov)

let sha1 = fun data -> Hash.from_bytes (sha1_bytes (Bytes.from_string data))

let sha1_iovec = fun iov -> Hash.from_bytes (sha1_iovec_bytes iov)

let sha256 = fun data -> Hash.from_bytes (sha256_bytes (Bytes.from_string data))

let sha256_iovec = fun iov -> Hash.from_bytes (sha256_iovec_bytes iov)

let sha512 = fun data -> Hash.from_bytes (sha512_bytes (Bytes.from_string data))

let sha512_iovec = fun iov -> Hash.from_bytes (sha512_iovec_bytes iov)

let hmac_sha256 = fun ~key ~data -> hmac_sha256_bytes key data

let default_hash = fun data -> Hash.from_bytes (simple_hash_bytes (Bytes.from_string data))

let default_hash_iovec = fun iov -> Hash.from_bytes (simple_hash_iovec_bytes iov)
