(** Core cryptographic types *)
type hash = Hash.t

(** Universal hash type - all hash algorithms produce this type *)
module Hash = Hash

module FFI = struct
  let md5 = fun data -> Hash.of_bytes (Crypto_stubs.md5 data)

  let md5_iovec = fun iov -> Hash.of_bytes (Crypto_stubs.md5_iovec iov)

  let sha1 = fun data -> Hash.of_bytes (Crypto_stubs.sha1 data)

  let sha1_iovec = fun iov -> Hash.of_bytes (Crypto_stubs.sha1_iovec iov)

  let sha256 = fun data -> Hash.of_bytes (Crypto_stubs.sha256 data)

  let sha256_iovec = fun iov -> Hash.of_bytes (Crypto_stubs.sha256_iovec iov)

  let sha512 = fun data -> Hash.of_bytes (Crypto_stubs.sha512 data)

  let sha512_iovec = fun iov -> Hash.of_bytes (Crypto_stubs.sha512_iovec iov)

  let hmac_sha256 = Crypto_stubs.hmac_sha256

  let default_hash = fun data -> Hash.of_bytes (Crypto_stubs.default_hash data)

  let default_hash_iovec = fun iov -> Hash.of_bytes (Crypto_stubs.default_hash_iovec iov)
end
