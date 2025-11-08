(** Core cryptographic types *)

type hash = Hash.t
(** Universal hash type - all hash algorithms produce this type *)

module Hash = Hash

module FFI = struct
  let md5 data = Hash.of_bytes (Crypto_stubs.md5 data)
  let sha1 data = Hash.of_bytes (Crypto_stubs.sha1 data)
  let sha256 data = Hash.of_bytes (Crypto_stubs.sha256 data)
  let sha512 data = Hash.of_bytes (Crypto_stubs.sha512 data)
  let default_hash data = Hash.of_bytes (Crypto_stubs.default_hash data)
end
