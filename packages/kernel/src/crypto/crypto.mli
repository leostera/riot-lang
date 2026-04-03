(** Core cryptographic types and operations *)
(** The universal hash type produced by all hash algorithms *)
type hash = Hash.t
module Hash = Hash

module FFI: sig
  val md5: string -> hash

  val md5_iovec: IO.Iovec.t -> hash

  val sha1: string -> hash

  val sha1_iovec: IO.Iovec.t -> hash

  val sha256: string -> hash

  val sha256_iovec: IO.Iovec.t -> hash

  val sha512: string -> hash

  val sha512_iovec: IO.Iovec.t -> hash

  val hmac_sha256: key:string -> data:string -> bytes

  val default_hash: string -> hash (* This is the algorithm for the default hasher *)

  val default_hash_iovec: IO.Iovec.t -> hash
end
