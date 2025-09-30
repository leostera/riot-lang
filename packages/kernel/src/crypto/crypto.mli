(** Core cryptographic types and operations *)

type hash = Hash.t
(** The universal hash type produced by all hash algorithms *)

module Hash = Hash

module FFI : sig
  val sha256 : string -> hash
  val sha512 : string -> hash

  val default_hash :
    string -> hash (* This is the algorithm for the default hasher *)
end
