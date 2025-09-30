(** Default hasher and random state for HashMap/HashSet *)

module DefaultHasher : Hasher.Intf
(** Default hasher using kernel's default hash algorithm *)

(** Random state for HashMap/HashSet - provides seeded hashing *)
module RandomState : sig
  type t

  val create : unit -> t
  (** Create a new random state with random seeds *)

  val hash_with_seed : t -> string -> int64 -> int64 -> Kernel.Crypto.hash
  (** Hash with this random state for DoS resistance *)

  val hash_with : t -> string -> Kernel.Crypto.hash
  (** Hash with this random state *)

  val to_int64 : t -> Kernel.Crypto.hash -> int64
  (** Convert hash to int64 mixed with seed for consistency *)
end
