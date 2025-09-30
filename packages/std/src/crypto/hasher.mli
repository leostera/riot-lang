(** Hasher module type and utilities *)

(** Interface that all hash algorithms must implement *)
module type Intf = sig
  type state
  (** Internal state of the hasher *)

  val create : unit -> state
  (** Create a new hasher state *)

  val write : state -> bytes -> unit
  (** Write data to the hasher *)

  val write_string : state -> string -> unit
  val write_unit : state -> unit -> unit
  val write_int : state -> int -> unit
  val write_int32 : state -> int32 -> unit
  val write_int64 : state -> int64 -> unit
  val write_float : state -> float -> unit
  val write_bool : state -> bool -> unit
  val write_list : (state -> 'a -> unit) -> state -> 'a list -> unit
  val write_array : (state -> 'a -> unit) -> state -> 'a array -> unit

  val finish : state -> Kernel.Crypto.hash
  (** Finalize and get the hash *)

  val hash_string : string -> Kernel.Crypto.hash
  (** Convenience functions to hash values directly *)

  val hash_bytes : bytes -> Kernel.Crypto.hash
  val hash_unit : unit -> Kernel.Crypto.hash
  val hash_int : int -> Kernel.Crypto.hash
  val hash_int32 : int32 -> Kernel.Crypto.hash
  val hash_int64 : int64 -> Kernel.Crypto.hash
  val hash_float : float -> Kernel.Crypto.hash
  val hash_bool : bool -> Kernel.Crypto.hash
  val hash_list : ('a -> Kernel.Crypto.hash) -> 'a list -> Kernel.Crypto.hash
  val hash_array : ('a -> Kernel.Crypto.hash) -> 'a array -> Kernel.Crypto.hash
end
