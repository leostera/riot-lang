(** Hasher module type and utilities *)

(** Interface that all hash algorithms must implement *)
module type Intf = sig
  (** Internal state of the hasher *)

  (** Create a new hasher state *)
  type state

  val create: unit -> state

  (** Write immutable string data to the hasher - mutates state *)
  val write: state -> string -> unit

  val write_iovec: state -> Kernel.IO.IoVec.t -> unit

  val write_hash: state -> Hash.t -> unit

  val write_unit: state -> unit -> unit

  val write_int: state -> int -> unit

  val write_int32: state -> int32 -> unit

  val write_int64: state -> int64 -> unit

  val write_float: state -> float -> unit

  val write_bool: state -> bool -> unit

  val write_list: (state -> 'a -> unit) -> state -> 'a list -> unit

  val write_array: (state -> 'a -> unit) -> state -> 'a array -> unit

  (** Finalize and get the hash *)
  val finish: state -> Hash.t

  (** Convenience functions to hash values directly *)
  val hash_string: string -> Hash.t

  val hash_bytes: bytes -> Hash.t

  val hash_unit: unit -> Hash.t

  val hash_int: int -> Hash.t

  val hash_int32: int32 -> Hash.t

  val hash_int64: int64 -> Hash.t

  val hash_float: float -> Hash.t

  val hash_bool: bool -> Hash.t

  val hash_list: ('a -> Hash.t) -> 'a list -> Hash.t

  val hash_array: ('a -> Hash.t) -> 'a array -> Hash.t
end
