(**
   Hash algorithm interface.

   Module signature that all hash algorithms implement, providing stateful
   hashing with incremental updates.

   ## Examples

   ```ocaml open Std

   (* Use any algorithm that implements Hasher.Intf *) module H = Crypto.Sha256 in
   let state = H.create () in
   H.write state "Part 1";
   H.write state "Part 2";
   let hash = H.finish state in
   Crypto.Digest.hex hash

   (* Direct hashing - one-shot *) let hash = H.hash_string "Hello, World!" in
   Crypto.Digest.hex hash ```

   ## When to Use

   - **Stateful API** ([create], [write], [finish]): When hashing streaming
     data
   - **Direct API** ([hash_string], etc.): When hashing complete values

   See [Crypto] for the default algorithm and convenience functions.
*)

(** Interface that all hash algorithms must implement *)
module type Intf = sig
  (** Internal state of the state *)

  (** Create a new state state *)
  type state

  val create: unit -> state

  (** Write immutable string data to the state. *)
  val write: state -> string -> unit

  (** Write an existing digest/hash value into the state. *)
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
