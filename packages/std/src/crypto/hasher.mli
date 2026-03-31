(** # Hasher - Hash algorithm interface

    Module signature that all hash algorithms implement, providing stateful
    hashing with incremental updates.

    ## Examples

    ```ocaml open Std

    (* Use any algorithm that implements Hasher.Intf *) module H = Crypto.Sha256
    in

    (* Stateful hashing - update incrementally *) let state = H.create () in let
    state = H.update state "Part 1" in let state = H.update state "Part 2" in
    let hash = H.finish state in Crypto.Digest.hex hash

    (* Direct hashing - one-shot *) let hash = H.hash_string "Hello, World!" in
    Crypto.Digest.hex hash ```

    ## When to Use

    - **Stateful API** ([create], [update], [finish]): When hashing streaming
      data
    - **Direct API** ([hash_string], etc.): When hashing complete values

    See [Crypto] for the default algorithm and convenience functions. *)

(** Interface that all hash algorithms must implement *)
module type Intf = sig
  (** Internal state of the state *)
  (** Create a new state state *)
  type state
  val create: unit -> state

  (** Write data to the state *)
  val write: state -> bytes -> unit

  val write_string: state -> string -> unit

  val write_unit: state -> unit -> unit

  val write_int: state -> int -> unit

  val write_int32: state -> int32 -> unit

  val write_int64: state -> int64 -> unit

  val write_float: state -> float -> unit

  val write_bool: state -> bool -> unit

  val write_list: (state -> 'a -> unit) -> state -> 'a list -> unit

  val write_array: (state -> 'a -> unit) -> state -> 'a array -> unit

  (** Finalize and get the hash *)
  val finish: state -> Kernel.Crypto.hash

  (** Convenience functions to hash values directly *)
  val hash_string: string -> Kernel.Crypto.hash

  val hash_bytes: bytes -> Kernel.Crypto.hash

  val hash_unit: unit -> Kernel.Crypto.hash

  val hash_int: int -> Kernel.Crypto.hash

  val hash_int32: int32 -> Kernel.Crypto.hash

  val hash_int64: int64 -> Kernel.Crypto.hash

  val hash_float: float -> Kernel.Crypto.hash

  val hash_bool: bool -> Kernel.Crypto.hash

  val hash_list: ('a -> Kernel.Crypto.hash) -> 'a list -> Kernel.Crypto.hash

  val hash_array: ('a -> Kernel.Crypto.hash) -> 'a array -> Kernel.Crypto.hash
end
