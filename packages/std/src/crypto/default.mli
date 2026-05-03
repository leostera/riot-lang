(**
   Default hash algorithms and randomized hash state.

   Provides the default hash algorithm and randomized hashing for collections
   to prevent denial-of-service attacks via hash collision.

   ## Examples

   ```ocaml open Std

   (* Use default hasher directly *) module H = Crypto.DefaultHasher in let
   hash = H.hash_string "data" in Crypto.Digest.hex hash

   (* RandomState for HashMap/HashSet - automatic DoS protection *) let
   random_state = Crypto.RandomState.create () in let hash1 =
   Crypto.RandomState.hash_with random_state "key1" in let hash2 =
   Crypto.RandomState.hash_with random_state "key2" (* Each RandomState.create
   () produces different seeds *) ```

   ## When to Use

   - **DefaultHasher**: When you need a hasher but don't care which algorithm
   - **RandomState**: Internal use by [HashMap] and [HashSet] for DoS
     resistance

   @see <https://en.wikipedia.org/wiki/Hash_table#Collision_resolution>
     Hash collision attacks
*)

(** Default hasher using kernel's default hash algorithm. *)
module DefaultHasher: Hasher.Intf

(** Random state for HashMap/HashSet - provides seeded hashing. *)
module RandomState: sig
  type t

  (** Create a new random state with random seeds *)
  val create: unit -> t

  (** Hash with this random state for DoS resistance *)
  val hash_with_seed: t -> string -> int64 -> int64 -> Hash.t

  (** Hash with this random state *)
  val hash_with: t -> string -> Hash.t

  (** Convert hash to int64 mixed with seed for consistency *)
  val to_int64: t -> Hash.t -> int64
end
