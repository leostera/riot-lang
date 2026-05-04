(**
   Cache-friendly hash map based on Google's SwissTable algorithm.

   This implementation provides a drop-in replacement for HashMap with better performance
   characteristics:

   - Lower memory overhead (1 byte per entry vs 8+ bytes)
   - Better cache locality (control bytes stored separately)
   - Faster lookups (parallel scanning of control bytes)
   - Efficient iteration (dense bucket layout)

   The API is compatible with Kernel.Collections.HashMap for easy migration.
*)

(** The type of hash maps from keys of type `'k` to values of type `'v`. *)
type ('k, 'v) t

(** Creates a new empty hash map with default capacity. *)
val create: unit -> ('k, 'v) t

(** Creates a new empty hash map with specified initial capacity. *)
val with_capacity: int -> ('k, 'v) t

(**
   Creates a hash map from a list of key-value pairs.
   If duplicate keys exist, later values override earlier ones.
*)
val from_list: ('k * 'v) list -> ('k, 'v) t

(**
   `insert map key value` inserts a key-value pair into the map.
   Returns `Some previous_value` if the key already existed, `None` otherwise.
*)
val insert: ('k, 'v) t -> 'k -> 'v -> 'v option

(**
   `get map key` looks up a value by key.
   Returns `Some value` if key exists, `None` otherwise.
*)
val get: ('k, 'v) t -> 'k -> 'v option

(**
   `remove map key` removes a key from the map.
   Returns `Some value` if the key existed, `None` otherwise.
*)
val remove: ('k, 'v) t -> 'k -> 'v option

(** `contains_key map key` checks if a key exists in the map. *)
val contains_key: ('k, 'v) t -> 'k -> bool

(** `len map` returns the number of key-value pairs in the map. *)
val len: ('k, 'v) t -> int

(** `is_empty map` checks if the map contains no elements. *)
val is_empty: ('k, 'v) t -> bool

(** `clear map` removes all elements from the map. *)
val clear: ('k, 'v) t -> unit

(**
   `keys map` returns a list of all keys in the map.
   The order is unspecified.
*)
val keys: ('k, 'v) t -> 'k list

(**
   `values map` returns a list of all values in the map.
   The order is unspecified.
*)
val values: ('k, 'v) t -> 'v list

(**
   `iter f map` applies function `f` to each key-value pair.
   The order is unspecified.
*)
val iter: ('k -> 'v -> unit) -> ('k, 'v) t -> unit

(**
   `fold f map acc` folds over all key-value pairs with an accumulator.
   The iteration order is unspecified.
*)
val fold: ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc

(**
   `to_list map` converts the map to a list of key-value pairs.
   The order is unspecified.
*)
val to_list: ('k, 'v) t -> ('k * 'v) list

type ('k, 'v) entry =
  (** Key exists with value. *)
  | Occupied of 'v
  (** Key does not exist. *)
  | Vacant

(** `entry map key` gets the entry for a key for in-place manipulation. *)
val entry: ('k, 'v) t -> 'k -> ('k, 'v) entry

(** `or_insert map key default` inserts a default value if key is absent, returns the value. *)
val or_insert: ('k, 'v) t -> 'k -> 'v -> 'v

(**
   `and_modify map key f` modifies the value if the key exists.
   No effect if the key is absent.
*)
val and_modify: ('k, 'v) t -> 'k -> ('v -> 'v) -> unit

(** `into_iter map` converts the map into an iterator over key-value pairs. *)
val into_iter: ('k, 'v) t -> ('k * 'v) Std.Iter.Iterator.t

(** `to_mut_iter map` returns a mutable iterator over the map's key-value pairs. *)
val to_mut_iter: ('k, 'v) t -> ('k * 'v) Std.Iter.MutIterator.t
