(** SwissTable HashMap - High-performance hash table based on Google's SwissTable algorithm

    This implementation provides a drop-in replacement for HashMap with better performance
    characteristics:
    
    - Lower memory overhead (1 byte per entry vs 8+ bytes)
    - Better cache locality (control bytes stored separately)
    - Faster lookups (parallel scanning of control bytes)
    - Efficient iteration (dense bucket layout)
    
    The API is compatible with Kernel.Collections.HashMap for easy migration. *)
type ('k, 'v) t
(** The type of hash maps from keys of type ['k] to values of type ['v]. *)
(** {1 Creation} *)

val create: unit -> ('k, 'v) t

(** Creates a new empty hash map with default capacity. *)
val with_capacity: int -> ('k, 'v) t

(** Creates a new empty hash map with specified initial capacity. *)
val of_list: ('k * 'v) list -> ('k, 'v) t

(** Creates a hash map from a list of key-value pairs.
    If duplicate keys exist, later values override earlier ones. *)
(** {1 Basic Operations} *)

val insert: ('k, 'v) t -> 'k -> 'v -> 'v option

(** [insert map key value] inserts a key-value pair into the map.
    Returns [Some previous_value] if the key already existed, [None] otherwise. *)
val get: ('k, 'v) t -> 'k -> 'v option

(** [get map key] looks up a value by key.
    Returns [Some value] if key exists, [None] otherwise. *)
val remove: ('k, 'v) t -> 'k -> 'v option

(** [remove map key] removes a key from the map.
    Returns [Some value] if the key existed, [None] otherwise. *)
val contains_key: ('k, 'v) t -> 'k -> bool

(** [contains_key map key] checks if a key exists in the map. *)
val len: ('k, 'v) t -> int

(** [len map] returns the number of key-value pairs in the map. *)
val is_empty: ('k, 'v) t -> bool

(** [is_empty map] checks if the map contains no elements. *)
val clear: ('k, 'v) t -> unit

(** [clear map] removes all elements from the map. *)
(** {1 Iteration} *)

val keys: ('k, 'v) t -> 'k list

(** [keys map] returns a list of all keys in the map.
    The order is unspecified. *)
val values: ('k, 'v) t -> 'v list

(** [values map] returns a list of all values in the map.
    The order is unspecified. *)
val iter: ('k -> 'v -> unit) -> ('k, 'v) t -> unit

(** [iter f map] applies function [f] to each key-value pair.
    The iteration order is unspecified. *)
val fold: ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc

(** [fold f map acc] folds over all key-value pairs with an accumulator.
    The iteration order is unspecified. *)
val to_list: ('k, 'v) t -> ('k * 'v) list

(** [to_list map] converts the map to a list of key-value pairs.
    The order is unspecified. *)
(** {1 Entry API} *)

type ('k, 'v) entry =
  | Occupied of 'v
  (** Key exists with value *)
  | Vacant
(** Key does not exist *)
val entry: ('k, 'v) t -> 'k -> ('k, 'v) entry

(** [entry map key] gets the entry for a key for in-place manipulation. *)
val or_insert: ('k, 'v) t -> 'k -> 'v -> 'v

(** [or_insert map key default] inserts a default value if key is absent, returns the value. *)
val and_modify: ('k, 'v) t -> 'k -> ('v -> 'v) -> unit

(** [and_modify map key f] modifies the value if the key exists.
    No effect if the key is absent. *)
(** {1 Iterators} *)

val into_iter: ('k, 'v) t -> ('k * 'v) Kernel.Iter.Iterator.t

(** [into_iter map] converts the map into an iterator over key-value pairs. *)
val to_mut_iter: ('k, 'v) t -> ('k * 'v) Kernel.Iter.MutIterator.t

(** [to_mut_iter map] returns a mutable iterator over the map's key-value pairs. *)
