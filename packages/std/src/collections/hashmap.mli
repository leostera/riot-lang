(** A hash table implementation similar to Rust's HashMap *)

type ('k, 'v) t
(** The type of hash maps from keys of type ['k] to values of type ['v] *)

(** {1 Creation} *)

val create : unit -> ('k, 'v) t
(** Create an empty hash map *)

val with_capacity : int -> ('k, 'v) t
(** Create an empty hash map with a given initial capacity *)

val of_list : ('k * 'v) list -> ('k, 'v) t
(** Create a hash map from a list of key-value pairs *)

(** {1 Basic Operations} *)

val insert : ('k, 'v) t -> 'k -> 'v -> 'v option
(** [insert map key value] inserts a key-value pair into the map. Returns
    [Some previous_value] if the key already existed, [None] otherwise. *)

val get : ('k, 'v) t -> 'k -> 'v option
(** [get map key] returns [Some value] if the key exists, [None] otherwise *)

val remove : ('k, 'v) t -> 'k -> 'v option
(** [remove map key] removes the key from the map. Returns [Some value] if the
    key existed, [None] otherwise. *)

val contains_key : ('k, 'v) t -> 'k -> bool
(** [contains_key map key] returns [true] if the key exists in the map *)

val len : ('k, 'v) t -> int
(** [len map] returns the number of key-value pairs in the map *)

val is_empty : ('k, 'v) t -> bool
(** [is_empty map] returns [true] if the map contains no elements *)

val clear : ('k, 'v) t -> unit
(** [clear map] removes all elements from the map *)

(** {1 Iteration} *)

val keys : ('k, 'v) t -> 'k list
(** [keys map] returns a list of all keys in the map *)

val values : ('k, 'v) t -> 'v list
(** [values map] returns a list of all values in the map *)

val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
(** [iter f map] applies function [f] to each key-value pair *)

val fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc
(** [fold f map acc] folds over all key-value pairs *)

val to_list : ('k, 'v) t -> ('k * 'v) list
(** [to_list map] returns all key-value pairs as a list *)

(** {1 Entry API} *)

(** Entry type for advanced key manipulation *)
type ('k, 'v) entry = Occupied of 'v ref | Vacant

val entry : ('k, 'v) t -> 'k -> ('k, 'v) entry
(** [entry map key] returns an entry for the given key *)

val or_insert : ('k, 'v) t -> 'k -> 'v -> 'v
(** [or_insert entry default] inserts [default] if the entry is vacant *)

val and_modify : ('k, 'v) t -> 'k -> ('v -> 'v) -> unit
(** [and_modify entry f] applies [f] to the value if the entry is occupied *)
