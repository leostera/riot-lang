(** SkipList - Probabilistic balanced search structure
    
    A skip list maintains sorted order automatically without explicit sorting.
    It provides O(log n) expected time for all operations.
    
    Advantages over Vector + sort:
    - No sorting needed (always maintains sorted order)
    - O(log n) insert vs O(n log n) sort
    - Better for incremental updates
    - Lock-free friendly
    
    Used by: Redis, LevelDB, RocksDB for memtables
*)

open Std

(** The type of a skip list *)
type t

(** Create a new empty skip list *)
val create : unit -> t

(** Insert a key-value pair
    
    If the key already exists, updates its value.
    Keys are automatically maintained in sorted order.
    
    @param t The skip list
    @param key 41-byte key
    @param value Value bytes
    @return Ok true if inserted (new key), Ok false if updated (existing key), Error on invalid key
*)
val insert : t -> key:bytes -> value:bytes -> (bool, string) result

(** Find a value by key
    
    O(log n) expected time.
    
    @param t The skip list
    @param key 41-byte key to search for
    @return Some value if found, None otherwise
*)
val find : t -> key:bytes -> bytes option

(** Get current size in bytes *)
val size_bytes : t -> int

(** Get number of entries *)
val count : t -> int

(** Iterate over all entries in sorted key order
    
    @param t The skip list
    @param f Function to call for each (key, value) pair
*)
val iter : t -> f:(key:bytes -> value:bytes -> unit) -> unit

(** Fold over all entries in sorted key order
    
    @param t The skip list
    @param init Initial accumulator value
    @param f Folding function
*)
val fold : t -> init:'acc -> f:(acc:'acc -> key:bytes -> value:bytes -> 'acc) -> 'acc

(** Clear all entries *)
val clear : t -> unit
