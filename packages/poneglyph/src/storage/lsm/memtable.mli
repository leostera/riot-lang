(** Memtable - In-memory sorted write buffer for LSM storage
    
    A memtable is an in-memory data structure that accepts writes in any order,
    maintains them in sorted order, and provides fast queries. When full, it
    flushes to an SSTable on disk.
    
    Design:
    - Sorted vector with binary search (O(log n) queries)
    - Sort after each insert (O(n log n) writes)
    - Configurable max size (typically 1-4MB)
    - Last-write-wins semantics for duplicate keys
    
    Usage:
    {[
      let mt = Memtable.create ~max_size:1_000_000 in  (* 1MB *)
      
      (* Add entries *)
      let _ = Memtable.add mt ~key ~value in
      
      (* Query *)
      match Memtable.get mt ~key with
      | Some value -> ...
      | None -> ...
      
      (* Flush when full *)
      if Memtable.is_full mt then
        let _ = Memtable.flush_to_sstable mt ~path:"data.sst" in
        Memtable.clear mt
    ]}
*)

open Std

(** The type of a memtable *)
type t

(** Create a new empty memtable
    
    @param max_size Maximum size in bytes before memtable should be flushed
*)
val create : max_size:int -> t

(** Add a key-value pair to the memtable
    
    If the key already exists, its value is overwritten (last write wins).
    The memtable automatically maintains sorted order.
    
    Returns Error if:
    - Key is not exactly 41 bytes
    - Adding would exceed max_size
    
    @param t The memtable
    @param key The 41-byte index key
    @param value The fact data as bytes
*)
val add : t -> key:bytes -> value:bytes -> (unit, string) result

(** Query for a value by key
    
    Uses binary search for O(log n) lookup.
    
    @param t The memtable
    @param key The 41-byte key to search for
    @return Some value if found, None otherwise
*)
val get : t -> key:bytes -> bytes option

(** Check if memtable should be flushed
    
    Returns true if current size >= max_size
*)
val is_full : t -> bool

(** Get current size in bytes
    
    This is the sum of all key and value sizes.
*)
val size_bytes : t -> int

(** Get number of entries in the memtable *)
val count : t -> int

(** Iterate over all entries in sorted key order
    
    @param t The memtable
    @param f Function to call for each key-value pair
*)
val iter : t -> f:(key:bytes -> value:bytes -> unit) -> unit

(** Fold over all entries in sorted key order
    
    @param t The memtable
    @param init Initial accumulator value
    @param f Folding function
*)
val fold : t -> init:'acc -> f:(acc:'acc -> key:bytes -> value:bytes -> 'acc) -> 'acc

(** Flush memtable to an SSTable file
    
    Writes all entries in sorted order to an SSTable.
    Does NOT clear the memtable afterward (call clear explicitly).
    
    Returns the number of entries written, or Error on failure.
    
    @param t The memtable
    @param path File path for the new SSTable
*)
val flush_to_sstable : t -> path:string -> (int, string) result

(** Clear all entries from the memtable
    
    Resets size to 0 and removes all entries.
    Typically called after successful flush.
*)
val clear : t -> unit
