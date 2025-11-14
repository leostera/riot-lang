(** LSM Engine - Orchestrates all LSM components into a storage system.

    The engine manages:
    - Write path: Memtable + WAL for durability
    - Read path: Memtable + SSTables for queries  
    - Flush: Memtable → SSTable when full
    - Compaction: Merge SSTables to reduce read amplification
    - Recovery: Replay WAL after crashes
*)

open Std

(** Configuration for the LSM engine *)
type config = {
  data_dir : string;  (** Directory for storing SSTables and WAL *)
  max_memtable_size : int;  (** Max memtable size in bytes before flush (default: 4MB) *)
  compaction_threshold : int;  (** Number of SSTables before compaction (default: 4) *)
}

(** The LSM engine handle *)
type t

(** Engine statistics *)
type stats = {
  memtable_size : int;  (** Current memtable size in bytes *)
  sstable_count : int;  (** Number of SSTables on disk *)
}

(** {1 Lifecycle} *)

(** [open_engine config] opens or creates an LSM engine.
    
    - Creates data directory if it doesn't exist
    - Opens or creates WAL
    - Discovers existing SSTables
    - Replays WAL for crash recovery
    
    @param config Engine configuration
    @return Engine handle or error
    
    Example:
    {[
      let config = {
        data_dir = "/tmp/lsm_data";
        max_memtable_size = 4 * 1024 * 1024;  (* 4MB *)
        compaction_threshold = 4;
      } in
      match Engine.open_engine config with
      | Ok engine -> (* use engine *)
      | Error err -> (* handle error *)
    ]}
*)
val open_engine : config -> (t, string) result

(** [close engine] closes the engine gracefully.
    
    - Flushes memtable to SSTable
    - Closes WAL
    - Closes all SSTable readers
    
    After calling close, the engine handle is invalid.
*)
val close : t -> (unit, string) result

(** {1 Core Operations} *)

(** [put engine ~key ~value] inserts or updates a key-value pair.
    
    Write path:
    1. Append to WAL (durability)
    2. Write to memtable (performance)
    3. Auto-flush if memtable exceeds max size
    
    @param key 41-byte encoded key
    @param value Encoded value bytes
    @return Ok () on success, Error on failure
*)
val put : t -> key:bytes -> value:bytes -> (unit, string) result

(** [get engine ~key] retrieves a value by key.
    
    Read path:
    1. Check memtable first (most recent)
    2. Check SSTables in reverse order (newest first)
    
    Returns None if key not found or has been deleted (tombstone).
    
    @param key 41-byte encoded key
    @return Some value if found, None otherwise
*)
val get : t -> key:bytes -> bytes option

(** [delete engine ~key] deletes a key by writing a tombstone.
    
    Tombstones are special markers that indicate deletion.
    They persist until compaction removes them.
    
    @param key 41-byte encoded key
    @return Ok () on success, Error on failure
*)
val delete : t -> key:bytes -> (unit, string) result

(** {1 Maintenance} *)

(** [flush engine] manually flushes the memtable to disk.
    
    Flush process:
    1. Write memtable to new SSTable
    2. Truncate WAL
    3. Clear memtable
    4. Add SSTable to engine's list
    
    Called automatically when memtable is full, but can be called manually
    for checkpointing or before shutdown.
*)
val flush : t -> (unit, string) result

(** [compact engine] manually compacts SSTables.
    
    If the number of SSTables >= compaction_threshold:
    1. Selects oldest N SSTables
    2. Merges them into one new SSTable
    3. Deletes old SSTables
    4. Updates engine's SSTable list
    
    Does nothing if compaction threshold not met.
*)
val compact : t -> (unit, string) result

(** [needs_compaction engine] checks if compaction is recommended.
    
    Returns true if SSTable count >= compaction threshold.
*)
val needs_compaction : t -> bool

(** {1 Statistics} *)

(** [stats engine] returns current engine statistics.
    
    Useful for monitoring and deciding when to compact/flush.
*)
val stats : t -> stats
