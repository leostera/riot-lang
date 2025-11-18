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

(** [put_batch engine ~entries] inserts or updates multiple key-value pairs atomically.
    
    Performance optimization: MUCH faster than calling put repeatedly.
    Instead of n × (WAL write + memtable sort), does:
    - 1 × batch WAL write (single fsync)
    - 1 × batch memtable add (single sort)
    
    Expected speedup: 50-70% for large batches (thousands of entries).
    
    Write path:
    1. Batch append to WAL (durability)
    2. Batch write to memtable (performance)
    3. Auto-flush if memtable exceeds max size
    
    @param entries List of (key, value) pairs to insert
    @return Ok () on success, Error on failure
*)
val put_batch : t -> entries:(bytes * bytes) list -> (unit, string) result

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

(** [write_batch engine ops] atomically writes multiple operations.
    
    All operations are written to WAL in a single atomic batch, then
    applied to the memtable. Either all operations succeed or none do.
    
    Operations can be:
    - [`Put (key, value)] - Insert/update
    - [`Delete key] - Delete (tombstone)
    
    Batch write path:
    1. Append all to WAL atomically (single fsync)
    2. Apply all to memtable
    3. Auto-flush if memtable exceeds max size
    
    @param ops List of operations to perform
    @return Ok () on success, Error on failure
    
    Example:
    {[
      let ops = [
        `Put (key1, value1);
        `Put (key2, value2);
        `Delete key3;
      ] in
      match Engine.write_batch engine ops with
      | Ok () -> (* all succeeded *)
      | Error e -> (* all rolled back *)
    ]}
*)
val write_batch : t -> [> `Put of bytes * bytes | `Delete of bytes ] list -> (unit, string) result

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

(** [compact_one_tier engine ~tier ~threshold] compacts one tier if it exceeds threshold.
    
    Tier-based compaction strategy:
    1. Group SSTables by size tier (tier 0 = <1MB, tier 1 = 1-8MB, etc.)
    2. If a tier has >= threshold SSTables:
       - Select 4 oldest SSTables from that tier
       - Merge them into one larger SSTable (promoted to tier+1)
       - Update manifest and delete old SSTables
    3. Returns Ok true if compaction performed, Ok false if not needed
    
    This is the core primitive for CLI-triggered compaction.
    The CLI decides WHEN to compact (policy), this implements HOW (mechanism).
    
    @param tier Which tier to check (0 = tiny, 1 = small, etc.)
    @param threshold Min number of SSTables to trigger compaction
    @param max_merge Maximum number of SSTables to merge in one batch (default: 4)
    @return Ok true if compacted, Ok false if not needed, Error on failure
*)
val compact_one_tier : t -> tier:int -> threshold:int -> ?max_merge:int -> unit -> (bool, string) result

(** {1 Statistics} *)

(** [stats engine] returns current engine statistics.
    
    Useful for monitoring and deciding when to compact/flush.
*)
val stats : t -> stats

(** {1 Queries} *)

(** [scan_prefix engine prefix] scans all keys with given prefix.
    
    Returns an iterator over key-value pairs where the key starts with the given prefix bytes.
    Results are in sorted order by key.
    
    Query path:
    1. Scan memtable (newest data)
    2. Scan SSTables newest to oldest
    3. Merge results with seen set (memtable > newer > older)
    4. Filter tombstones (empty values)
    
    Correctness:
    - Memtable values override SSTable values (newer wins)
    - Newer SSTable values override older SSTable values
    - Tombstones (empty values) are filtered out
    
    Performance:
    - O(n) where n = total keys with prefix across all levels
    - Can be optimized with bloom filters and block-level filtering
    
    Iterator benefits:
    - Early termination: `scan_prefix engine ~prefix |> Iter.take 10`
    - Composition: `scan_prefix engine ~prefix |> Iter.filter pred |> Iter.map f`
    - Memory control: Process results incrementally (bounded by single SSTable)
    - Flexible consumption: `|> Iter.to_list` if you need a list
    
    Fully lazy implementation:
    - Memtable scan: Truly lazy (streams from Vector, filters on-the-fly)
    - SSTable scan: Truly lazy (opens SSTables on-demand as iterator consumed)
    - Early termination: Stops opening SSTables when iteration stops
    - Memory: O(single SSTable scan) not O(all SSTables)
    
    Resource management:
    - SSTables are opened on-demand and closed when exhausted
    - If iterator is dropped early, current SSTable reader may remain open
      until the iterator is garbage collected (minor resource leak in edge case)
    - For long-running processes, consume or explicitly drop iterators
    
    Performance characteristics:
    - Best case: O(memtable scan) if all results in memtable
    - Worst case: O(memtable + N SSTables) if need to scan all
    - Early termination: Only scans SSTables until result count reached
    
    @param prefix Prefix bytes to match
    @return Iterator over (key, value) pairs in sorted order
    
    Example:
    {[
      (* Get all results as list *)
      let all_results = Engine.scan_prefix engine ~prefix |> Iter.to_list in
      
      (* Get first 10 matching keys *)
      let first_10 = Engine.scan_prefix engine ~prefix
        |> Iter.take 10
        |> Iter.to_list in
      
      (* Find first key matching a condition *)
      let found = Engine.scan_prefix engine ~prefix
        |> Iter.find (fun (key, _value) -> some_condition key) in
      
      (* Process without loading all into memory *)
      Engine.scan_prefix engine ~prefix
        |> Iter.iter (fun (key, value) -> process key value)
    ]}
*)
val scan_prefix : t -> prefix:bytes -> (bytes * bytes) Iter.MutIterator.t

val get_manifest : t -> Manifest.t
(** Get current manifest *)
