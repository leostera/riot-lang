open Std
open Model

type t
(** High-level graph wrapper over storage backends *)

(** {1 Creating Graphs} *)

val open_shared : data_dir:string -> (t, string) result
(** Open an LSM graph store for read-only access with shared lock.
    
    - Acquires LOCK_SH on {data_dir}/LOCK
    - Multiple processes can open_shared concurrently
    - Blocks if any process holds exclusive lock (writer/compactor active)
    - Safe for queries - files won't be deleted during scan
    - Lock held until close()
    
    Used by: poneglyph query, get, stats
    
    @param data_dir Directory containing the LSM data
    @return Ok graph handle, or Error if lock acquisition fails *)

val open_exclusive : data_dir:string -> ?timeout:Time.Duration.t -> unit -> (t, string) result
(** Open an LSM graph store for read-write access with exclusive lock.
    
    - Acquires LOCK_EX on {data_dir}/LOCK
    - Only one process can hold exclusive lock
    - Blocks all other opens (shared or exclusive)
    - Allows writes and compaction
    - Lock held until close()
    
    Used by: poneglyph state, compact, new
    
    @param data_dir Directory containing the LSM data
    @param timeout Max time to wait for lock (default: 30s, negative = wait forever)
    @return Ok graph handle, or Error if lock acquisition fails *)

(** {1 Legacy API} *)

type create_config =
  | InMemory  (** In-memory HashMap storage *)
  | Persistent of string  (** File-based JSON storage *)
  | Lsm of string  (** LSM-based multi-index storage (acquires lock) *)

val create : ?config:create_config -> unit -> t
(** Create a new graph with specified storage backend.
    Defaults to LSM with unique temp directory if no config provided.
    
    For LSM storage, this acquires a write lock (equivalent to open_write).
    For tests, use this with Lsm config for convenience. *)

val close : t -> unit
(** Close the graph and flush any pending writes (for LSM storage) *)

val cleanup_orphaned_files : t -> unit
(** Delete SST files on disk that are not tracked in the manifest.
    
    This removes orphaned files left behind from incomplete compactions.
    Only works for LSM storage backend. Safe to call - only deletes files
    not referenced in the manifest. *)

val flush : t -> unit
(** Manually flush data to disk (for LSM storage).
    
    Note: The high-level [state] function already flushes automatically
    for LSM storage, so this is rarely needed. Useful for ensuring
    durability at specific checkpoints without closing the graph. *)

val compact_if_needed : t -> threshold:int -> unit
(** Trigger compaction if tier 0 exceeds threshold (LSM storage only).
    
    This is a CLI-specific helper for triggering compaction after writes.
    The CLI decides WHEN to compact (policy), the LSM library does HOW (mechanism).
    
    For LSM storage:
    - Checks if any index has tier 0 with >= threshold SSTables
    - If so, merges oldest 4 SSTables into one larger SSTable
    - Updates manifests and deletes old files
    - Logs results
    
    For other storage backends, this is a no-op.
    
    @param threshold Min number of SSTables in tier 0 to trigger compaction *)

val compact_tier : t -> tier:int -> threshold:int -> ?max_merge:int -> unit -> (bool, string) result
(** Explicit compaction for a specific tier (LSM storage only).
    
    This is used by the `poneglyph compact` command for manual compaction.
    
    For LSM storage:
    - Checks if the specified tier has >= threshold SSTables
    - If so, merges oldest N SSTables with size-based batching (promoted to tier+1)
    - Size-based batching: <10KB files: up to 50, <100KB: up to 20, else: up to 10
    - Updates manifests and deletes old files
    - Returns Ok true if compaction performed, Ok false if not needed
    
    For other storage backends, returns Error.
    
    @param tier Which tier to compact (0 = <1MB, 1 = 1-8MB, etc.)
    @param threshold Min number of SSTables to trigger compaction
    @param max_merge Maximum files to merge in one batch (default: 4)
    @return Ok true if compacted, Ok false if not needed, Error if not LSM storage *)

val state : t -> Fact.t list -> int
(** State facts into the graph, returns transaction ID *)

val retract : t -> fact_uri:Uri.t -> unit
(** Retract a fact *)

val get : t -> entity:Uri.t -> attr:Uri.t -> Fact.value option
(** Get current value of entity attribute *)

val get_all_facts : t -> entity:Uri.t -> Fact.t Iter.MutIterator.t
(** Get all facts (including retracted) about entity.
    Returns an iterator for memory-efficient traversal. *)

val get_current_facts : t -> entity:Uri.t -> Fact.t Iter.MutIterator.t
(** Get only current facts about entity.
    Returns an iterator for memory-efficient traversal. *)

val exists : t -> Uri.t -> bool
(** Check if entity has any current facts *)

val get_kind : t -> Uri.t -> Uri.t option
(** Get entity's kind *)

val list_schemas : t -> Uri.t Iter.MutIterator.t
(** List registered schemas.
    Returns an iterator for memory-efficient traversal. *)

val save : t -> unit
(** Save to disk (for persistent graphs) *)

val transitive : t -> start:Uri.t -> edge:Uri.t -> max_depth:int option -> Uri.t Iter.MutIterator.t
(** Follow edges transitively from starting entity.
    Returns an iterator for memory-efficient traversal.
    NOTE: This is currently list-based BFS. Will be replaced with lazy iterator in Phase 6b. *)

val count_entities : t -> int
(** Count entities with current facts *)

val count_facts : t -> int
(** Count total facts (including retracted) *)

val count_current_facts : t -> int
(** Count non-retracted facts *)

val find_entities : t -> attr:Uri.t -> value:Fact.value -> Uri.t Iter.MutIterator.t
(** Find all entities with specific attribute=value pair (reverse lookup).
    Returns an iterator for memory-efficient traversal. *)

val find_by_kind : t -> kind:Uri.t -> Uri.t Iter.MutIterator.t
(** Find all entities of a specific kind.
    Returns an iterator for memory-efficient traversal. *)

val get_facts_by_attribute : t -> attribute:Uri.t -> Fact.t Iter.MutIterator.t
(** Get all facts for a specific attribute - optimized for LSM storage.
    For LSM storage, uses AVET index for fast lookup.
    For other storage types, filters all facts.
    Much faster than get_all_current_facts when querying by attribute. *)

val get_all_current_facts : t -> Fact.t Iter.MutIterator.t
(** Get all current (non-retracted) facts from the graph.
    Returns an iterator for memory-efficient streaming.
    Critical for large datasets - avoids loading millions of facts into memory.
    Used by Datalog integration. *)

val find_by_source : t -> source:Uri.t -> Uri.t Iter.MutIterator.t
(** Find all entities with facts from a specific source.
    Returns an iterator for memory-efficient traversal. *)

val retract_by_source : t -> source:Uri.t -> unit
(** Retract all facts from a specific source *)

val get_detailed_stats : t -> Data.Json.t
(** Get detailed statistics about the database in JSON format.
    For LSM stores, includes per-tier SSTable information.
    For other stores, returns basic backend info. *)
