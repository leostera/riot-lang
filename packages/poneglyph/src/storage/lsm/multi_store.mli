(** Multi-Index LSM Store - Atomic writes across 4 indices
    
    This module implements a multi-index LSM storage layer with:
    - EAVT index: Query all attributes of an entity
    - AVET index: Find entities by attribute-value (reverse lookup)
    - FACT index: Lookup fact metadata by fact_uri
    - SOURCE index: Query facts by source_uri
    
    All writes are atomic across all 4 indices using a global WAL.
*)

open Std
open Model

type t
(** Multi-index store handle *)

(** {1 Lifecycle} *)

val create : data_dir:string -> (t, string) result
(** Create or open a multi-index store.
    
    Creates:
    - Main data directory
    - Global WAL (global.wal) for atomic multi-index writes
    - 4 subdirectories with LSM engines:
      - eavt/ - Entity-Attribute-Value-Transaction index
      - avet/ - Attribute-Value-Entity-Transaction index
      - fact/ - Fact metadata index
      - source/ - Source metadata index
    
    @param data_dir Root directory for all storage
    @return Store handle or error *)

val close : t -> (unit, string) result
(** Close all engines and the global WAL.
    
    Flushes all memtables to SSTables before closing. *)

val flush_all : t -> (unit, string) result
(** Manually flush all engines' memtables to SSTables.
    
    Forces all in-memory data to be written to disk across all 4 indices.
    Called automatically by close, but can be called manually to ensure
    durability at specific points (e.g., after important writes).
    
    @return Ok () on success, Error on flush failure *)

(** {1 Operations} *)

val state : t -> Fact.t list -> (int, string) result
(** Atomically write facts to all 4 indices.
    
    Write path:
    1. Generate UUIDv7 transaction ID (monotonic, time-ordered)
    2. Convert URIs to int64 IDs using SHA-256 hashing
    3. Build tagged batch for EAVT, AVET, FACT, SOURCE indices
    4. Write to global WAL atomically (single fsync for all 4 indices)
    5. Route entries to individual engine memtables
    6. Auto-flush if any memtable exceeds threshold
    
    @param facts List of facts to write
    @return Number of facts written or error
    
    Example:
    {[
      let facts = [
        { entity = alice_uri;
          attribute = name_attr;
          value = String "Alice";
          ... };
      ] in
      match Multi_store.state store facts with
      | Ok count -> (* All facts written atomically *)
      | Error e -> (* All rolled back *)
    ]} *)

val retract : t -> Fact.t list -> (int, string) result
(** Retract facts - mark them as deleted without removing them.
    
    Retraction in Datomic-style databases is an append-only soft delete:
    - The original fact remains in the database (for time-travel queries)
    - A new version of the fact is written with retracted=true
    - Queries filter out retracted facts by default
    
    This allows:
    - Full history: You can query what was true at any point in time
    - Auditability: Deletions are tracked with who/when/why
    - Undo: Retractions can be un-retracted
    
    @param facts List of facts to retract
    @return Number of facts retracted or error
    
    Example:
    {[
      (* Retract Alice's age *)
      let fact_to_retract = {
        entity = alice_uri;
        attribute = age_attr;
        value = Int 30;
        ...
      } in
      match Multi_store.retract store [fact_to_retract] with
      | Ok count -> (* Fact marked as retracted *)
      | Error e -> (* Retraction failed *)
    ]} *)

(** {1 Queries} *)

val get_entity_facts : t -> entity:Uri.t -> Fact.t Iter.MutIterator.t
(** Get all facts for a given entity.
    
    Scans the EAVT index for all facts where entity matches,
    then looks up full fact data from the FACT index.
    
    Returns an iterator over facts in sorted order by (attribute, value, tx_id).
    Only returns the latest version of each (attribute, value) pair.
    Retracted facts are filtered out.
    
    @param entity The entity URI to query
    @return Iterator over current facts for this entity
    
    Example:
    {[
      let facts = Multi_store.get_entity_facts store ~entity:user_uri in
      
      (* Process incrementally *)
      facts
      |> Iter.MutIterator.for_each ~fn:(fun fact ->
          Log.info "Fact: %s" (Fact.to_string fact)
        );
      
      (* Or convert to list if needed *)
      let fact_list = Multi_store.get_entity_facts store ~entity:user_uri
        |> Iter.MutIterator.to_list
    ]} *)

val find_entities_by_attr_value : t -> attribute:Uri.t -> value:Fact.value -> Uri.t Iter.MutIterator.t
(** Find all entities that have a specific attribute-value pair.
    
    Scans the AVET index for all facts where attribute and value match,
    then extracts unique entity URIs.
    
    This is the reverse lookup of get_entity_facts - instead of
    "what are Alice's attributes?", this answers "who has name='Alice'?".
    
    Returns an iterator over unique entity URIs (deduplicated, only latest versions).
    Retracted facts are filtered out.
    
    @param attribute The attribute URI to search for
    @param value The value to match
    @return Iterator over unique entity URIs that have this attribute-value pair
    
    Example:
    {[
      (* Find all people named "Alice" *)
      let entities = Multi_store.find_entities_by_attr_value store
        ~attribute:(Uri.of_string "@field:name")
        ~value:(Fact.String "Alice") in
      
      (* Count results without loading into memory *)
      let count = entities |> Iter.MutIterator.count in
      
      (* Or take first 10 *)
      entities
      |> Iter.MutIterator.take 10
      |> Iter.MutIterator.to_list
    ]} *)

val get_facts_by_attribute : t -> attribute:Uri.t -> Fact.t Iter.MutIterator.t
(** Get all facts for a specific attribute using AVET index - optimized query!
    
    This is much faster than get_all_current_facts when you only need facts
    for a specific attribute (e.g., all "language" facts, all "depends_on" facts).
    
    Scans the AVET index for all facts where attribute matches,
    then looks up full fact data from the FACT index.
    
    Returns an iterator over facts in sorted order by (value, entity, tx_id).
    Only returns the latest version of each (entity, value) pair.
    Retracted facts are filtered out.
    
    @param attribute The attribute URI to query
    @return Iterator over current facts with this attribute
    
    Example:
    {[
      (* Get all "language" facts efficiently *)
      let language_facts = Multi_store.get_facts_by_attribute store
        ~attribute:(Uri.of_string "@field:language") in
      
      (* Process incrementally *)
      language_facts
      |> Iter.MutIterator.for_each ~fn:(fun fact ->
          match fact.value with
          | Fact.String lang -> Log.info "Entity %s has language %s" 
              (Uri.to_string fact.entity) lang
          | _ -> ()
        );
    ]} *)

val get_all_current_facts : t -> Fact.t Iter.MutIterator.t
(** Get ALL current (non-retracted) facts in the database.
    
    WARNING: This is an expensive operation that scans the entire EAVT index!
    Only use this when you need to process all facts (e.g., for Datalog queries
    or full database exports).
    
    Scans the entire EAVT index, looks up each fact in the FACT index,
    deduplicates to keep only the latest version of each (entity, attribute, value),
    and filters out retracted facts.
    
    @return Iterator over all current facts in the database
    
    Example:
    {[
      (* Get all facts for Datalog querying *)
      let all_facts = Multi_store.get_all_current_facts store in
      
      (* Process in batches *)
      all_facts
      |> Iter.MutIterator.chunks 1000
      |> Iter.MutIterator.for_each ~fn:(fun batch ->
          process_batch batch
        )
    ]} *)

val get_facts_by_source : t -> source:Uri.t -> Fact.t Iter.MutIterator.t
(** Get all facts from a specific source using SOURCE index - optimized query!
    
    This is much faster than filtering get_all_current_facts when you only need facts
    from a specific source (e.g., all facts from a specific file).
    
    Scans the SOURCE index for all facts where source matches,
    then looks up full fact data from the FACT index.
    
    Returns an iterator over facts in sorted order by (entity, attribute, tx_id).
    Only returns the latest version of each (entity, attribute, value).
    Retracted facts are filtered out.
    
    @param source The source URI to query
    @return Iterator over current facts from this source
    
    Example:
    {[
      (* Get all facts from a specific file *)
      let file_facts = Multi_store.get_facts_by_source store
        ~source:(Uri.of_string "file:///path/to/file.ml") in
      
      (* Process incrementally *)
      file_facts
      |> Iter.MutIterator.for_each ~fn:(fun fact ->
          Log.info "Fact: %s" (Fact.to_string fact)
        );
    ]} *)

(** {1 Compaction} *)

val compact_tier : t -> tier:int -> threshold:int -> ?max_merge:int -> unit -> (bool, string) result
(** Compact one tier across all 4 indices.
    
    Runs tier-based compaction on all engines (EAVT, AVET, FACT, SOURCE):
    1. Check each index for SSTables in the specified tier
    2. If tier has >= threshold SSTables, merge oldest N into one (size-based batching)
    3. Update manifests and delete old SSTables
    
    This is the primitive called by CLI-triggered compaction.
    
    @param tier Which tier to check (0 = tiny <1MB, 1 = small 1-8MB, etc.)
    @param threshold Min number of SSTables to trigger compaction
    @param max_merge Maximum files to merge in one batch (default: 4, uses size-based batching)
    @return Ok true if any index was compacted, Ok false if none needed it, Error on failure
    
    Example:
    {[
      (* Called after each CLI write *)
      match Multi_store.compact_tier store ~tier:0 ~threshold:2 ~max_merge:50 () with
      | Ok true -> Log.info "Compacted tier 0"
      | Ok false -> ()  (* Nothing to do *)
      | Error e -> Log.warn "Compaction failed: %s" e
    ]} *)

val needs_compaction : t -> threshold:int -> bool
(** Check if any index needs compaction.
    
    Returns true if any of the 4 indices has a tier with >= threshold SSTables.
    This is used by the CLI to decide whether to trigger compaction.
    
    @param threshold Min number of SSTables in a tier to trigger compaction
    @return true if compaction is needed
    
    Example:
    {[
      (* CLI compaction policy *)
      if Multi_store.needs_compaction store ~threshold:8 then
        Multi_store.compact_tier store ~tier:0 ~threshold:8
        |> ignore
    ]} *)

val get_stats : t -> Data.Json.t
(** Get detailed statistics about the database including per-tier SSTable info *)

val cleanup_orphaned_files : t -> unit
(** Delete SST files on disk that are not tracked in the manifest.
    
    Scans all index directories (eavt, avet, fact, source) and removes
    any .sst files that are not referenced in the manifest. This cleans up
    orphaned files left behind from incomplete compactions or crashes.
    
    Safe to call - only deletes files not in the manifest. *)
