open Std

(** {1 Poneglyph - EAV Graph Store}
    
    A lightweight entity-attribute-value graph database for tracking
    build metadata, file relationships, and semantic code information.
*)

(** {2 Core Types} *)

type t
(** The graph store *)

module Uri = Model.Uri
(** URI construction and manipulation *)

module Fact = Model.Fact
(** Fact construction and value types *)

module Schema = Model.Schema
(** Schema definition helpers *)

module Storage = Storage
(** Storage backends (Inmemory, Simple_file, LSM) *)

module Ref_store = Storage.Lsm.Ref_store
(** Reference store - Ground truth oracle for LSM testing *)

module Cli = Cli
(** CLI command implementations *)

(** {2 Creation & Persistence} *)

val open_shared : data_dir:string -> (t, string) result
(** Open LSM database for read-only access with shared lock.
    
    - Acquires LOCK_SH - multiple processes can read concurrently
    - Blocks if writer/compactor is active
    - Safe for queries - no file deletion during scan
    - Use for: query, stats, read-only operations
    
    @param data_dir Path to database directory
    @return Database handle or error *)

val open_exclusive : data_dir:string -> ?timeout:Time.Duration.t -> unit -> (t, string) result
(** Open LSM database for read-write access with exclusive lock.
    
    - Acquires LOCK_EX - only one process can write
    - Blocks all readers and other writers
    - Use for: state, retract, compact operations
    
    @param data_dir Path to database directory
    @param timeout Lock acquisition timeout (default: 30s)
    @return Database handle or error *)

(** {3 Legacy API} *)

type create_config = Graph_store.create_config =
  | InMemory  (** In-memory HashMap storage *)
  | Persistent of string  (** File-based JSON storage *)
  | Lsm of string  (** LSM-based multi-index storage (acquires write lock) *)

val create : ?config:create_config -> unit -> t
(** Create a new graph store with specified backend.
    
    Default: LSM storage with unique temp directory for isolation.
    Provides production-grade performance and durability.
    
    For LSM storage, this acquires a write lock (equivalent to open_write).
    For tests, use this with Lsm config for convenience.
    
    Examples:
    {[
      (* LSM-based (default - high performance, unique temp dir for isolation) *)
      let graph = create () in
      
      (* LSM with custom directory (production deployment) *)
      let graph = create ~config:(Lsm "/var/lib/poneglyph") () in
      
      (* In-memory (ephemeral, no persistence) *)
      let graph = create ~config:InMemory () in
      
      (* File-based JSON (simple persistence) *)
      let graph = create ~config:(Persistent "data.json") () in
    ]}
*)

val create_persistent : string -> t
(** Legacy API: Create a graph store backed by a file.
    Equivalent to [create ~config:(Persistent path) ()]. *)

val create_lsm : string -> t
(** Create an LSM-backed graph store (acquires write lock).
    Equivalent to [create ~config:(Lsm data_dir) ()]. *)

val save : t -> unit
(** Save graph to disk (only for persistent stores).
    LSM stores are automatically persisted. *)

val load : string -> t
(** Load a graph from file. Equivalent to [create_persistent path]. *)

val close : t -> unit
(** Close the graph and flush any pending writes.
    Important for LSM stores to ensure all data is persisted. *)

val flush : t -> unit
(** Manually flush data to disk (LSM storage only).
    
    Note: [state] already flushes automatically for LSM, so this is
    rarely needed. Use it to ensure durability at specific checkpoints. *)

(** {2 Stating Facts} *)

val state : t -> Fact.t list -> int
(** State facts into the graph. Returns transaction ID.
    
    Durability guarantee:
    - For LSM storage: Data is flushed to disk before returning.
      When this function returns, the facts are guaranteed to be durable.
    - For in-memory storage: Data is only in RAM.
    - For file storage: Data is written to the file.
    
    Performance note: LSM storage flushes on every call for safety.
    For bulk imports, consider using a lower-level API (future work).
    
    {[
      let facts = Fact.for_entity file_uri [
        content_hash ~hash:"abc123";
        formatted ~value:true;
      ] in
      let tx_id = state graph facts  (* Data is now on disk for LSM *)
    ]}
*)

val retract : t -> fact_uri:Uri.t -> unit
(** Retract a fact by its URI. The fact remains in history but
    is no longer considered current. *)

(** {2 Querying} *)

val get : t -> entity:Uri.t -> attr:Uri.t -> Fact.value option
(** Get the current value of an attribute for an entity.
    
    {[
      match get graph ~entity:file_uri ~attr:hash_attr with
      | Some (Fact.String hash) -> print hash
      | _ -> ()
    ]}
*)

val exists : t -> Uri.t -> bool
(** Check if an entity has any current (non-retracted) facts *)

val get_all_facts : t -> entity:Uri.t -> Fact.t Iter.MutIterator.t
(** Get all facts about an entity, including retracted ones.
    Returns an iterator for memory-efficient traversal.
    
    {[
      (* Process facts incrementally *)
      get_all_facts graph ~entity:file_uri
      |> Iter.MutIterator.for_each ~fn:(fun fact ->
          Log.info "Fact: %s" (Fact.to_string fact)
        );
      
      (* Or convert to list if needed *)
      let facts = get_all_facts graph ~entity:file_uri
        |> Iter.MutIterator.to_list in
      List.iter process_fact facts
    ]}
*)

val get_current_facts : t -> entity:Uri.t -> Fact.t Iter.MutIterator.t
(** Get only current (non-retracted) facts about an entity.
    Returns an iterator for memory-efficient traversal.
    
    {[
      (* Count facts without loading into memory *)
      let count = get_current_facts graph ~entity:file_uri
        |> Iter.MutIterator.count in
      Log.info "Entity has %d facts" count
    ]}
*)

val get_kind : t -> Uri.t -> Uri.t option
(** Get the kind/type of an entity via @field:instance_of *)

(** {2 Transitive Queries} *)

val transitive : t -> start:Uri.t -> edge:Uri.t -> max_depth:int option -> Uri.t Iter.MutIterator.t
(** Follow edges transitively from a starting entity using lazy BFS.
    
    Returns a lazy iterator that explores the graph on-demand:
    - Only fetches facts when needed (not all upfront)
    - Respects max_depth limit during traversal
    - Supports early termination (stop iterating anytime)
    - Memory usage: O(visited set) not O(entire graph)
    
    {[
      (* Find all transitive dependencies *)
      transitive graph
        ~start:module_uri
        ~edge:depends_on_attr
        ~max_depth:(Some 10)
      |> Iter.MutIterator.take 100  (* Only explores first 100 reachable nodes *)
      |> Iter.MutIterator.to_list
      
      (* Check if path exists without exploring entire graph *)
      let has_path_to_target = 
        transitive graph ~start ~edge ~max_depth:None
        |> Iter.MutIterator.exists ~fn:(fun uri -> Uri.equal uri target)
    ]}
*)

val find_entities : t -> attr:Uri.t -> value:Fact.value -> Uri.t Iter.MutIterator.t
(** Find all entities with a specific attribute=value pair (reverse lookup).
    Returns an iterator for memory-efficient traversal.
    
    {[
      (* Find all files with a specific hash *)
      find_entities graph 
        ~attr:hash_attr 
        ~value:(Fact.String "abc123")
      |> Iter.MutIterator.for_each ~fn:(fun uri ->
          Log.info "Found: %s" (Uri.to_string uri)
        )
    ]}
*)

val find_by_kind : t -> kind:Uri.t -> Uri.t Iter.MutIterator.t
(** Find all entities of a specific kind.
    Returns an iterator for memory-efficient traversal.
    
    {[
      (* Find all file entities *)
      let file_kind = Uri.of_string "tusk:kind:file" in
      let all_files = find_by_kind graph ~kind:file_kind in
      
      (* Count files *)
      let count = all_files |> Iter.MutIterator.count in
      Log.info "Found %d files" count
    ]}
*)

(** {2 Source-Based Queries} *)

val find_by_source : t -> source:Uri.t -> Uri.t Iter.MutIterator.t
(** Find all entities with facts from a specific source.
    Returns an iterator for memory-efficient traversal.
    
    {[
      (* Find everything from a build *)
      let build_source = Uri.of_string "tusk:build:2024-11-14-001" in
      let entities = find_by_source graph ~source:build_source in
      
      (* Process incrementally *)
      entities
      |> Iter.MutIterator.for_each ~fn:(fun entity ->
          Log.info "Entity from build: %s" (Uri.to_string entity)
        )
    ]}
*)

val retract_by_source : t -> source:Uri.t -> unit
(** Retract all facts from a specific source.
    Useful for cleaning up after failed builds or bad LLM annotations.
    
    {[
      (* Remove all facts from a failed build *)
      let failed_build = Uri.of_string "tusk:build:failed-12345" in
      retract_by_source graph ~source:failed_build
    ]}
*)

(** {2 Schema Management} *)

val register_schema : t -> Schema.def list -> unit
(** Register a schema into the graph. This stores schema facts
    that describe your domain model.
    
    {[
      module MySchema = struct
        let ns = Schema.namespace "myapp"
        let user = Schema.kind ~ns "user"
        let email = Schema.field ~ns "email" 
          |> Schema.used_on user
          |> Schema.value_type Schema.Type.string
        let all_defs = [user; email]
      end
      
      register_schema graph MySchema.all_defs
    ]}
*)

val list_schemas : t -> Uri.t Iter.MutIterator.t
(** List all registered schema namespaces.
    Returns an iterator for memory-efficient traversal. *)

val bootstrap : t -> unit
(** Bootstrap the core Poneglyph schema (kinds, fields, types).
    Called automatically on create. *)

(** {2 Entity Helpers} *)

module Entity = Model.Entity
(** Entity record type and utilities *)

val load_entity : t -> Uri.t -> Entity.t option
(** Load a complete entity with all its current facts.
    
    {[
      match load_entity graph file_uri with
      | Some entity -> 
          Log.info "%s" (Entity.to_string entity)
      | None -> 
          Log.warn "Entity not found"
    ]}
*)

(** {2 Utilities} *)

val stats : t -> (string * int) list
(** Get statistics about the graph:
    - "entities": Number of entities with facts
    - "facts": Total number of facts
    - "current_facts": Number of non-retracted facts
*)

(** {2 Datalog Integration}

    Phase 0: Query-Only Datalog
    
    Execute Datalog queries programmatically or via CLI.
**)

val execute_query : t -> query:string -> (Datalog.Substitution.t Iter.MutIterator.t, string) result
(** Execute a Datalog query and return streaming results.
    
    Supports both single-goal and multi-goal queries:
    
    {[
      (* Single goal *)
      match execute_query graph ~query:"'codedb:package'(M, \"std\")" with
      | Ok results -> 
          results |> Iter.MutIterator.for_each ~fn:(fun subst ->
            println (Data.Json.to_string (Datalog.Substitution.to_json subst))
          )
      | Error e -> println ("Query error: " ^ e)
      
      (* Multi-goal join *)
      execute_query graph 
        ~query:"'ocaml:canonical_name'(M, \"Std__List\"), 'codedb:provided_by'(M, F)"
    ]}
    
    @param query Datalog query string (single or multi-goal)
    @return Iterator of variable substitutions or error
*)

val fact :
  source:Uri.t ->
  entity:Uri.t ->
  attribute:Uri.t ->
  value:Fact.value ->
  stated_at:Datetime.t ->
  tx_id:UUID.t ->
  Fact.t

val facts :
  source:Uri.t ->
  tx_id:UUID.t ->
  stated_at:Datetime.t ->
  entity:Uri.t ->
  (Uri.t * Fact.value) list -> 
  Fact.t list
