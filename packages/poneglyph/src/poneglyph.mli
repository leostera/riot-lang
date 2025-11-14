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

(** {2 Creation & Persistence} *)

val create : unit -> t
(** Create a new in-memory graph store *)

val create_persistent : string -> t
(** Create a graph store backed by a file.
    If file exists, loads it; otherwise creates new. *)

val save : t -> unit
(** Save graph to disk (only for persistent stores) *)

val load : string -> t
(** Load a graph from file *)

(** {2 Stating Facts} *)

val state : t -> Fact.t list -> int
(** State facts into the graph. Returns transaction ID.
    
    {[
      let facts = Fact.for_entity file_uri [
        content_hash ~hash:"abc123";
        formatted ~value:true;
      ] in
      let tx_id = state graph facts
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

val get_all_facts : t -> entity:Uri.t -> Fact.t list
(** Get all facts about an entity, including retracted ones *)

val get_current_facts : t -> entity:Uri.t -> Fact.t list
(** Get only current (non-retracted) facts about an entity *)

val get_kind : t -> Uri.t -> Uri.t option
(** Get the kind/type of an entity via @field:instance_of *)

(** {2 Transitive Queries} *)

val transitive : t -> start:Uri.t -> edge:Uri.t -> max_depth:int option -> Uri.t list
(** Follow edges transitively from a starting entity.
    
    {[
      (* Find all transitive dependencies *)
      transitive graph
        ~start:module_uri
        ~edge:depends_on_attr
        ~max_depth:(Some 10)
    ]}
*)

val find_entities : t -> attr:Uri.t -> value:Fact.value -> Uri.t list
(** Find all entities with a specific attribute=value pair (reverse lookup).
    
    {[
      (* Find all files with a specific hash *)
      find_entities graph 
        ~attr:hash_attr 
        ~value:(Fact.String "abc123")
    ]}
*)

val find_by_kind : t -> kind:Uri.t -> Uri.t list
(** Find all entities of a specific kind.
    
    {[
      (* Find all file entities *)
      let file_kind = Uri.of_string "tusk:kind:file" in
      let all_files = find_by_kind graph ~kind:file_kind
    ]}
*)

(** {2 Source-Based Queries} *)

val find_by_source : t -> source:Uri.t -> Uri.t list
(** Find all entities with facts from a specific source.
    
    {[
      (* Find everything from a build *)
      let build_source = Uri.of_string "tusk:build:2024-11-14-001" in
      let entities = find_by_source graph ~source:build_source
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

val list_schemas : t -> Uri.t list
(** List all registered schema namespaces *)

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

    Poneglyph implements Datalog's pluggable storage interface, allowing
    Datalog queries to run directly on Poneglyph graphs without copying facts.
    
    {b Note}: Full Datalog query evaluation requires the Datalog evaluation
    engine (coming in Week 2). Currently, you can access the storage layer
    and see which predicates are available.
    
    {3 Design}
    
    Each Poneglyph attribute becomes a binary Datalog predicate:
    
    {[
      (* Poneglyph fact *)
      Fact { entity: "module:A", attribute: "depends_on", value: Uri "module:B" }
      
      (* Becomes Datalog predicate *)
      depends_on("module:A", "module:B")
    ]}
    
    {3 Example (When Datalog Runtime Ready)}
    
    {[
      (* Query transitive dependencies *)
      let results = Poneglyph.Datalog.query graph
        ~rules:[
          "path(X, Y) :- depends_on(X, Y).";
          "path(X, Z) :- depends_on(X, Y), path(Y, Z).";
        ]
        ~query:"path('module:A', X)"
    ]}
*)

module Datalog : sig
  val predicates : t -> string list
  (** List all available predicates (attributes) in the graph.
      
      Example:
      {[
        let preds = Datalog.predicates graph
        (* Returns: ["depends_on"; "formatted"; "hash"; ...] *)
      ]}
  *)
  
  val get_facts : t -> predicate:string -> Datalog.Storage.fact_tuple Datalog.Relation.t
  (** Get all facts for a predicate as a Datalog relation.
      
      Example:
      {[
        let facts = Datalog.get_facts graph ~predicate:"depends_on"
        (* Returns relation of tuples: [["module:A", "module:B"]; ...] *)
      ]}
  *)
  
  val test_storage : t -> unit
  (** Test the Datalog storage backend. Prints available predicates and fact counts.
      Useful for debugging the integration. *)
end
