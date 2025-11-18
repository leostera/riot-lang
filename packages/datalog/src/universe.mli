open Std

(** {1 Universe - Query-Only Datalog Snapshot}
    
    Phase 0: Query-Only Datalog Core
    
    A Universe represents a read-only snapshot of facts at a specific
    transaction ID. It's a thin wrapper that:
    - Holds a storage backend reference
    - Tracks the snapshot tx_id for isolation
    - Delegates all queries to storage
    
    {2 Key Design}
    
    - No derived facts (use projection jobs instead)
    - No rules (query-only, not evaluation)
    - Snapshot isolation via tx_id
    - Pure streaming delegation to storage
    
    {2 Example}
    
    {[
      (* Create universe at latest transaction *)
      module U = Universe.Make(PoneglyphStorage)
      
      let storage = Graph_store.open_shared ~data_dir:"db" in
      let latest_tx = Graph_store.get_latest_tx_id storage in
      let universe = U.create storage ~tx_id:latest_tx in
      
      (* Query at this snapshot *)
      let facts = U.get_facts_matching universe 
        ~predicate:"language" 
        ~pattern:[None; Some (String "ocaml")] in
      
      (* Time-travel query *)
      let old_universe = U.create storage ~tx_id:12345L in
      let old_facts = U.get_facts_matching old_universe
        ~predicate:"language"
        ~pattern:[None; Some (String "ocaml")] in
    ]}
*)

(** {2 Universe Functor} *)

module Make (S : Storage.STORAGE) : sig
  type t
  (** A Datalog universe - thin wrapper over storage backend *)
  
  (** {3 Construction} *)
  
  val create : S.t -> t
  (** Create universe with storage backend.
      
      Snapshot isolation is handled by the storage backend itself.
      For Poneglyph: pass a Graph_store.t that represents the desired snapshot.
      For InMemory: no versioning.
  *)
  
  (** {3 Fact Access} *)
  
  val get_facts_matching : t -> predicate:string -> pattern:Value.t option list -> Storage.fact_tuple Relation.t
  (** Get facts matching a pattern.
      
      Pattern format: [Some v] for constants, [None] for wildcards.
      Storage backends optimize based on pattern (e.g., AVET index for value queries).
      
      Example:
      {[
        (* Query: language(?, "ocaml") *)
        get_facts_matching universe ~predicate:"language" 
          ~pattern:[None; Some (String "ocaml")]
        (* Uses AVET index scan for (language, "ocaml") *)
      ]}
      
      Returns streaming iterator - first result in <100ms even with millions of facts.
  *)
  
  val storage : t -> S.t
  (** Access underlying storage (for advanced use) *)
end

(** {2 InMemory Universe for Testing} *)

module InMemory : sig
  include module type of Make(Inmemory_storage)
  
  val create_empty : unit -> t
  (** Create universe with empty in-memory storage *)
  
  val of_facts : (string * Storage.fact_tuple list) list -> t
  (** Create universe from facts list.
      
      Example:
      {[
        let universe = Universe.InMemory.of_facts [
          ("edge", [[Int 1; Int 2]; [Int 2; Int 3]]);
          ("person", [[String "alice"; Int 30]]);
        ]
      ]}
  *)
end
