open Std

(** {1 Storage - Pluggable Backend Interface}
    
    Phase 0: Query-Only Datalog Core
    
    This interface provides snapshot-based access to facts with:
    - Streaming evaluation (zero materialization)
    - Snapshot isolation (handled by storage backend internally)
    
    {2 Design Philosophy}
    
    The storage layer provides read-only snapshots of facts.
    Snapshot isolation is handled internally by each backend:
    
    - Poneglyph: Graph_store.t represents a specific snapshot
    - InMemory: No versioning needed (testing only)
    
    Datalog is now a thin query layer over the storage snapshot:
    - No derived facts in Datalog (use projection jobs instead)
    - No rules evaluation (query-only)
*)

(** {2 Core Types} *)

type fact_tuple = Value.t list
(** A single fact tuple: [Int 1; String "alice"; Uri "..."] *)

(** {2 Storage Interface} *)

module type STORAGE = sig
  type t
  (** Storage backend handle - may include snapshot info internally *)
  
  val get_facts_matching : 
    t -> 
    predicate:string -> 
    pattern:Value.t option list ->
    fact_tuple Relation.t
  (** Get facts matching a pattern.
      
      Storage backends handle snapshot isolation internally:
      - Poneglyph: Graph_store.t represents a specific snapshot
      - InMemory: no versioning needed
      
      Pattern uses [Some v] for constants, [None] for wildcards.
      Storage backends SHOULD optimize based on pattern:
      - [Some entity; None] → entity lookup
      - [None; Some value] → AVET index scan
      - _ → full scan with filter
      
      Example:
      {[
        (* Find all files with language "ocaml" *)
        get_facts_matching storage 
          ~predicate:"language" 
          ~pattern:[None; Some (String "ocaml")]
        (* Returns streaming iterator of matching facts *)
      ]}
      
      Performance: MUST return streaming iterator, not materialized list.
      First result should appear in <100ms even with millions of facts.
  *)
end

(** {2 Helper Functions} *)

val matches_pattern : Value.t option list -> fact_tuple -> bool
(** Check if a tuple matches a pattern (with None as wildcard).
    Used as a default filter when [get_facts_matching] is not optimized.
    
    Example:
    {[
      matches_pattern [Some (Int 1); None] [Int 1; Int 2]  (* true *)
      matches_pattern [Some (Int 1); None] [Int 2; Int 3]  (* false *)
    ]}
*)
