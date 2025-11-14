open Std

(** {1 Storage - Pluggable Backend Interface}
    
    Datalog is a query engine, not a database. This interface allows
    plugging in different storage backends:
    
    - InMemory: Simple HashMap-based storage (default)
    - Poneglyph: Zero-copy access to graph database
    - SQLite: Disk-backed storage
    - Custom: Your own implementation
    
    {2 Design Philosophy}
    
    The storage layer provides read-only access to base facts.
    Derived facts (from rules) are always computed in-memory by the
    evaluation engine.
    
    This separation means:
    - No copying facts from Poneglyph into Datalog
    - Lazy evaluation - only fetch predicates when rules need them
    - Storage optimized for its domain (graphs, SQL, etc.)
*)

(** {2 Core Types} *)

type fact_tuple = Value.t list
(** A single fact tuple: [Int 1; String "alice"; Uri "..."] *)

(** {2 Storage Interface} *)

module type STORAGE = sig
  type t
  (** Storage backend handle *)
  
  (** {3 Fact Access} *)
  
  val get_facts : t -> predicate:string -> fact_tuple Relation.t
  (** Get all facts for a predicate as a sorted relation.
      Returns empty relation if predicate doesn't exist.
      
      Example:
      {[
        get_facts storage ~predicate:"edge"
        (* Returns Relation of tuples: [[Int 1; Int 2]; [Int 2; Int 3]; ...] *)
      ]}
  *)
  
  val predicates : t -> string list
  (** List all available predicate names.
      Used for introspection and debugging.
      
      Example:
      {[
        predicates storage
        (* Returns ["edge"; "person"; "parent"] *)
      ]}
  *)
  
  (** {3 Efficient Iteration} *)
  
  val iter_facts : t -> predicate:string -> (fact_tuple -> unit) -> unit
  (** Iterate over facts without materializing the entire relation.
      Useful for large datasets where we want streaming access.
      
      Example:
      {[
        iter_facts storage ~predicate:"edge" (fun tuple ->
          match tuple with
          | [Int x; Int y] -> printf "edge(%d, %d)\n" x y
          | _ -> ())
      ]}
  *)
  
  (** {3 Optional: Indexed Access}
      
      These are optional optimizations. Storage backends MAY provide
      indexed access for better performance. If not provided, the
      evaluator will use full scans with filtering.
  *)
  
  val get_facts_matching : t -> predicate:string -> pattern:Value.t option list -> fact_tuple Relation.t
  (** Get facts matching a pattern with wildcards.
      Pattern uses [Some v] for constants, [None] for wildcards.
      
      Example:
      {[
        (* Find all edges starting from node 1 *)
        get_facts_matching storage ~predicate:"edge" 
          ~pattern:[Some (Int 1); None]
        (* Returns [[Int 1; Int 2]; [Int 1; Int 5]] *)
      ]}
      
      Default implementation (if not overridden):
      {[
        let facts = get_facts t ~predicate in
        Relation.filter (matches_pattern pattern) facts
      ]}
  *)
end

(** {2 Requirements for Poneglyph Implementation}
    
    When implementing this interface for Poneglyph, you'll need:
    
    {3 1. Predicate Mapping}
    
    Map Poneglyph concepts to Datalog predicates:
    
    {[
      (* Example predicate scheme *)
      "triple"      -> (subject, predicate, object) RDF triples
      "node"        -> (id, type, label) node data
      "edge"        -> (from, to, label) edge data
      "property"    -> (entity, key, value) key-value properties
    ]}
    
    {3 2. Value Conversion}
    
    Convert Poneglyph values to Datalog values:
    
    {[
      Poneglyph.NodeId   -> Value.Int (or Value.String)
      Poneglyph.String   -> Value.String
      Poneglyph.Number   -> Value.Int
      Poneglyph.Uri      -> Value.Uri
    ]}
    
    {3 3. Efficient Fact Fetching}
    
    [get_facts] should be fast and avoid copying:
    - Return a lazy iterator wrapped in Relation
    - Use Poneglyph's native traversal
    - Cache frequently accessed predicates
    
    {3 4. Optional Indexing}
    
    If Poneglyph has indexes, implement [get_facts_matching]:
    - Use indexes for prefix queries
    - Example: [pattern:[Some x; None; None]] uses index on first column
    
    {3 Example Implementation Skeleton}
    
    {[
      module PoneglyghStorage : Storage.STORAGE = struct
        type t = Poneglyph.graph
        
        let get_facts graph ~predicate =
          match predicate with
          | "triple" -> 
              (* Fetch all RDF triples *)
              let tuples = Poneglyph.all_triples graph
                |> List.map (fun (s, p, o) -> 
                    [node_to_value s; string_to_value p; node_to_value o])
              in
              Relation.of_list tuples
          
          | "edge" ->
              (* Fetch all edges *)
              let tuples = Poneglyph.all_edges graph
                |> List.map (fun edge -> 
                    [node_id_to_value edge.from; 
                     node_id_to_value edge.to;
                     string_to_value edge.label])
              in
              Relation.of_list tuples
          
          | _ -> Relation.empty ()
        
        let predicates graph =
          ["triple"; "edge"; "node"; "property"]
        
        let iter_facts graph ~predicate f =
          (* Stream facts without materializing *)
          match predicate with
          | "edge" ->
              Poneglyph.iter_edges graph (fun edge ->
                f [node_id_to_value edge.from; ...])
          | _ -> ()
        
        let get_facts_matching graph ~predicate ~pattern =
          (* Use Poneglyph indexes if available *)
          match predicate, pattern with
          | "edge", [Some (Int from); None; None] ->
              (* Index scan: all edges from specific node *)
              let edges = Poneglyph.edges_from graph from in
              ...
          | _ ->
              (* Fall back to full scan *)
              let facts = get_facts graph ~predicate in
              Relation.filter (matches_pattern pattern) facts
      end
    ]}
*)

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
