open Std

(** {1 InMemory Storage - Default HashMap Backend}
    
    Simple in-memory storage using HashMap for predicate lookup.
    This is the default storage backend for Datalog.
    
    Best for:
    - Small to medium datasets (< 1M facts)
    - Testing and prototyping
    - Standalone Datalog queries without external database
    
    Not ideal for:
    - Very large datasets (use Poneglyph or SQLite instead)
    - Persistent storage (this is in-memory only)
    - Multi-process sharing (each process has its own copy)
*)

include Storage.STORAGE

(** {2 Construction} *)

val create : unit -> t
(** Create empty in-memory storage *)

val of_facts : (string * Storage.fact_tuple list) list -> t
(** Create storage from list of (predicate, facts) pairs.
    
    Example:
    {[
      of_facts [
        ("edge", [[Int 1; Int 2]; [Int 2; Int 3]]);
        ("person", [[String "alice"; Int 30]]);
      ]
    ]}
*)

(** {2 Mutation} *)

val add_fact : t -> predicate:string -> tuple:Storage.fact_tuple -> unit
(** Add a single fact to storage.
    Automatically deduplicates and maintains sorted order.
    
    Example:
    {[
      let storage = create () in
      add_fact storage ~predicate:"edge" ~tuple:[Int 1; Int 2];
      add_fact storage ~predicate:"edge" ~tuple:[Int 2; Int 3]
    ]}
*)

val add_facts : t -> predicate:string -> tuples:Storage.fact_tuple list -> unit
(** Add multiple facts at once (more efficient than repeated add_fact) *)

val clear : t -> predicate:string -> unit
(** Remove all facts for a predicate *)

val clear_all : t -> unit
(** Remove all facts from storage *)

(** {2 Statistics} *)

val fact_count : t -> predicate:string -> int
(** Number of facts for a predicate *)

val total_facts : t -> int
(** Total number of facts across all predicates *)
