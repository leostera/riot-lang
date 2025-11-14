(** Reference store - Ground truth oracle for LSM testing.
    
    This is a SIMPLE, OBVIOUSLY CORRECT implementation that the LSM
    engine will be tested against. It uses HashMaps and filtering,
    with no optimization whatsoever.
    
    Key properties:
    - Last tx wins for same fact_id
    - Retracted facts are filtered out in queries (after compact)
    - Query functions match LSM semantics exactly
    
    Example usage:
    {[
      let store = Ref_store.empty () in
      
      let fact = Fact.make ~source:(Uri.of_string "test:source")
        ~entity:(Uri.of_string "test:entity:1")
        ~attribute:(Uri.of_string "test:name")
        ~value:(Fact.String "Alice")
        ~stated_at:(Datetime.now ())
        ~tx_id:1 in
      
      Ref_store.add_fact store fact;
      let results = Ref_store.query_entity store ~entity:(Uri.of_string "test:entity:1") in
      (* results contains [fact] *)
    ]}
*)

open Std
open Std.Collections
open Model

type t
(** Mutable reference store *)

(** {2 Construction} *)

val empty : unit -> t
(** Create an empty reference store *)

(** {2 Operations} *)

val add_fact : t -> Fact.t -> unit
(** Add a fact to the store. If fact_id already exists with lower tx_id,
    this replaces it (last-tx-wins). *)

val compact : t -> unit
(** Apply compaction: keep only last version per fact_id, remove retracted.
    This simulates what LSM compaction does. *)

(** {2 Queries} *)

val query_entity : t -> entity:Uri.t -> Fact.t list
(** Find all live (non-retracted) facts for an entity *)

val query_attr_value : t -> attr:Uri.t -> value:Fact.value -> Fact.t list
(** Find all live facts with given attribute and value *)

val query_source : t -> source:Uri.t -> Fact.t list
(** Find all live facts from a given source *)

val query_fact_id : t -> fact_id:Uri.t -> Fact.t option
(** Find the latest version of a fact by its fact_uri *)

val all_live_facts : t -> Fact.t list
(** Get all live (non-retracted) facts in the store *)

(** {2 Statistics} *)

val fact_count : t -> int
(** Total number of fact versions stored *)

val live_fact_count : t -> int
(** Number of live (non-retracted, latest version) facts *)

val entity_count : t -> int
(** Number of unique entities *)
