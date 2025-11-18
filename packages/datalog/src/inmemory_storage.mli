open Std

(** Simple in-memory storage for testing Phase 0 query-only Datalog *)

type t

val create : unit -> t
(** Create empty in-memory storage *)

val add_fact : t -> predicate:string -> tuple:Storage.fact_tuple -> unit
(** Add a fact to storage *)

val get_facts_matching : 
  t -> 
  predicate:string -> 
  pattern:Value.t option list ->
  Storage.fact_tuple Relation.t
(** Get facts matching pattern (implements STORAGE interface) *)

val of_facts : (string * Storage.fact_tuple list) list -> t
(** Create storage from list of (predicate, tuples) pairs *)
