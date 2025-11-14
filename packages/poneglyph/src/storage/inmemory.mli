open Std
open Model
include Intf.S

val with_facts : t -> Fact.t list -> t
(** Create storage pre-populated with facts (for testing/bootstrap) *)

val find_entities_by_attr_value : t -> attr:Uri.t -> value:Fact.value -> Uri.t list
(** Find all entities with a specific attribute-value pair *)

val entity_count : t -> int
(** Get total number of entities *)

val fact_count : t -> int
(** Get total number of facts (including retracted) *)

val current_fact_count : t -> int
(** Get number of current (non-retracted) facts *)
