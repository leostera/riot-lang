open Std
open Model

type t
(** High-level graph wrapper over storage backends *)

val create : ?persistent:string -> unit -> t
(** Create a new graph. If [persistent] is provided, uses file-based storage. *)

val state : t -> Fact.t list -> int
(** State facts into the graph, returns transaction ID *)

val retract : t -> fact_uri:Uri.t -> unit
(** Retract a fact *)

val get : t -> entity:Uri.t -> attr:Uri.t -> Fact.value option
(** Get current value of entity attribute *)

val get_all_facts : t -> entity:Uri.t -> Fact.t list
(** Get all facts (including retracted) about entity *)

val get_current_facts : t -> entity:Uri.t -> Fact.t list
(** Get only current facts about entity *)

val exists : t -> Uri.t -> bool
(** Check if entity has any current facts *)

val get_kind : t -> Uri.t -> Uri.t option
(** Get entity's kind *)

val list_schemas : t -> Uri.t list
(** List registered schemas *)

val save : t -> unit
(** Save to disk (for persistent graphs) *)

val transitive : t -> start:Uri.t -> edge:Uri.t -> max_depth:int option -> Uri.t list
(** Follow edges transitively from starting entity *)

val count_entities : t -> int
(** Count entities with current facts *)

val count_facts : t -> int
(** Count total facts (including retracted) *)

val count_current_facts : t -> int
(** Count non-retracted facts *)

val find_entities : t -> attr:Uri.t -> value:Fact.value -> Uri.t list
(** Find all entities with specific attribute=value pair (reverse lookup) *)

val find_by_kind : t -> kind:Uri.t -> Uri.t list
(** Find all entities of a specific kind *)

val get_all_current_facts : t -> Fact.t list
(** Get all current (non-retracted) facts from the graph. Used by Datalog integration. *)

val find_by_source : t -> source:Uri.t -> Uri.t list
(** Find all entities with facts from a specific source *)

val retract_by_source : t -> source:Uri.t -> unit
(** Retract all facts from a specific source *)
