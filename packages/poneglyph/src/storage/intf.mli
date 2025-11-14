open Std

(** {1 Storage Interface}
    
    Abstract interface for Poneglyph storage backends.
    Implementations: Inmemory (fast, volatile), SimpleFile (persistent).
*)

module type S = sig
  open Model

  type t
  (** Storage backend instance *)

  (** {2 Lifecycle} *)

  val create : unit -> t
  (** Create empty storage *)

  val load : string -> t
  (** Load storage from .db file *)

  val save : t -> string -> unit
  (** Save storage to .db file *)

  (** {2 Writing Facts} *)

  val state : t -> Fact.t list -> int
  (** State facts into storage. Returns transaction ID.
      All facts in the list share the same tx_id. *)

  val retract : t -> fact_uri:Uri.t -> unit
  (** Retract a fact by its URI. The fact remains in storage but
      is marked as retracted and excluded from current queries. *)

  (** {2 Querying} *)

  val get : t -> entity:Uri.t -> attr:Uri.t -> Fact.value option
  (** Get the current (non-retracted) value of an attribute for an entity.
      Returns None if no current value exists. *)

  val get_all_facts : t -> entity:Uri.t -> Fact.t list
  (** Get all facts about an entity, including retracted ones.
      Useful for examining history. *)

  val get_current_facts : t -> entity:Uri.t -> Fact.t list
  (** Get only current (non-retracted) facts about an entity. *)

  val exists : t -> Uri.t -> bool
  (** Check if an entity has any current (non-retracted) facts *)

  val get_kind : t -> Uri.t -> Uri.t option
  (** Get entity's kind/type via the [@field:instance_of] attribute *)

  val list_schemas : t -> Uri.t list
  (** List all registered schema namespaces (entities with kind [@kind:schema]) *)

  val get_all_current_facts : t -> Fact.t list
  (** Get all current (non-retracted) facts from storage.
      Used by Datalog integration to access all facts. *)

  (** {2 Reverse Lookups} *)

  val find_entities_by_attr_value : t -> attr:Uri.t -> value:Fact.value -> Uri.t list
  (** Find all entities that have a specific attribute-value pair.
      Only returns entities where this is a current (non-retracted) fact. *)

  (** {2 Statistics} *)

  val entity_count : t -> int
  (** Get total number of entities with facts *)

  val fact_count : t -> int
  (** Get total number of facts (including retracted) *)

  val current_fact_count : t -> int
  (** Get number of current (non-retracted) facts *)
end
