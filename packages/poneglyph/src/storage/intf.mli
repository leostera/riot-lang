open Std

module type S = sig
  open Model

  type t

  val create : unit -> t
  (** Create empty storage *)

  val load : string -> t
  (** Load from .db file *)

  val save : t -> string -> unit
  (** Save to .db file *)

  val state : t -> Fact.t list -> int
  (** State facts, returns tx_id *)

  val retract : t -> fact_uri:Uri.t -> unit
  (** Retract a fact by its URI *)

  val get : t -> entity:Uri.t -> attr:Uri.t -> Fact.value option
  (** Get latest non-retracted value for entity+attribute *)

  val get_all_facts : t -> entity:Uri.t -> Fact.t list
  (** Get all facts about entity (including retracted) *)

  val get_current_facts : t -> entity:Uri.t -> Fact.t list
  (** Get only current (non-retracted) facts *)

  val exists : t -> Uri.t -> bool
  (** Check if entity has any facts *)

  val get_kind : t -> Uri.t -> Uri.t option
  (** Get entity's kind via @field:instance_of *)

  val list_schemas : t -> Uri.t list
  (** List all registered schemas *)
end
