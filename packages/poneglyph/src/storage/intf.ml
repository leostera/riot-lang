open Std

module type S = sig
  open Model

  type t

  val create : unit -> t
  val load : string -> t
  val save : t -> string -> unit
  val state : t -> Fact.t list -> int
  val retract : t -> fact_uri:Uri.t -> unit
  val get : t -> entity:Uri.t -> attr:Uri.t -> Fact.value option
  val get_all_facts : t -> entity:Uri.t -> Fact.t list
  val get_current_facts : t -> entity:Uri.t -> Fact.t list
  val exists : t -> Uri.t -> bool
  val get_kind : t -> Uri.t -> Uri.t option
  val list_schemas : t -> Uri.t list
end
