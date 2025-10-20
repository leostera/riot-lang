open Std

type value =
  | String of string
  | Int of int
  | Bool of bool
  | Float of float
  | Uri of Uri.t
  | DateTime of Datetime.t  (** Fact values *)

type t = {
  fact_uri : Uri.t;
  entity : Uri.t;
  attribute : Uri.t;
  value : value;
  stated_at : Datetime.t;
  tx_id : int;
  retracted : bool;
}
(** A fact with full history tracking *)

val make :
  entity:Uri.t ->
  attribute:Uri.t ->
  value:value ->
  stated_at:Datetime.t ->
  tx_id:int ->
  t
(** Create a new fact with auto-generated fact_uri *)

val for_entity : Uri.t -> (Uri.t -> t) list -> t list
(** Build facts for an entity by applying fact builders. Example:
    Fact.for_entity uri [builder1; builder2] *)

val value_to_string : value -> string
(** Human-readable representation of value *)
