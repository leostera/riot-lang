open Std

(** {1 Entity - High-level Entity Record}
    
    An entity is a collection of facts about a URI. This module provides
    a convenient record type and utilities for working with entities.
*)

type t = {
  uri : Uri.t;
  kind : Uri.t option;
  facts : (Uri.t * Fact.value) list;
}
(** An entity record with its URI, optional kind, and current facts *)

val make : uri:Uri.t -> kind:Uri.t option -> facts:(Uri.t * Fact.value) list -> t
(** Create an entity record *)

val to_string : t -> string
(** Pretty-print entity with all its facts *)

val get_attr : t -> Uri.t -> Fact.value option
(** Get attribute value from entity's facts *)
