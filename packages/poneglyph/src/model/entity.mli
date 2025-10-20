open Std

type t = { uri : Uri.t; ns : Uri.t; kind : Uri.t; facts : Fact.t list }
(** An entity with its URI, namespace, kind, and all its facts *)

val make : uri:Uri.t -> ns:Uri.t -> kind:Uri.t -> facts:Fact.t list -> t
(** Create an entity *)

val with_facts : t -> Fact.t list -> t
(** Update entity with new facts *)

val add_fact : t -> Fact.t -> t
(** Add a fact to entity *)
