open Std

type def = Uri.t * Fact.t list
(** A definition is a URI + schema facts about that entity *)

val namespace : string -> Uri.part
(** Create a namespace for your schema *)

val kind : ns:Uri.part -> string -> def
(** Define a kind (entity type) *)

val field : ns:Uri.part -> string -> def
(** Define a field (attribute) *)

val doc : string -> def -> def
(** Add documentation *)

val used_on : def -> def -> def
(** Specify which kind(s) this field can be used on *)

val value_type : Uri.t -> def -> def
(** Specify the value type for this field *)

val cardinality : string -> def -> def
(** Specify cardinality: "one" or "many" *)

val required : bool -> def -> def
(** Specify if this field is required *)

val bootstrap : stated_at:Datetime.t -> Fact.t list
(** Generate all bootstrap facts for the core @ schema *)
