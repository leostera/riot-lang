open Std

(** {1 Schema Definition}
    
    Poneglyph schemas are self-describing: schema definitions are just facts
    about entities. This module provides helpers for defining kinds (entity types)
    and fields (attributes).
*)

type def = Uri.t * Fact.t list
(** A schema definition is a URI and the facts that describe it *)
(** A definition is a URI + schema facts about that entity *)

(** {2 Schema Builders} *)

val namespace : string -> Uri.part
(** Create a namespace part for URIs *)

val kind : ns:Uri.part -> string -> def
(** Define a kind (entity type).
    
    {[
      let file = kind ~ns:(namespace "tusk") "file"
    ]}
*)

val field : ns:Uri.part -> string -> def
(** Define a field (attribute).
    
    {[
      let content_hash = field ~ns:(namespace "tusk") "content_hash"
    ]}
*)

(** {2 Schema Decorators (Fluent API)} *)

val doc : string -> def -> def
(** Add documentation to a kind or field.
    
    {[
      let file = kind ~ns "file" |> doc "A source file in the project"
    ]}
*)

val used_on : def -> def -> def
(** Specify which kind(s) a field can be used on.
    
    {[
      let content_hash = field ~ns "content_hash"
        |> used_on file
        |> used_on artifact
    ]}
*)

val value_type : Uri.t -> def -> def
(** Specify the value type of a field.
    
    {[
      let content_hash = field ~ns "content_hash"
        |> value_type Type.string
    ]}
*)

val cardinality : string -> def -> def
(** Specify cardinality: "one" or "many" *)

val required : bool -> def -> def
(** Mark field as required or optional *)

(** {2 Type URIs} *)

module Type : sig
  val string : Uri.t
  val int : Uri.t
  val bool : Uri.t
  val float : Uri.t
  val uri : Uri.t
  val datetime : Uri.t
end

(** {2 Fact Value Builders}
    
    Helper functions for creating facts with properly typed values.
*)

val string_value : field:def -> value:string -> Uri.t -> Fact.t
(** Create a string-valued fact for an entity.
    
    {[
      let make_hash ~hash entity =
        string_value ~field:content_hash ~value:hash entity
    ]}
*)

val int_value : field:def -> value:int -> Uri.t -> Fact.t
val bool_value : field:def -> value:bool -> Uri.t -> Fact.t
val float_value : field:def -> value:float -> Uri.t -> Fact.t
val uri_value : field:def -> value:Uri.t -> Uri.t -> Fact.t
val datetime_value : field:def -> value:Datetime.t -> Uri.t -> Fact.t

(** {2 Bootstrap} *)

val bootstrap : stated_at:Datetime.t -> Fact.t list
(** Generate bootstrap facts for the core Poneglyph schema.
    Defines: @kind:kind, @kind:field, @kind:type, @field:instance_of, etc. *)
(** Generate all bootstrap facts for the core @ schema *)
