open Poneglyph

(** Schema definition utilities for Poneglyph.

    In Poneglyph, the schema itself is stored as facts! This module provides
    tools for defining schemas declaratively. Schema definitions are just
    entities with facts about them.

    Example:
    {[
      module Tusk = struct
        let ns = Schema.namespace "tusk"

        let file =
          Schema.kind ~ns "file"
          |> Schema.doc "A File in the Tusk schema"

        let content_hash =
          Schema.field ~ns "content_hash"
          |> Schema.used_on file
          |> Schema.value Schema.Type.string
          |> Schema.doc "The content hash of a file"
      end

      (* Register the schema *)
      Schema.register store [file; content_hash]
    ]} *)

(** {1 Schema Namespace}

    Schema metadata is stored under the "schema" namespace. *)

val schema_ns : Uri.part
val kind_type : Uri.t
val doc_attr : Uri.t
val used_on_attr : Uri.t
val value_type_attr : Uri.t

(** {1 Value Type Entities} *)

val string_type : Uri.t
val int_type : Uri.t
val bool_type : Uri.t
val float_type : Uri.t
val uri_type : Uri.t
val datetime_type : Uri.t
val list_type : Uri.t -> Uri.t

module Type : sig
  val string : Uri.t
  val int : Uri.t
  val bool : Uri.t
  val float : Uri.t
  val uri : Uri.t
  val datetime : Uri.t
  val list : Uri.t -> Uri.t
end

(** {1 Schema Definition}

    A definition is a URI + a list of schema facts about that entity. *)

type def = Uri.t * Fact.t list

val namespace : string -> Uri.part
(** Create a namespace for your schema. *)

val kind : ns:Uri.part -> string -> def
(** Define a kind (entity type). Returns the kind URI and its schema facts. *)

val field : ns:Uri.part -> string -> def
(** Define a field (attribute). Returns the field URI and its schema facts. *)

val doc : string -> def -> def
(** Add documentation. Works for both kinds and fields. *)

val used_on : def -> def -> def
(** Specify which kind(s) this field can be used on. *)

val value : Uri.t -> def -> def
(** Specify the value type for this field. Use Schema.Type.* for types. *)

val register : Poneglyph.t -> def list -> unit
(** Register schema definitions into a store. This stores the schema facts so
    you can query them later. *)

(** {1 Fact Builders}

    These functions create fact builders (Uri.t -> Fact.t) from field
    definitions. *)

val string_value : field:def -> value:string -> Uri.t -> Fact.t
val int_value : field:def -> value:int -> Uri.t -> Fact.t
val bool_value : field:def -> value:bool -> Uri.t -> Fact.t
val float_value : field:def -> value:float -> Uri.t -> Fact.t
val uri_value : field:def -> value:Uri.t -> Uri.t -> Fact.t
val datetime_value : field:def -> value:Datetime.t -> Uri.t -> Fact.t
val uri_list_value : field:def -> values:Uri.t list -> Uri.t -> Fact.t
val string_list_value : field:def -> values:string list -> Uri.t -> Fact.t
