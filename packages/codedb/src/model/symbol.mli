open Std

type reference =
  | Module of Module_name.t
  | Value of Value_name.t
  | Type of Type_name.t
  | Interface of Module_name.t

type kind = Module | Value | Type | Interface

type files = {
  implementation : File.t option;
  interface : File.t option;
}

type t = {
  kind : kind;
  name : Module_name.t;
  package : Package_info.t;
  files : files;
}

val reference_name : reference -> string
val reference_kind : reference -> kind
val kind_to_string : kind -> string
val kind_from_string : string -> kind option
val make :
  kind:kind ->
  name:Module_name.t ->
  package:Package_info.t ->
  files:files ->
  t
val kind_to_fact_string : kind -> string
val entity_uri : t -> Poneglyph.Uri.t
val to_facts : tx_id:UUID.t -> ?stated_at:Datetime.t -> t -> Poneglyph.Fact.t list
val to_json : t -> Data.Json.t
val from_json : Data.Json.t -> (t, string) result
