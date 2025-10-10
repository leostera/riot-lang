open Std
open Poneglyph

(** Schema definition types *)

type value_type = String | Int | Bool | Float | Uri | DateTime | List of value_type

type kind_def = {
  uri : Uri.t;
  name : string;
  ns : Uri.part;
  doc : string option;
}

type field_def = {
  uri : Uri.t;
  name : string;
  ns : Uri.part;
  value_type : value_type option;
  used_on : kind_def list;
  doc : string option;
}

(** Namespace *)

let namespace name = Uri.ns name

(** Kind definition builders *)

let kind ~ns name =
  let uri = Uri.make [ ns; Uri.kind name ] in
  { uri; name; ns; doc = None }

module Kind = struct
  let doc doc_str kind_def = { kind_def with doc = Some doc_str }
end

(** Field definition builders *)

let field ~ns name =
  let uri = Uri.make [ ns; Uri.field name ] in
  { uri; name; ns; value_type = None; used_on = []; doc = None }

let used_on kind_def field_def =
  { field_def with used_on = kind_def :: field_def.used_on }

let value vt field_def = { field_def with value_type = Some vt }

module Field = struct
  let doc doc_str field_def = { field_def with doc = Some doc_str }
end

(** Fact builders using field definitions *)

let string_value ~field_def ~value entity =
  Fact.fact entity field_def.uri (Value.String value)

let int_value ~field_def ~value entity =
  Fact.fact entity field_def.uri (Value.Int value)

let bool_value ~field_def ~value entity =
  Fact.fact entity field_def.uri (Value.Bool value)

let float_value ~field_def ~value entity =
  Fact.fact entity field_def.uri (Value.Float value)

let uri_value ~field_def ~value entity =
  Fact.fact entity field_def.uri (Value.Uri value)

let datetime_value ~field_def ~value entity =
  Fact.fact entity field_def.uri (Value.DateTime value)

let uri_list_value ~field_def ~values entity =
  let value_list = Value.List (List.map (fun v -> Value.Uri v) values) in
  Fact.fact entity field_def.uri value_list

let string_list_value ~field_def ~values entity =
  let value_list = Value.List (List.map (fun v -> Value.String v) values) in
  Fact.fact entity field_def.uri value_list
