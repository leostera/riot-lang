open Std

type error = {
  message : string;
  syntax_kind : Syntax_kind.t;
  span : Ceibo.Span.t;
  context : string list;
}

type record_field_item =
  | RecordField of Cst.RecordField.t
  | Comment of Cst.comment
  | Docstring of Cst.docstring

val create_from_ceibo : kind:[
  | `Implementation
  | `Interface
] -> source:string -> tokens:Token.t list -> Cst.green_node -> (Cst.t, error) result

val structure_items_from_syntax_node : Cst.syntax_node -> (Cst.StructureItem.t list, error) result

val structure_items_from_syntax_node_with_source : source:string ->
Cst.syntax_node ->
(Cst.StructureItem.t list, error) result

val structure_items_from_syntax_nodes : Cst.syntax_node list -> (Cst.StructureItem.t list, error) result

val structure_items_of_payload : Cst.payload ->
(Cst.StructureItem.t list option, error) result

(** Ordered record-body helper stream.

    This keeps `RecordField` items in source order and surfaces any remaining
    standalone `}`-owned comments/docstrings after field-owned trivia has been
    excluded. *)
val record_field_items_of_fields : Cst.RecordField.t list -> record_field_item list

val structure_items_of_module_expression : Cst.ModuleExpression.t ->
(Cst.StructureItem.t list option, error) result

val signature_items_from_syntax_node : Cst.syntax_node -> (Cst.SignatureItem.t list, error) result

val signature_items_from_syntax_node_with_source : source:string ->
Cst.syntax_node ->
(Cst.SignatureItem.t list, error) result

val signature_items_from_syntax_nodes : Cst.syntax_node list -> (Cst.SignatureItem.t list, error) result

val signature_items_of_payload : Cst.payload ->
(Cst.SignatureItem.t list option, error) result

val signature_items_of_module_type : Cst.ModuleType.t ->
(Cst.SignatureItem.t list option, error) result
