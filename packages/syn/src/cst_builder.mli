open Std

type error = {
  message: string;
  syntax_kind: Syntax_kind.t;
  span: Ceibo.Span.t;
  context: string list;
}
type record_field_item =
  | RecordField of Cst.RecordField.t
  | Comment of Cst.comment
  | Docstring of Cst.docstring
  | TrailingComment of Cst.comment
  | TrailingDocstring of Cst.docstring
type object_member_item =
  | ObjectMember of Cst.ObjectMember.t
  | Comment of Cst.comment
  | Docstring of Cst.docstring
type class_field_item =
  | ClassField of Cst.ClassField.t
  | Comment of Cst.comment
  | Docstring of Cst.docstring
type class_type_field_item =
  | ClassTypeField of Cst.ClassTypeField.t
  | Comment of Cst.comment
  | Docstring of Cst.docstring
val create_from_ceibo: kind:[
    | `Implementation
    | `Interface
  ] -> source:string -> tokens:Token.t list -> Cst.green_node -> (Cst.t, error) result

val structure_items_from_syntax_node: Cst.syntax_node -> (Cst.StructureItem.t list, error) result

val structure_items_from_syntax_node_with_source:
  source:string -> Cst.syntax_node -> (Cst.StructureItem.t list, error) result

val structure_items_from_syntax_nodes: Cst.syntax_node list -> (Cst.StructureItem.t list, error) result
(** Ordered record-body helper stream.

    This keeps `RecordField` items in source order and surfaces any remaining
    standalone `}`-owned comments/docstrings after field-owned trivia has been
    excluded. *)
val record_field_items_of_fields: Cst.RecordField.t list -> record_field_item list
(** Ordered object-body helper stream.

    This keeps `ObjectMember` items in source order and surfaces any
    standalone body comments/docstrings, optionally using the enclosing
    `source_node` to include trailing trivia on `end` when members are empty.
*)
val object_member_items_of_members:
  ?source_node:Cst.syntax_node -> Cst.ObjectMember.t list -> object_member_item list
(** Ordered class-structure helper stream.

    This keeps `ClassField` items in source order and surfaces any standalone
    body comments/docstrings, optionally using the enclosing `source_node` to
    include trailing trivia on `end` when fields are empty.
*)
val class_field_items_of_fields:
  ?source_node:Cst.syntax_node -> Cst.ClassField.t list -> class_field_item list
(** Ordered class-type signature helper stream.

    This keeps `ClassTypeField` items in source order and surfaces any
    standalone body comments/docstrings, optionally using the enclosing
    `source_node` to include trailing trivia on `end` when fields are empty.
*)
val class_type_field_items_of_fields:
  ?source_node:Cst.syntax_node -> Cst.ClassTypeField.t list -> class_type_field_item list

val structure_items_of_module_expression:
  Cst.ModuleExpression.t -> (Cst.StructureItem.t list, error) result

val signature_items_from_syntax_node: Cst.syntax_node -> (Cst.SignatureItem.t list, error) result

val signature_items_from_syntax_node_with_source:
  source:string -> Cst.syntax_node -> (Cst.SignatureItem.t list, error) result

val signature_items_from_syntax_nodes: Cst.syntax_node list -> (Cst.SignatureItem.t list, error) result

val signature_items_of_module_type: Cst.ModuleType.t -> (Cst.SignatureItem.t list, error) result

val pattern_of_syntax_node: Cst.syntax_node -> (Cst.Pattern.t, error) result

val expression_of_syntax_node: Cst.syntax_node -> (Cst.Expression.t, error) result
