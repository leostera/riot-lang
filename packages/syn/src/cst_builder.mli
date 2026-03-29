open Std

type error = {
  message : string;
  syntax_kind : Syntax_kind.t;
  span : Ceibo.Span.t;
  context : string list;
}
val create_from_ceibo : kind:[
  | `Implementation
  | `Interface
] -> source:string -> tokens:Token.t list -> Cst.green_node -> (Cst.t, error) result

val structure_items_from_syntax_node : Cst.syntax_node -> (Cst.StructureItem.t list, error) result

val structure_items_from_syntax_node_with_source : source:string ->
Cst.syntax_node ->
(Cst.StructureItem.t list, error) result

val structure_items_from_syntax_nodes : Cst.syntax_node list -> (Cst.StructureItem.t list, error) result

val structure_items_of_module_expression : Cst.ModuleExpression.t ->
(Cst.StructureItem.t list option, error) result

val signature_items_from_syntax_node : Cst.syntax_node -> (Cst.SignatureItem.t list, error) result

val signature_items_from_syntax_node_with_source : source:string ->
Cst.syntax_node ->
(Cst.SignatureItem.t list, error) result

val signature_items_from_syntax_nodes : Cst.syntax_node list -> (Cst.SignatureItem.t list, error) result

val signature_items_of_module_type : Cst.ModuleType.t ->
(Cst.SignatureItem.t list option, error) result
