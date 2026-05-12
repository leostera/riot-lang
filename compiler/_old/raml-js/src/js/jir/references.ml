open Std
module Core = Raml_core.Core_ir
module Jir = Types
module Modules = Modules
module Objects = Objects

let named_property_access = Objects.named_access

let entity = fun entity_id ->
  let reference = Modules.entity_reference entity_id in
  let base =
    match reference.root with
    | Modules.Identifier entity_id -> Jir.Expr.Identifier entity_id
    | Modules.Namespace module_ref -> Jir.Expr.Imported (Modules.namespace_import module_ref)
  in
  List.fold_left reference.properties ~init:base ~fn:named_property_access
