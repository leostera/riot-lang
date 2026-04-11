open Std

module Core = Raml_core.Core_ir
module Jir = Types
module Intrinsics = Intrinsics
module Modules = Modules

let string_literal = fun value -> Jir.Expr.Literal (Jir.Literal.String value)

let named_property_access = fun object_ property ->
  if Syntax.can_use_dot_property property then
    Intrinsics.member object_ property
  else
    Intrinsics.index object_ (string_literal property)

let entity = fun entity_id ->
  let reference = Modules.entity_reference entity_id in
  let base =
    match reference.root with
    | Modules.Identifier entity_id -> Jir.Expr.Identifier entity_id
    | Modules.Namespace module_ref ->
        Jir.Expr.Imported (Jir.Imports.namespace
          ~from:module_ref
          ~local:(Modules.namespace_binder module_ref)
          ())
  in
  List.fold_left named_property_access base reference.properties
