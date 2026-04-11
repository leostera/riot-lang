open Std

module Core = Raml_core.Core_ir
module Jir = Types
module Intrinsics = Intrinsics
module Syntax = Syntax

let is_module_segment = fun segment ->
  String.length segment > 0 && Syntax.is_ascii_uppercase segment.[0]

let string_literal = fun value -> Jir.Expr.Literal (Jir.Literal.String value)

let named_property_access = fun object_ property ->
  if Syntax.can_use_dot_property property then
    Intrinsics.member object_ property
  else
    Intrinsics.index object_ (string_literal property)

let entity = fun entity_id ->
  let parts = Core.Entity_id.to_segments entity_id in
  match parts with
  | [] -> Jir.Expr.Identifier entity_id
  | head :: tail ->
      let base =
        if not (List.is_empty tail) && is_module_segment head then
          let module_ref = Jir.Modules.sibling_unit head in
          Jir.Expr.Imported (Jir.Imports.namespace
            ~from:module_ref
            ~local:(Jir.Modules.namespace_binder module_ref)
            ())
        else if Option.is_some (Core.Entity_id.binding_id entity_id) && List.is_empty tail then
          Jir.Expr.Identifier entity_id
        else
          Jir.Expr.Identifier (Core.Entity_id.of_name head)
      in
      List.fold_left named_property_access base tail
