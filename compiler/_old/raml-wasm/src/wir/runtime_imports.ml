open Std
module Core = Raml_core.Core_ir
module Wasm_types = Types

let runtime_import = fun name ->
  Wasm_types.Import.{ module_name = "riot:runtime"; name; kind = Runtime }

let host_import = fun name -> Wasm_types.Import.{ module_name = "riot:host"; name; kind = Host }

let import_of_primitive = fun primitive ->
  match primitive with
  | Core.Primitive.Equal -> Some (runtime_import "eq")
  | _ -> None

let classify_primitive = fun primitive ->
  match import_of_primitive primitive with
  | Some import -> (
      match import.kind with
      | Wasm_types.Import.Runtime -> Wasm_types.Primitive_kind.Runtime
      | Wasm_types.Import.Host -> Wasm_types.Primitive_kind.Host_import
    )
  | None -> Wasm_types.Primitive_kind.Pure

let import_of_runtime_name = fun name ->
  match name with
  | "print_endline"
  | "print_string"
  | "print_char"
  | "print_int"
  | "print_newline"
  | "string_of_int"
  | "string_of_float"
  | "int_of_string"
  | "float_of_string"
  | "printf" -> Some (runtime_import name)
  | "read_file"
  | "write_file" -> Some (host_import name)
  | _ -> None

let import_of_surface_path = fun path -> import_of_runtime_name (Core.Surface_path.last_name path)

let import_of_direct_callee = fun entity_id ->
  match Core.Entity_id.binding_id entity_id with
  | Some binding_id when Option.is_some (Core.Binding_id.stamp binding_id) -> None
  | Some _ -> import_of_surface_path (Core.Entity_id.surface_path entity_id)
  | None -> import_of_surface_path (Core.Entity_id.surface_path entity_id)
