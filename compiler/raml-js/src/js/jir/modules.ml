module Core = Raml_core.Core_ir
module Syntax = Syntax

type kind = Types.Modules.kind =
  | Relative_unit
  | Runtime

type t = Types.Modules.t = {
  kind: kind;
  unit_name: string;
  import_path: string;
  namespace: string list;
}

type reference_root =
  | Identifier of Core.Entity_id.t
  | Namespace of t

type entity_reference = {
  root: reference_root;
  properties: string list;
}

let is_module_segment = fun segment ->
  String.length segment > 0 && Syntax.is_ascii_uppercase segment.[0]

let sibling_unit = Types.Modules.sibling_unit

let runtime = Types.Modules.runtime

let entity_reference = fun entity_id ->
  let parts = Core.Entity_id.to_segments entity_id in
  match parts with
  | [] -> { root = Identifier entity_id; properties = [] }
  | head :: tail ->
      if not (List.is_empty tail) && is_module_segment head then
        { root = Namespace (sibling_unit head); properties = tail }
      else if Option.is_some (Core.Entity_id.binding_id entity_id) && List.is_empty tail then
        { root = Identifier entity_id; properties = [] }
      else
        { root = Identifier (Core.Entity_id.of_name head); properties = tail }

let namespace_binder = Types.Modules.namespace_binder

let import_path = Types.Modules.import_path

let namespace_segments = Types.Modules.namespace_segments

let compare = Types.Modules.compare

let equal = Types.Modules.equal

let kind_to_json = Types.Modules.kind_to_json

let to_json = Types.Modules.to_json
