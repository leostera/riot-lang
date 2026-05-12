open Std
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
  match String.length segment with
  | 0 -> false
  | _ ->
      match String.get segment ~at:0 with
      | Some c -> Syntax.is_ascii_uppercase c
      | None -> false

let sibling_unit = Types.Modules.sibling_unit

let runtime = Types.Modules.runtime

let entity_reference = fun entity_id ->
  let parts = Core.Entity_id.to_segments entity_id in
  match parts with
  | [] -> { root = Identifier entity_id; properties = [] }
  | head :: tail ->
      match tail with
      | _ :: _ ->
          if is_module_segment head then
            { root = Namespace (sibling_unit head); properties = tail }
          else
            { root = Identifier (Core.Entity_id.from_name head); properties = tail }
      | [] ->
          if Option.is_some (Core.Entity_id.binding_id entity_id) then
            { root = Identifier entity_id; properties = [] }
          else
            { root = Identifier (Core.Entity_id.from_name head); properties = tail }

let namespace_binder = Types.Modules.namespace_binder

let namespace_import = fun module_ref ->
  Types.Imports.namespace ~from:module_ref ~local:(namespace_binder module_ref) ()

let import_path = Types.Modules.import_path

let namespace_segments = Types.Modules.namespace_segments

let compare = Types.Modules.compare

let equal = Types.Modules.equal

let kind_to_json = Types.Modules.kind_to_json

let to_json = Types.Modules.to_json
