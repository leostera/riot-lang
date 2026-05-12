(* Backend-owned JS module references used by JIR imports and runtime helpers.

   The current layer is intentionally small: sibling compilation units and
   backend-owned runtime modules. The point is to keep module ownership
   structured inside JIR even while final path resolution stays heuristic. *)

module Core = Raml_core.Core_ir

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
val sibling_unit: string -> t

val runtime: string -> t

(** Split a Core entity path into a reference root plus remaining JS property
    accesses.

    The current policy is intentionally heuristic:
    - uppercase multi-segment heads become sibling-unit namespace imports
    - bound single-segment entities stay local identifiers
    - everything else falls back to a value identifier plus property tail

    This keeps the current module story centralized while leaving room for a
    future package/artifact-aware resolver. *)
val entity_reference: Core.Entity_id.t -> entity_reference

val namespace_binder: t -> Types.Binder.t

val namespace_import: t -> Types.Imports.requirement

val import_path: t -> string

val namespace_segments: t -> string list

val compare: t -> t -> Std.Order.t

val equal: t -> t -> bool

val kind_to_json: kind -> Std.Data.Json.t

val to_json: t -> Std.Data.Json.t
