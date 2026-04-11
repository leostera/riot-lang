(* Backend-owned JS module references used by JIR imports and runtime helpers.

   The current layer is intentionally small: sibling compilation units and
   backend-owned runtime modules. The point is to keep module ownership
   structured inside JIR even while final path resolution stays heuristic. *)
type kind = Types.Modules.kind =
  | Relative_unit
  | Runtime

type t = Types.Modules.t = {
  kind: kind;
  unit_name: string;
}

val sibling_unit: string -> t

val runtime: string -> t

val import_path: t -> string

val namespace_segments: t -> string list

val compare: t -> t -> int

val equal: t -> t -> bool

val kind_to_json: kind -> Std.Data.Json.t

val to_json: t -> Std.Data.Json.t
