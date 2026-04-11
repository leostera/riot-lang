type kind = Types.Modules.kind =
  | Relative_unit
  | Runtime

type t = Types.Modules.t = {
  kind: kind;
  unit_name: string;
  import_path: string;
  namespace: string list;
}

let sibling_unit = Types.Modules.sibling_unit

let runtime = Types.Modules.runtime

let namespace_binder = Types.Modules.namespace_binder

let import_path = Types.Modules.import_path

let namespace_segments = Types.Modules.namespace_segments

let compare = Types.Modules.compare

let equal = Types.Modules.equal

let kind_to_json = Types.Modules.kind_to_json

let to_json = Types.Modules.to_json
