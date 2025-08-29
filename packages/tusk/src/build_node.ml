(** Build node definition - separated to avoid circular dependencies *)

type spec =
  | Unplanned
  | Planned of {
      hash : Hasher.hash;
      outs : Path.t list;
      actions : Actions.action list;
    }

type source_kind = 
  | C_stub       (* .c stub file *)
  | ML of {      (* .ml implementation file *)
      simple_name: string;     (* Original module name (e.g., "Config") *)
      namespaced_name: string; (* Full namespaced name (e.g., "Std__Config") *)
      namespace: string list;  (* For future folder-based namespacing *)
    }
  | MLI of {     (* .mli interface file *)
      simple_name: string;     (* Original module name (e.g., "Config") *)
      namespaced_name: string; (* Full namespaced name (e.g., "Std__Config") *)
      namespace: string list;  (* For future folder-based namespacing *)
    }
  | Other of string  (* Other file types *)

type source = {
  file: Path.t;
  kind: source_kind;
}

type t = {
  toolchain : Toolchains.toolchain;
  package : Workspace.package;
  srcs : source list;
  mutable deps : Node_id.t list; (* Now stores IDs, not nodes *)
  mutable spec : spec;
}

(* Helper functions *)
let is_planned node =
  match node.spec with Planned _ -> true | Unplanned -> false

let is_unplanned node = not (is_planned node)
let compare a b = String.compare a.package.name b.package.name

(* Hash computation result types *)
type hash_result =
  | Planned of t
  | MissingDependencies of { node : t; deps : t list }
  | Error of string

(** Compute content-based hash for a build node *)
let compute_hash node ~get_dep =
  (* This is a simplified version - the actual hashing is done by Build_planner *)
  (* We just check if dependencies are ready *)
  let dep_nodes = List.filter_map get_dep node.deps in
  let unplanned_deps = List.filter is_unplanned dep_nodes in

  if unplanned_deps <> [] then
    MissingDependencies { node; deps = unplanned_deps }
  else
    match node.spec with
    | Planned _ -> Planned node
    | Unplanned -> Error "Node should be planned by Build_planner.plan_node"
