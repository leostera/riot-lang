(** Workspace - TOML parsing for workspace manifests *)

open Std
open Std.Data

(** Types *)

type t = { root : Path.t; target_dir_root : Path.t; packages : Package.t list }

(** Workspace TOML parsing *)

type manifest = {
  members : string list;
  dependencies : Package.dependency list;
}

let parse_dependency (name : string) (value : Toml.value) : Package.dependency =
  match value with
  | Toml.Table attrs -> (
      match List.assoc_opt "path" attrs with
      | Some (Toml.String path_str) -> { name; source = Path (Path.v path_str) }
      | _ -> { name; source = Workspace })
  | _ -> { name; source = Workspace }

let parse_dependencies (items : (string * Toml.value) list) :
    Package.dependency list =
  List.map (fun (name, value) -> parse_dependency name value) items

let parse_members (toml : Toml.value) : string list =
  match toml with
  | Toml.Table items -> (
      match List.assoc_opt "workspace" items with
      | Some (Toml.Table workspace_items) -> (
          match List.assoc_opt "members" workspace_items with
          | Some (Toml.Array members) -> List.filter_map Toml.get_string members
          | _ -> [])
      | _ -> [])
  | _ -> []

let parse_workspace_dependencies (toml : Toml.value) : Package.dependency list =
  Log.debug "[WORKSPACE] parse_workspacE_dependencies has items: %s"
    (Toml.to_string toml);
  match toml with
  | Toml.Table items -> (
      match List.assoc_opt "dependencies" items with
      | Some (Toml.Table dep_items) -> parse_dependencies dep_items
      | _ -> [])
  | _ -> []

let manifest_from_toml (toml : Toml.value) : (manifest, string) result =
  let members = parse_members toml in
  let dependencies = parse_workspace_dependencies toml in
  Ok { members; dependencies }

let make ~root ~packages : t =
  { root; target_dir_root = Path.(root / Path.v "target"); packages }

(** Utility functions *)

let project_id workspace =
  let root_str = Path.to_string workspace.root in
  String.map (fun c -> if c = '/' then '-' else c) root_str

let server_port workspace =
  let root_str = Path.to_string workspace.root in
  let hash = Hashtbl.hash root_str in
  let port_range = 65535 - 49152 in
  49152 + (abs hash mod port_range)

module Tests = struct
  let test_parse_workspace_toml () : (unit, string) result = Ok () [@test]
end [@test]
