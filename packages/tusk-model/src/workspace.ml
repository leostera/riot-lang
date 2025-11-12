(** Workspace - TOML parsing for workspace manifests *)

open Std
open Std.Collections
open Std.Data
open Std.IO

(** Types *)

type t = {
  root : Path.t;
  target_dir_root : Path.t;
  packages : Package.t list;
  profile_overrides : (string * Package.profile_override) list;
}

(** Workspace TOML parsing *)

type manifest = {
  members : Path.t list;
  dependencies : Package.dependency list;
  profile_overrides : (string * Package.profile_override) list;
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

let parse_members (toml : Toml.value) : Path.t list =
  match toml with
  | Toml.Table items -> (
      match List.assoc_opt "workspace" items with
      | Some (Toml.Table workspace_items) -> (
          match List.assoc_opt "members" workspace_items with
          | Some (Toml.Array members) ->
              List.filter_map
                (fun m -> Option.map Path.v (Toml.get_string m))
                members
          | _ -> [])
      | _ -> [])
  | _ -> []

let parse_workspace_dependencies (toml : Toml.value) : Package.dependency list =
  Log.debug ("[WORKSPACE] parse_workspacE_dependencies has items: " ^ Toml.to_string toml);
  match toml with
  | Toml.Table items -> (
      match List.assoc_opt "dependencies" items with
      | Some (Toml.Table dep_items) -> parse_dependencies dep_items
      | _ -> [])
  | _ -> []

let parse_profile_overrides (toml : Toml.value) : (string * Profile.profile_override) list =
  Log.debug "[WORKSPACE] parse_profile_overrides called";
  match toml with
  | Toml.Table items -> (
      Log.debug ("[WORKSPACE] Looking for [profile] in TOML with " ^ Int.to_string (List.length items) ^ " top-level keys");
      Log.debug ("[WORKSPACE] Top-level keys: " ^ String.concat ", " (List.map fst items));
      match List.assoc_opt "profile" items with
      | Some (Toml.Table profile_items) ->
          Log.debug ("[WORKSPACE] Found [profile] section with " ^ Int.to_string (List.length profile_items) ^ " profiles");
          let result = List.filter_map (fun (profile_name, value) ->
            Log.debug ("[WORKSPACE] Parsing profile: " ^ profile_name);
            match value with
            | Toml.Table profile_table ->
                Log.debug ("[WORKSPACE] Profile " ^ profile_name ^ " has " ^ Int.to_string (List.length profile_table) ^ " fields");
                Some (profile_name, Profile.override_from_toml profile_table)
            | _ -> 
                Log.debug ("[WORKSPACE] Profile " ^ profile_name ^ " is not a table, skipping");
                None
          ) profile_items in
          Log.debug ("[WORKSPACE] Parsed " ^ Int.to_string (List.length result) ^ " profile overrides");
          result
      | _ -> 
          Log.debug "[WORKSPACE] No [profile] section found in TOML";
          [])
  | _ -> 
      Log.debug "[WORKSPACE] TOML root is not a table";
      []

let of_toml (toml : Toml.value) : (manifest, string) result =
  let members = parse_members toml in
  let dependencies = parse_workspace_dependencies toml in
  let profile_overrides = parse_profile_overrides toml in
  Ok { members; dependencies; profile_overrides }

let manifest_from_toml = of_toml [@@deprecated "Use of_toml instead"]

let make ~root ~packages ?(profile_overrides = []) () : t =
  (* Note: Hardcoded to avoid circular dependency with Tusk_dirs.
     Keep in sync with Tusk_dirs.build_dir_name *)
  { root; target_dir_root = Path.(root / Path.v "_build"); packages; profile_overrides }

(** Utility functions *)

let project_id workspace =
  let root_str = Path.to_string workspace.root in
  String.map (fun c -> if c = '/' then '-' else c) root_str

let server_port workspace =
  let root_str = Path.to_string workspace.root in
  let hash = Std.Crypto.hash_string root_str in
  let hash_int = Std.Crypto.Digest.to_int hash in
  let port_range = 65535 - 49152 in
  50152 + (abs hash_int mod port_range)

(** Command discovery functions - moved here to avoid circular dependency *)
let discover_commands (workspace : t) : Package_command.t list =
  List.concat_map (fun (pkg : Package.t) -> pkg.commands) workspace.packages

let find_command (workspace : t) (name : string) : Package_command.t option =
  discover_commands workspace
  |> List.find_opt (fun (cmd : Package_command.t) -> cmd.name = name)

module Tests = struct
  let test_parse_workspace_toml () : (unit, string) result = Ok () [@test]
end [@test]
