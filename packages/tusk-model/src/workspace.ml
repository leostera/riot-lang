(** Workspace - TOML parsing for workspace manifests *)
open Std
open Std.Collections
open Std.Data
open Std.IO

(** Types *)
type t = {
  root: Path.t;
  target_dir_root: Path.t;
  packages: Package.t list;
  profile_overrides: (string * Package.profile_override) list;
}

(** Workspace TOML parsing *)
type manifest = {
  members: Path.t list;
  dependencies: Package.dependency list;
  dev_dependencies: Package.dependency list;
  build_dependencies: Package.dependency list;
  profile_overrides: (string * Package.profile_override) list;
  target_dir: string option;
}

let parse_dependency : string -> Toml.value -> Package.dependency = fun name value ->
  match value with
  | Toml.Table attrs -> (
      match List.assoc_opt "path" attrs with
      | Some (Toml.String path_str) -> { name; source = Path (Path.v path_str) }
      | _ -> { name; source = Workspace }
    )
  | _ -> { name; source = Workspace }

let parse_dependencies : (string * Toml.value) list -> Package.dependency list = fun items ->
  List.map (fun ((name, value)) -> parse_dependency name value) items

let parse_dependency_section section_name (toml: Toml.value) : Package.dependency list =
  match toml with
  | Toml.Table items -> (
      match List.assoc_opt section_name items with
      | Some (Toml.Table dep_items) -> parse_dependencies dep_items
      | _ -> []
    )
  | _ -> []

let parse_members : Toml.value -> Path.t list = fun toml ->
  match toml with
  | Toml.Table items -> (
      match List.assoc_opt "workspace" items with
      | Some (Toml.Table workspace_items) -> (
          match List.assoc_opt "members" workspace_items with
          | Some (Toml.Array members) ->
              List.filter_map
                (fun m ->
                  Option.map Path.v (Toml.get_string m))
                members
          | _ -> []
        )
      | _ -> []
    )
  | _ -> []

let parse_workspace_dependencies : Toml.value -> Package.dependency list = fun toml ->
  Log.debug ("[WORKSPACE] parse_workspacE_dependencies has items: " ^ Toml.to_string toml);
  parse_dependency_section "dependencies" toml

let parse_workspace_dev_dependencies : Toml.value -> Package.dependency list = fun toml ->
  parse_dependency_section "dev-dependencies" toml

let parse_workspace_build_dependencies : Toml.value -> Package.dependency list = fun toml ->
  parse_dependency_section "build-dependencies" toml

let parse_profile_overrides : Toml.value -> (string * Profile.profile_override) list = fun toml ->
  Log.debug "[WORKSPACE] parse_profile_overrides called";
  match toml with
  | Toml.Table items -> (
      Log.debug
        ("[WORKSPACE] Looking for [profile] in TOML with " ^ Int.to_string (List.length items) ^ " top-level keys");
      Log.debug ("[WORKSPACE] Top-level keys: " ^ String.concat ", " (List.map fst items));
      match List.assoc_opt "profile" items with
      | Some (Toml.Table profile_items) ->
          Log.debug
            ("[WORKSPACE] Found [profile] section with "
            ^ Int.to_string (List.length profile_items)
            ^ " profiles");
          let result =
            List.filter_map
              (fun ((profile_name, value)) ->
                Log.debug ("[WORKSPACE] Parsing profile: " ^ profile_name);
                match value with
                | Toml.Table profile_table ->
                    Log.debug
                      ("[WORKSPACE] Profile "
                      ^ profile_name
                      ^ " has "
                      ^ Int.to_string (List.length profile_table)
                      ^ " fields");
                    Some (profile_name, Profile.override_from_toml profile_table)
                | _ ->
                    Log.debug ("[WORKSPACE] Profile " ^ profile_name ^ " is not a table, skipping");
                    None)
              profile_items
          in
          Log.debug
            ("[WORKSPACE] Parsed " ^ Int.to_string (List.length result) ^ " profile overrides");
          result
      | _ ->
          Log.debug "[WORKSPACE] No [profile] section found in TOML";
          []
    )
  | _ ->
      Log.debug "[WORKSPACE] TOML root is not a table";
      []

let parse_target_dir : Toml.value -> string option = fun toml ->
  match toml with
  | Toml.Table items -> (
      match List.assoc_opt "tusk" items with
      | Some (Toml.Table tusk_items) -> (
          match List.assoc_opt "target_dir" tusk_items with
          | Some (Toml.String target_dir) -> Some target_dir
          | _ -> None
        )
      | _ -> None
    )
  | _ -> None

let of_toml : Toml.value -> (manifest, string) result = fun toml ->
  let members = parse_members toml in
  let dependencies = parse_workspace_dependencies toml in
  let dev_dependencies = parse_workspace_dev_dependencies toml in
  let build_dependencies = parse_workspace_build_dependencies toml in
  let profile_overrides = parse_profile_overrides toml in
  let target_dir = parse_target_dir toml in
  Ok {
    members;
    dependencies;
    dev_dependencies;
    build_dependencies;
    profile_overrides;
    target_dir;
  }

let manifest_from_toml = of_toml [@@deprecated "Use of_toml instead"]

let resolve_target_dir_root = fun ~root ?target_dir () ->
  let target_dir =
    match target_dir with
    | Some target_dir -> target_dir
    | None -> "_build"
  in
  let target_dir_path = Path.v target_dir in
  if Path.is_absolute target_dir_path then
    target_dir_path
  else
    Path.(root / target_dir_path)

let make ~root ~packages ?(profile_overrides = []) ?target_dir () : t = {
  root;
  target_dir_root = resolve_target_dir_root ~root ?target_dir ();
  packages;
  profile_overrides
}
(** Utility functions *)
let project_id = fun workspace ->
  let root_str = Path.to_string workspace.root in
  String.map
    (fun c ->
      if c = '/' then
        '-'
      else
        c)
    root_str

let server_port = fun workspace ->
  let root_str = Path.to_string workspace.root in
  let hash = Std.Crypto.hash_string root_str in
  let hash_int = Std.Crypto.Digest.to_int hash in
  let port_range = 65_535 - 49_152 in
  50_152 + (abs hash_int mod port_range)
(** Command discovery functions - moved here to avoid circular dependency *)
let discover_commands : t -> Package_command.t list = fun workspace ->
  List.concat_map (fun (pkg: Package.t) -> pkg.commands) workspace.packages

let find_command : t -> string -> Package_command.t option = fun workspace name ->
  discover_commands workspace |> List.find_opt (fun (cmd: Package_command.t) -> cmd.name = name)

let discover_fix_providers : t -> Fix_provider.t list = fun workspace ->
  List.concat_map (fun (pkg: Package.t) -> pkg.fix_providers) workspace.packages

module Tests = struct
  let test_parse_workspace_toml () : (unit, string) result = Ok () [@test]

  let test_parse_target_dir () : (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[workspace]
members = ["packages/foo"]

[tusk]
target_dir = "build-out"
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let manifest = of_toml toml |> Result.expect ~msg:"expected workspace manifest" in
    if manifest.target_dir = Some "build-out" then
      Ok ()
    else
      Error "expected [tusk].target_dir to be parsed" [@test]

  let test_make_uses_custom_target_dir () : (unit, string) result =
    let workspace = make ~root:(Path.v "/tmp/example") ~packages:[] ~target_dir:"build-out" () in
    if Path.to_string workspace.target_dir_root = "/tmp/example/build-out" then
      Ok ()
    else
      Error "expected custom target_dir_root" [@test]

  let test_discover_fix_providers () : (unit, string) result =
    let package_toml =
      Std.Data.Toml.parse
        {|
[package]
name = "std"
version = "0.1.0"

[tusk.fix.provider]
path = "fix/no_stdlib_provider.ml"
rules = ["no-stdlib"]
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let package = Package.from_toml
      package_toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example/packages/std")
      ~relative_path:(Path.v "packages/std")
    |> Result.expect ~msg:"expected package manifest" in
    let workspace = make ~root:(Path.v "/tmp/example") ~packages:[ package ] () in
    match discover_fix_providers workspace with
    | [ provider ] ->
        if
          String.equal provider.package_name "std"
          && String.equal (Path.to_string provider.source_path) "/tmp/example/packages/std/fix/no_stdlib_provider.ml"
          && String.equal provider.name "std"
          && provider.rules = [ "std:no-stdlib" ]
        then
          Ok ()
        else
          Error "expected provider metadata to round-trip"
    | _ -> Error "expected one fix provider" [@test]

  let test_parse_workspace_dependency_classes () : (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[workspace]
members = ["packages/foo"]

[dependencies]
std = { path = "packages/std" }

[dev-dependencies]
propane = { path = "packages/propane" }

[build-dependencies]
fixme = { path = "packages/fixme" }
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let manifest = of_toml toml |> Result.expect ~msg:"expected workspace manifest" in
    if
      List.map (fun (dep: Package.dependency) -> dep.Package.name) manifest.dependencies = [ "std" ]
      && List.map (fun (dep: Package.dependency) -> dep.Package.name) manifest.dev_dependencies
      = [ "propane" ]
      && List.map (fun (dep: Package.dependency) -> dep.Package.name) manifest.build_dependencies
      = [ "fixme" ]
    then
      Ok ()
    else
      Error "expected workspace dependency classes to parse" [@test]
end [@test]
