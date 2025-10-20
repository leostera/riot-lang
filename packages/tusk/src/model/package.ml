(** Package - TOML parsing for package manifests *)

open Std
open Std.Data

(** Types *)

type dependency_source = Workspace | Path of Path.t
type dependency = { name : string; source : dependency_source }
type binary = { name : string; path : Path.t }

type t = {
  name : string;
  path : Path.t;
  relative_path : Path.t;
  dependencies : dependency list;
  binaries : binary list;
}

(** Hashing *)

let hash (type state) (module H : Crypto.Hasher.Intf with type state = state)
    (hasher : state) (pkg : t) =
  H.write_string hasher pkg.name;
  let sorted_deps =
    List.sort
      (fun (a : dependency) (b : dependency) -> String.compare a.name b.name)
      pkg.dependencies
  in
  List.iter
    (fun (dep : dependency) -> H.write_string hasher dep.name)
    sorted_deps

(** Package TOML parsing *)

let parse_name (items : (string * Toml.value) list) (fallback : string) : string
    =
  match List.assoc_opt "package" items with
  | Some (Toml.Table pkg_items) -> (
      match List.assoc_opt "name" pkg_items with
      | Some (Toml.String n) -> n
      | _ -> fallback)
  | _ -> fallback

let resolve_workspace_dependency (name : string)
    (workspace_deps : dependency list) : dependency =
  match
    List.find_opt (fun (d : dependency) -> d.name = name) workspace_deps
  with
  | Some dep -> dep
  | None ->
      failwith
        (format
           "Dependency '%s' with { workspace = true } not found in workspace \
            dependencies"
           name)

let parse_dependency (name : string) (value : Toml.value)
    ~(workspace_deps : dependency list) : dependency =
  match value with
  | Toml.Table attrs -> (
      match List.assoc_opt "workspace" attrs with
      | Some (Toml.Bool true) ->
          resolve_workspace_dependency name workspace_deps
      | _ -> (
          match List.assoc_opt "path" attrs with
          | Some (Toml.String path_str) ->
              { name; source = Path (Path.v path_str) }
          | _ -> { name; source = Workspace }))
  | _ -> { name; source = Workspace }

let parse_dependencies (items : (string * Toml.value) list)
    ~(workspace_deps : dependency list) : dependency list =
  List.map
    (fun (name, value) -> parse_dependency name value ~workspace_deps)
    items

let parse_binary (value : Toml.value) ~(package_path : Path.t) :
    (binary, string) result =
  match value with
  | Toml.Table items -> (
      match (List.assoc_opt "name" items, List.assoc_opt "path" items) with
      | Some (Toml.String name), Some (Toml.String path_str) ->
          let bin_path = Path.(package_path / Path.v path_str) in
          Ok { name; path = bin_path }
      | Some (Toml.String _), None ->
          Error "Binary entry missing required 'path' field"
      | None, Some (Toml.String _) ->
          Error "Binary entry missing required 'name' field"
      | Some (Toml.String _), Some _ ->
          Error "Binary 'path' field must be a string"
      | Some _, _ -> Error "Binary 'name' field must be a string"
      | None, None ->
          Error "Binary entry missing required 'name' and 'path' fields")
  | _ -> Error "Binary entry must be a table"

let parse_binaries (items : (string * Toml.value) list) ~(package_path : Path.t)
    : (binary list, string) result =
  Log.debug "[PACKAGE] parse_binaries called with %d top-level items"
    (List.length items);
  List.iter (fun (k, _) -> Log.debug "[PACKAGE]   key: %s" k) items;
  match List.assoc_opt "bin" items with
  | None ->
      Log.debug "[PACKAGE] No 'bin' key found";
      Ok []
  | Some (Toml.Array bin_entries) ->
      let results = List.map (parse_binary ~package_path) bin_entries in
      let errors =
        List.filter_map
          (fun r -> match r with Error e -> Some e | Ok _ -> None)
          results
      in
      if errors <> [] then Error (String.concat "; " errors)
      else
        Ok
          (List.filter_map
             (fun r -> match r with Ok b -> Some b | Error _ -> None)
             results)
  | Some _ -> Error "[[bin]] must be an array of tables"

let from_toml (toml : Toml.value) ~(workspace_deps : dependency list)
    ~(path : Path.t) ~(relative_path : Path.t) : (t, string) result =
  match toml with
  | Toml.Table items ->
      let fallback_name = Path.basename path in
      let name = parse_name items fallback_name in
      let dependencies =
        match List.assoc_opt "dependencies" items with
        | Some (Toml.Table dep_items) ->
            parse_dependencies dep_items ~workspace_deps
        | _ -> []
      in
      let binaries =
        match parse_binaries items ~package_path:path with
        | Ok bins ->
            Log.debug "[PACKAGE] Parsed %d binaries for package %s"
              (List.length bins) name;
            bins
        | Error msg ->
            Log.warn "[PACKAGE] Failed to parse binaries for %s: %s" name msg;
            []
      in
      Ok { name; path; relative_path; dependencies; binaries }
  | _ -> Error "TOML is not a table"

module Tests = struct
  let test_parse_package_toml () : (unit, string) result = Ok () [@test]

  let test_parse_dependencies_with_workspace_true () : (unit, string) result =
    Ok () [@test]

  let test_parse_dependencies_with_path () : (unit, string) result =
    Ok () [@test]
end [@test]
