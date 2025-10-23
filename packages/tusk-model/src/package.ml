(** Package - TOML parsing for package manifests *)

open Std
open Std.Data

(** Types *)

type dependency_source = Workspace | Path of Path.t
type dependency = { name : string; source : dependency_source }
type binary = { name : string; path : Path.t }
type library = { path : Path.t }
type test_module = { name : string; path : Path.t }

type t = {
  name : string;
  path : Path.t;
  relative_path : Path.t;
  dependencies : dependency list;
  binaries : binary list;
  library : library option;
  test_library : library option;
  test_modules : test_module list;
}

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

let parse_library (items : (string * Toml.value) list) ~(package_path : Path.t)
    ~(package_name : string) : (library option, string) result =
  match List.assoc_opt "lib" items with
  | None -> Ok None
  | Some (Toml.Table lib_items) -> (
      match List.assoc_opt "path" lib_items with
      | Some (Toml.String path_str) ->
          let lib_path = Path.(package_path / Path.v path_str) in
          Ok (Some { path = lib_path })
      | None ->
          let default_path =
            Path.(
              package_path / Path.v "src" / Path.v (format "%s.ml" package_name))
          in
          Ok (Some { path = default_path })
      | Some _ -> Error "Library 'path' field must be a string")
  | Some _ -> Error "[lib] must be a table"

let parse_test_library (items : (string * Toml.value) list)
    ~(package_path : Path.t) ~(package_name : string) :
    (library option, string) result =
  let tests_dir = Path.(package_path / Path.v "tests") in
  match Fs.is_dir tests_dir with
  | Ok true ->
      let test_lib_path =
        Path.(tests_dir / Path.v (format "%s_tests.ml" package_name))
      in
      Ok (Some { path = test_lib_path })
  | _ -> Ok None

let scan_test_modules ~(package_path : Path.t) : test_module list =
  let tests_dir = Path.(package_path / Path.v "tests") in
  match Fs.read_dir tests_dir with
  | Error _ -> []
  | Ok iter ->
      let entries = Std.Iter.MutIterator.to_list iter in
      List.filter_map
        (fun entry ->
          let name = Path.basename entry in
          if String.ends_with ~suffix:"_tests.ml" name then
            let module_name = String.sub name 0 (String.length name - 3) in
            Some { name = module_name; path = Path.(Path.v "tests" / entry) }
          else None)
        entries

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
      let library =
        match parse_library items ~package_path:path ~package_name:name with
        | Ok lib -> lib
        | Error msg ->
            Log.warn "[PACKAGE] Failed to parse library for %s: %s" name msg;
            None
      in
      let test_library =
        match
          parse_test_library items ~package_path:path ~package_name:name
        with
        | Ok lib -> lib
        | Error msg ->
            Log.warn "[PACKAGE] Failed to parse test library for %s: %s" name
              msg;
            None
      in
      let test_modules = scan_test_modules ~package_path:path in
      Ok
        {
          name;
          path;
          relative_path;
          dependencies;
          binaries;
          library;
          test_library;
          test_modules;
        }
  | _ -> Error "TOML is not a table"
