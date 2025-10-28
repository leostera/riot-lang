(** Package - TOML parsing for package manifests *)

open Std
open Std.Data

(** Types *)

type dependency_source = Workspace | Path of Path.t
type dependency = { name : string; source : dependency_source }
type binary = { name : string; path : Path.t }
type library = { path : Path.t }
type sources = { src : Path.t list; native : Path.t list; tests : Path.t list }

type t = {
  name : string;
  path : Path.t;
  relative_path : Path.t;
  dependencies : dependency list;
  binaries : binary list;
  library : library option;
  sources : sources;
}

let equal a b = a.name = b.name && a.path = b.path

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
          let bin_path = Path.v path_str in
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

let scan_sources ~(package_path : Path.t) : sources =
  let rec scan_dir_recursive ~from_dir ~rel_path =
    match Fs.read_dir from_dir with
    | Error _ -> []
    | Ok iter ->
        let entries = Std.Iter.MutIterator.to_list iter in
        List.concat_map
          (fun filename ->
            let abs_path = Path.(from_dir / filename) in
            let rel_path_full = Path.(rel_path / filename) in
            match Fs.is_dir abs_path with
            | Ok true ->
                scan_dir_recursive ~from_dir:abs_path ~rel_path:rel_path_full
            | Ok false -> [ rel_path_full ]
            | Error _ -> [])
          entries
  in
  let src_files =
    scan_dir_recursive
      ~from_dir:Path.(package_path / Path.v "src")
      ~rel_path:(Path.v "src")
  in
  let test_files =
    scan_dir_recursive
      ~from_dir:Path.(package_path / Path.v "tests")
      ~rel_path:(Path.v "tests")
  in
  let native_files =
    scan_dir_recursive
      ~from_dir:Path.(package_path / Path.v "native")
      ~rel_path:(Path.v "native")
  in
  { src = src_files; tests = test_files; native = native_files }

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
      let sources = scan_sources ~package_path:path in
      Ok { name; path; relative_path; dependencies; binaries; library; sources }
  | _ -> Error "TOML is not a table"

let to_json (pkg : t) : Json.t =
  let dependencies_json =
    Json.Array
      (List.map
         (fun (dep : dependency) ->
           Json.Object
             [
               ("name", Json.String dep.name);
               ( "source",
                 match dep.source with
                 | Workspace -> Json.String "workspace"
                 | Path p -> Json.String (Path.to_string p) );
             ])
         pkg.dependencies)
  in
  let binaries_json =
    Json.Array
      (List.map
         (fun (bin : binary) ->
           Json.Object
             [
               ("name", Json.String bin.name);
               ("path", Json.String (Path.to_string bin.path));
             ])
         pkg.binaries)
  in
  let library_json =
    match pkg.library with
    | Some lib ->
        Json.Object [ ("path", Json.String (Path.to_string lib.path)) ]
    | None -> Json.Null
  in
  Json.Object
    [
      ("name", Json.String pkg.name);
      ("path", Json.String (Path.to_string pkg.path));
      ("relative_path", Json.String (Path.to_string pkg.relative_path));
      ("dependencies", dependencies_json);
      ("binaries", binaries_json);
      ("library", library_json);
    ]

let from_json (json : Json.t) : (t, string) result =
  match json with
  | Json.Object fields -> (
      match
        ( List.assoc_opt "name" fields,
          List.assoc_opt "path" fields,
          List.assoc_opt "relative_path" fields )
      with
      | ( Some (Json.String name),
          Some (Json.String path_str),
          Some (Json.String rel_path_str) ) ->
          let path = Path.of_string path_str |> Result.unwrap in
          let relative_path = Path.of_string rel_path_str |> Result.unwrap in

          let dependencies =
            match List.assoc_opt "dependencies" fields with
            | Some (Json.Array deps) ->
                List.filter_map
                  (function
                    | Json.Object dep_fields -> (
                        match
                          ( List.assoc_opt "name" dep_fields,
                            List.assoc_opt "source" dep_fields )
                        with
                        | ( Some (Json.String dep_name),
                            Some (Json.String "workspace") ) ->
                            Some { name = dep_name; source = Workspace }
                        | ( Some (Json.String dep_name),
                            Some (Json.String source_path) ) ->
                            Some
                              {
                                name = dep_name;
                                source = Path (Path.v source_path);
                              }
                        | _ -> None)
                    | _ -> None)
                  deps
            | _ -> []
          in

          let binaries =
            match List.assoc_opt "binaries" fields with
            | Some (Json.Array bins) ->
                List.filter_map
                  (function
                    | Json.Object bin_fields -> (
                        match
                          ( List.assoc_opt "name" bin_fields,
                            List.assoc_opt "path" bin_fields )
                        with
                        | ( Some (Json.String bin_name),
                            Some (Json.String bin_path) ) ->
                            Some { name = bin_name; path = Path.v bin_path }
                        | _ -> None)
                    | _ -> None)
                  bins
            | _ -> []
          in

          let library =
            match List.assoc_opt "library" fields with
            | Some (Json.Object lib_fields) -> (
                match List.assoc_opt "path" lib_fields with
                | Some (Json.String lib_path) -> Some { path = Path.v lib_path }
                | _ -> None)
            | _ -> None
          in

          Ok
            {
              name;
              path;
              relative_path;
              dependencies;
              binaries;
              library;
              sources = { src = []; native = []; tests = [] };
            }
      | _ -> Error "Invalid package JSON")
  | _ -> Error "Package must be a JSON object"
