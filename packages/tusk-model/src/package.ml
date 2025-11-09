(** Package - TOML parsing for package manifests *)

open Std
open Std.Data
open Std.Collections

(** Types *)

type dependency_source = Workspace | Path of Path.t
type dependency = { name : string; source : dependency_source }
type binary = { name : string; path : Path.t }
type library = { path : Path.t }
type sources = { src : Path.t list; native : Path.t list; tests : Path.t list; examples: Path.t list }

type foreign_dependency = {
  name : string;
  path : Path.t;
  inputs : Path.t list;
  build_cmd : string list;
  clean_cmd : string list option;
  test_cmd : string list option;
  outputs : Path.t list;
  env : (string * string) list;
}

type t = {
  name : string;
  path : Path.t;
  relative_path : Path.t;
  dependencies : dependency list;
  foreign_dependencies : foreign_dependency list;
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
      panic
        ("Dependency '" ^ name ^ "' with { workspace = true } not found in workspace dependencies")

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

let parse_foreign_dependency (name : string) (value : Toml.value)
    ~(package_path : Path.t) : (foreign_dependency, string) result =
  match value with
  | Toml.Table attrs -> (
      let get_string key =
        match List.assoc_opt key attrs with
        | Some (Toml.String s) -> Ok s
        | Some _ -> Error ("Foreign dependency '" ^ name ^ "': '" ^ key ^ "' must be a string")
        | None -> Error ("Foreign dependency '" ^ name ^ "': missing required field '" ^ key ^ "'")
      in
      let get_string_list key =
        match List.assoc_opt key attrs with
        | Some (Toml.Array arr) ->
            let strings = List.filter_map
              (function Toml.String s -> Some s | _ -> None) arr in
            if List.length strings = List.length arr then Ok strings
            else Error ("Foreign dependency '" ^ name ^ "': '" ^ key ^ "' must be an array of strings")
        | Some _ -> Error ("Foreign dependency '" ^ name ^ "': '" ^ key ^ "' must be an array")
        | None -> Error ("Foreign dependency '" ^ name ^ "': missing required field '" ^ key ^ "'")
      in
      let get_string_list_opt key =
        match List.assoc_opt key attrs with
        | Some (Toml.Array arr) ->
            let strings = List.filter_map
              (function Toml.String s -> Some s | _ -> None) arr in
            if List.length strings = List.length arr then Some strings
            else None
        | _ -> None
      in
      let get_env () =
        match List.assoc_opt "env" attrs with
        | Some (Toml.Table env_items) ->
            List.filter_map (fun (k, v) ->
              match v with
              | Toml.String s -> Some (k, s)
              | _ -> None
            ) env_items
        | _ -> []
      in
      
      match get_string "path", get_string_list "build_cmd", get_string_list "outputs" with
      | Ok path_str, Ok build_cmd, Ok outputs ->
          let dep_path = Path.(package_path / v path_str) in
          let output_paths = List.map Path.v outputs in
          let clean_cmd = get_string_list_opt "clean_cmd" in
          let test_cmd = get_string_list_opt "test_cmd" in
          let env = get_env () in
          
          (* Scan for foreign dependency source files *)
          let scan_foreign_inputs foreign_path =
            let rec scan_recursive ~from_dir ~rel_path ~exclude_dirs =
              match Fs.read_dir from_dir with
              | Error _ -> []
              | Ok iter ->
                  let entries = Std.Iter.MutIterator.to_list iter in
                  List.concat_map
                    (fun entry ->
                      let abs_path = Path.(from_dir / entry) in
                      let rel_path_full = Path.(rel_path / entry) in
                      let entry_name = Path.basename abs_path in
                      
                      (* Skip hidden files and build artifact directories *)
                      let should_skip = 
                        String.starts_with ~prefix:"." entry_name ||
                        List.mem entry_name exclude_dirs
                      in
                      
                      if should_skip then []
                      else
                        match Fs.is_dir abs_path with
                        | Ok true -> 
                            scan_recursive ~from_dir:abs_path ~rel_path:rel_path_full ~exclude_dirs
                        | Ok false ->
                            (* Only include source files and build configs *)
                            let should_include = 
                              String.ends_with ~suffix:".rs" entry_name ||
                              String.ends_with ~suffix:".c" entry_name ||
                              String.ends_with ~suffix:".h" entry_name ||
                              String.ends_with ~suffix:".cpp" entry_name ||
                              String.ends_with ~suffix:".hpp" entry_name ||
                              entry_name = "Cargo.toml" ||
                              entry_name = "Cargo.lock" ||
                              entry_name = "build.rs" ||
                              entry_name = "CMakeLists.txt" ||
                              entry_name = "Makefile"
                            in
                            if should_include then [ rel_path_full ]
                            else []
                        | Error _ -> [])
                    entries
            in
            let exclude_dirs = ["target"; "_build"; "build"; "dist"; "node_modules"] in
            scan_recursive ~from_dir:foreign_path ~rel_path:(Path.v ".") ~exclude_dirs
          in
          
          let inputs = scan_foreign_inputs dep_path in
          Log.info ("[PACKAGE] Foreign dependency '" ^ name ^ "' found " ^ Int.to_string (List.length inputs) ^ " input files");
          
          Ok { name; path = dep_path; inputs; build_cmd; clean_cmd; test_cmd; outputs = output_paths; env }
      | Error e, _, _ -> Error e
      | _, Error e, _ -> Error e
      | _, _, Error e -> Error e)
  | _ -> Error ("Foreign dependency '" ^ name ^ "' must be a table")

let parse_foreign_dependencies (items : (string * Toml.value) list)
    ~(package_path : Path.t) : (foreign_dependency list, string) result =
  Log.debug ("[PACKAGE] parse_foreign_dependencies: checking for 'foreign-dependencies' key");
  Log.debug ("[PACKAGE] Available keys: " ^ String.concat ", " (List.map fst items));
  
  (* Collect all keys that start with "foreign-dependencies." *)
  let foreign_dep_items = List.filter_map (fun (key, value) ->
    if String.starts_with ~prefix:"foreign-dependencies." key then
      (* Extract the dependency name after "foreign-dependencies." *)
      let prefix_len = String.length "foreign-dependencies." in
      let dep_name = String.sub key prefix_len (String.length key - prefix_len) in
      Some (dep_name, value)
    else None
  ) items in
  
  if List.length foreign_dep_items > 0 then
    Log.debug ("[PACKAGE] Found " ^ Int.to_string (List.length foreign_dep_items) ^ " foreign dependencies via dotted keys");
  
  (* Also check for standard nested table format *)
  let nested_deps = match List.assoc_opt "foreign-dependencies" items with
  | Some (Toml.Table deps) ->
      Log.debug ("[PACKAGE] Found foreign-dependencies table with " ^ Int.to_string (List.length deps) ^ " entries");
      deps
  | Some _ ->
      Log.warn ("[PACKAGE] foreign-dependencies exists but is not a table");
      []
  | None ->
      Log.debug ("[PACKAGE] No 'foreign-dependencies' table found");
      []
  in
  
  (* Combine both sources *)
  let all_deps = foreign_dep_items @ nested_deps in
  
  if all_deps = [] then Ok []
  else
    let results = List.map (fun (name, value) ->
      parse_foreign_dependency name value ~package_path
    ) all_deps in
    let errors = List.filter_map
      (fun r -> match r with Error e -> Some e | Ok _ -> None) results in
    if errors != [] then Error (String.concat "; " errors)
    else Ok (List.filter_map
      (fun r -> match r with Ok d -> Some d | Error _ -> None) results)

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
      | Some _, Some _ -> Error "Binary 'name' field must be a string"
      | Some _, None -> Error "Binary 'name' field must be a string"
      | None, Some _ -> Error "Binary 'path' field must be a string"
      | None, None ->
          Error "Binary entry missing required 'name' and 'path' fields")
  | _ -> Error "Binary entry must be a table"

let parse_binaries (items : (string * Toml.value) list) ~(package_path : Path.t)
    : (binary list, string) result =
  match List.assoc_opt "bin" items with
  | None -> Ok []
  | Some (Toml.Array bin_entries) ->
      let results = List.map (parse_binary ~package_path) bin_entries in
      let errors =
        List.filter_map
          (fun r -> match r with Error e -> Some e | Ok _ -> None)
          results
      in
      if errors != [] then Error (String.concat "; " errors)
      else
        Ok
          (List.filter_map
             (fun r -> match r with Ok b -> Some b | Error _ -> None)
             results)
  | Some _ -> Error "[[bin]] must be an array of tables"

let parse_library (items : (string * Toml.value) list) ~(package_path : Path.t)
    ~(package_name : string) : (library option, string) result =
  match List.assoc_opt "lib" items with
  | None ->
      (* Autodiscover: if src/<package_name>.ml exists, use it as library *)
      let default_lib_path =
        Path.(package_path / Path.v "src" / Path.v (package_name ^ ".ml"))
      in
      (match Fs.exists default_lib_path with
      | Ok true -> Ok (Some { path = default_lib_path })
      | Ok false | Error _ -> Ok None)
  | Some (Toml.Table lib_items) -> (
      match List.assoc_opt "path" lib_items with
      | Some (Toml.String path_str) ->
          let lib_path = Path.(package_path / Path.v path_str) in
          Ok (Some { path = lib_path })
      | None ->
          let default_path =
            Path.(
              package_path / Path.v "src" / Path.v (package_name ^ ".ml"))
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
  let example_files =
    scan_dir_recursive
      ~from_dir:Path.(package_path / Path.v "examples")
      ~rel_path:(Path.v "examples")
  in
  { src = src_files; tests = test_files; native = native_files; examples = example_files }

(** Autodiscover test binaries from test files ending in _tests.ml or -tests.ml *)
let autodiscover_test_binaries (sources : sources) ~(package_path : Path.t) :
    binary list =
  List.filter_map
    (fun test_file ->
      let filename = Path.basename test_file in
      if
        String.ends_with ~suffix:"_tests.ml" filename
        || String.ends_with ~suffix:"-tests.ml" filename
      then
        let binary_name =
          Path.remove_extension (Path.v filename) |> Path.to_string
        in
        (* test_file is already relative to package (e.g., tests/foo_tests.ml) *)
        let binary_path = test_file in
        Some { name = binary_name; path = binary_path }
      else None)
    sources.tests

(** Autodiscover example binaries from any .ml file in examples/ directory *)
let autodiscover_example_binaries (sources : sources) ~(package_path : Path.t) :
    binary list =
  List.filter_map
    (fun example_file ->
      let filename = Path.basename example_file in
      if String.ends_with ~suffix:".ml" filename then
        let binary_name =
          Path.remove_extension (Path.v filename) |> Path.to_string
        in
        (* example_file is already relative to package (e.g., examples/sqltool.ml) *)
        Some { name = binary_name; path = example_file }
      else None)
    sources.examples

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
        | Ok bins -> bins
        | Error msg ->
            Log.warn ("[PACKAGE] Failed to parse binaries for " ^ name ^ ": " ^ msg);
            []
      in
      let library =
        match parse_library items ~package_path:path ~package_name:name with
        | Ok lib -> lib
        | Error msg ->
            Log.warn ("[PACKAGE] Failed to parse library for " ^ name ^ ": " ^ msg);
            None
      in
      let foreign =
        match parse_foreign_dependencies items ~package_path:path with
        | Ok deps -> deps
        | Error msg ->
            Log.warn ("[PACKAGE] Failed to parse foreign dependencies for " ^ name ^ ": " ^ msg);
            []
      in
      let sources = scan_sources ~package_path:path in
      let test_binaries = autodiscover_test_binaries sources ~package_path:path in
      let example_binaries =
        autodiscover_example_binaries sources ~package_path:path
      in
      Log.debug ("[PACKAGE] " ^ name ^ ": discovered " ^ Int.to_string (List.length test_binaries) ^ " test binaries from " ^ Int.to_string (List.length sources.tests) ^ " test files");
      Log.debug ("[PACKAGE] " ^ name ^ ": discovered " ^ Int.to_string (List.length example_binaries) ^ " example binaries from " ^ Int.to_string (List.length sources.examples) ^ " example files");
      let all_binaries = binaries @ test_binaries @ example_binaries in
      Ok
        {
          name;
          path;
          relative_path;
          dependencies;
          foreign_dependencies = foreign;
          binaries = all_binaries;
          library;
          sources;
        }
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
          Some (Json.String rel_path_str) ) -> (
          match Path.of_string path_str, Path.of_string rel_path_str with
          | Ok path, Ok relative_path ->

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
              foreign_dependencies = [];
              binaries;
              library;
              sources = { src = []; native = []; tests = []; examples = [] };
            }
          | Error _, _ -> Error ("Invalid path in package JSON: " ^ path_str)
          | _, Error _ -> Error ("Invalid relative_path in package JSON: " ^ rel_path_str))
      | _ -> Error "Invalid package JSON")
  | _ -> Error "Package must be a JSON object"
