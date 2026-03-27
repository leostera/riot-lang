(** Package - TOML parsing for package manifests *)

open Std
open Std.Data
open Std.Collections

(** Types *)

type dependency_source = Workspace | Path of Path.t
type dependency_scope = Normal | Dev | Build
type key = Key of string
type dependency = { name : string; source : dependency_source }
type binary = { name : string; path : Path.t }
type library = { path : Path.t }
type sources = { src : Path.t list; native : Path.t list; tests : Path.t list; examples: Path.t list; bench: Path.t list }

type target_platform = string  (* "macos", "linux", "windows", etc. *)

(** Re-export types from Profile *)
type 'a override = 'a Profile.override
type profile_override = Profile.profile_override

(** Target-specific override - can override any profile field for a specific platform *)
type target_override = {
  profile_override : Profile.profile_override option;  (* Profile fields that can be overridden *)
}

type compiler_config = { 
  profile_overrides : (string * profile_override) list;  (* "debug" -> override, "release" -> override *)
  target_overrides : (target_platform * target_override) list;  (* "macos" -> override, etc. *)
}

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
  dev_dependencies : dependency list;
  build_dependencies : dependency list;
  foreign_dependencies : foreign_dependency list;
  binaries : binary list;
  library : library option;
  sources : sources;
  compiler : compiler_config;
  commands : Package_command.t list;
  fix_providers : Fix_provider.t list;
}

let equal a b = a.name = b.name && a.path = b.path
let key_of_string value = Key value
let key_to_string (Key value) = value
let key_equal left right = String.equal (key_to_string left) (key_to_string right)
let key_compare left right = String.compare (key_to_string left) (key_to_string right)

let dependencies_for_scope scope (pkg : t) =
  match scope with
  | Normal -> pkg.dependencies
  | Dev -> pkg.dev_dependencies
  | Build -> pkg.build_dependencies

let binary_scope (bin : binary) =
  let path_str = Path.to_string bin.path in
  if
    String.starts_with ~prefix:"tests/" path_str
    || String.starts_with ~prefix:"examples/" path_str
    || String.starts_with ~prefix:"bench/" path_str
  then Dev
  else Normal

let binaries_for_scope scope (pkg : t) =
  match scope with
  | Normal ->
      List.filter (fun bin -> binary_scope bin = Normal) pkg.binaries
  | Dev ->
      List.filter (fun bin -> binary_scope bin = Dev) pkg.binaries
  | Build -> []

let commands_for_scope scope (pkg : t) =
  match scope with
  | Normal -> pkg.commands
  | Dev | Build -> []

let sources_for_scope scope (pkg : t) =
  match scope with
  | Normal ->
      { pkg.sources with tests = []; examples = []; bench = [] }
  | Dev ->
      {
        pkg.sources with
        src = [];
        native = [];
      }
  | Build ->
      {
        pkg.sources with
        src = [];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      }

let for_scope scope (pkg : t) =
  match scope with
  | Normal ->
      {
        pkg with
        dev_dependencies = [];
        build_dependencies = [];
        binaries = binaries_for_scope Normal pkg;
        commands = commands_for_scope Normal pkg;
        sources = sources_for_scope Normal pkg;
      }
  | Dev ->
      {
        pkg with
        build_dependencies = [];
        library = None;
        binaries = binaries_for_scope Dev pkg;
        commands = commands_for_scope Dev pkg;
        sources = sources_for_scope Dev pkg;
      }
  | Build ->
      {
        pkg with
        dependencies = [];
        dev_dependencies = [];
        library = None;
        binaries = [];
        commands = commands_for_scope Build pkg;
        sources = sources_for_scope Build pkg;
      }

let build_graph_dependencies (pkg : t) = pkg.dependencies @ pkg.dev_dependencies

let all_dependencies (pkg : t) =
  pkg.dependencies @ pkg.dev_dependencies @ pkg.build_dependencies

(** Check if this package is a workspace member (not an external dependency).
    External dependencies have relative_path that escapes the workspace (starts with "../")
    or uses absolute paths. *)
let is_workspace_member (pkg : t) : bool =
  let rel_str = Path.to_string pkg.relative_path in
  not (String.starts_with ~prefix:"../" rel_str || Path.is_absolute pkg.relative_path)

(** Validate package name according to Tusk naming conventions *)
let validate_name name =
  let is_alpha c = 
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  in
  
  let is_lowercase c = 
    c >= 'a' && c <= 'z'
  in
  
  let is_digit c = 
    c >= '0' && c <= '9'
  in
  
  let is_alphanum c = 
    is_alpha c || is_digit c
  in
  
  let is_valid_char c = 
    is_alphanum c || c = '-' || c = '_'
  in
  
  if String.length name = 0 then
    Error "Package name cannot be empty"
  else
    let first_char = String.get name 0 in
    let last_char = String.get name (String.length name - 1) in
    
    if not (is_lowercase first_char && is_alpha first_char) then
      Error ("Package name must start with a lowercase letter. Try '" ^ 
                      String.lowercase_ascii name ^ "' instead")
    else if first_char = '-' || first_char = '_' then
      Error "Package name cannot start with hyphen or underscore"
    else if last_char = '-' || last_char = '_' then
      Error "Package name cannot end with hyphen or underscore"
    else if not (String.for_all is_valid_char name) then
      Error "Package name can only contain lowercase letters, numbers, hyphens, and underscores"
    else
      Ok name

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

let parse_dependency_section section_name items ~(workspace_deps : dependency list) =
  match List.assoc_opt section_name items with
  | Some (Toml.Table dep_items) ->
      parse_dependencies dep_items ~workspace_deps
  | _ -> []

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
          Log.debug ("[PACKAGE] Foreign dependency '" ^ name ^ "' found " ^ Int.to_string (List.length inputs) ^ " input files");
          
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

let parse_compiler_config (items : (string * Toml.value) list) : compiler_config =
  (* Parse [profile.debug] and [profile.release] sections *)
  let profile_overrides =
    match List.assoc_opt "profile" items with
    | Some (Toml.Table profile_table) ->
        List.filter_map (fun (profile_name, value) ->
          match value with
          | Toml.Table profile_items ->
              Some (profile_name, Profile.override_from_toml profile_items)
          | _ -> None
        ) profile_table
    | _ -> []
  in
  
  (* Parse [target.macos], [target.linux], etc. sections *)
  let target_overrides =
    match List.assoc_opt "target" items with
    | Some (Toml.Table target_table) ->
        List.filter_map (fun (platform, value) ->
          match value with
          | Toml.Table platform_items ->
              let profile_override = Profile.override_from_toml platform_items in
              Some (platform, { profile_override = Some profile_override })
          | _ -> None
        ) target_table
    | _ -> []
  in
  
  { profile_overrides; target_overrides }

let provider_excluded_relpaths ~(package_path : Path.t) providers =
  let ocaml_source_suffix path_str =
    String.ends_with ~suffix:".ml" path_str
    || String.ends_with ~suffix:".mli" path_str
  in
  let collect_provider_tree rel_path =
    let rel_str = Path.to_string rel_path in
    let provider_parent = Path.dirname rel_path in
    let parent_basename = Path.basename provider_parent in
    let basename = Path.basename rel_path in
    if
      String.equal basename "tusk_fix_rules.ml"
      && String.equal parent_basename "tusk_fix_rules"
    then
      let provider_dir = Path.(package_path / provider_parent) in
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
                | Ok false ->
                    let rel_str = Path.to_string rel_path_full in
                    if ocaml_source_suffix rel_str then [ rel_path_full ] else []
                | Error _ -> [])
              entries
      in
      scan_dir_recursive ~from_dir:provider_dir ~rel_path:provider_parent
    else
      [ rel_path ]
  in
  providers
  |> List.filter_map (fun (provider : Fix_provider.t) ->
         match Path.strip_prefix provider.source_path ~prefix:package_path with
         | Ok rel_path -> Some (collect_provider_tree rel_path)
         | Error _ -> None)
  |> List.concat
  |> List.sort_uniq (fun left right ->
         String.compare (Path.to_string left) (Path.to_string right))

let scan_sources ~(package_path : Path.t) ?(excluded_relpaths = []) () : sources =
  let excluded_relpath_strings =
    excluded_relpaths
    |> List.map Path.to_string
  in
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
            | Ok false ->
                if
                  List.mem (Path.to_string rel_path_full) excluded_relpath_strings
                then []
                else [ rel_path_full ]
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
  let bench_files =
    scan_dir_recursive
      ~from_dir:Path.(package_path / Path.v "bench")
      ~rel_path:(Path.v "bench")
  in
  { src = src_files; tests = test_files; native = native_files; examples = example_files; bench = bench_files }

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

(** Autodiscover benchmark binaries from bench files ending in _bench.ml *)
let autodiscover_bench_binaries (sources : sources) ~(package_path : Path.t) :
    binary list =
  List.filter_map
    (fun bench_file ->
      let filename = Path.basename bench_file in
      if String.ends_with ~suffix:"_bench.ml" filename then
        let binary_name =
          Path.remove_extension (Path.v filename) |> Path.to_string
        in
        (* bench_file is already relative to package (e.g., bench/foo_bench.ml) *)
        Some { name = binary_name; path = bench_file }
      else None)
    sources.bench

let merge_binaries ~(declared : binary list) ~(autodiscovered : binary list) :
    binary list =
  let seen_paths =
    declared |> List.map (fun (bin : binary) -> Path.to_string bin.path)
  in
  let _, discovered =
    List.fold_left
      (fun (seen_paths, acc) (bin : binary) ->
        let path = Path.to_string bin.path in
        if List.mem path seen_paths
        then (seen_paths, acc)
        else (path :: seen_paths, bin :: acc))
      (seen_paths, []) autodiscovered
  in
  declared @ List.rev discovered

let from_toml (toml : Toml.value) ~(workspace_deps : dependency list)
    ~(workspace_dev_deps : dependency list)
    ~(workspace_build_deps : dependency list)
    ~(path : Path.t) ~(relative_path : Path.t) : (t, string) result =
  match toml with
  | Toml.Table items ->
      let fallback_name = Path.basename path in
      let name = parse_name items fallback_name in
      let dependencies =
        parse_dependency_section "dependencies" items ~workspace_deps
      in
      let dev_dependencies =
        parse_dependency_section "dev-dependencies" items
          ~workspace_deps:workspace_dev_deps
      in
      let build_dependencies =
        parse_dependency_section "build-dependencies" items
          ~workspace_deps:workspace_build_deps
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
      let fix_providers =
        Fix_provider.parse_from_toml items ~package_name:name ~package_path:path
      in
      let excluded_relpaths =
        provider_excluded_relpaths ~package_path:path fix_providers
      in
      let sources = scan_sources ~package_path:path ~excluded_relpaths () in
      let compiler = parse_compiler_config items in
      let test_binaries = autodiscover_test_binaries sources ~package_path:path in
      let example_binaries =
        autodiscover_example_binaries sources ~package_path:path
      in
      let bench_binaries = autodiscover_bench_binaries sources ~package_path:path in
      Log.debug ("[PACKAGE] " ^ name ^ ": discovered " ^ Int.to_string (List.length test_binaries) ^ " test binaries from " ^ Int.to_string (List.length sources.tests) ^ " test files");
      Log.debug ("[PACKAGE] " ^ name ^ ": discovered " ^ Int.to_string (List.length example_binaries) ^ " example binaries from " ^ Int.to_string (List.length sources.examples) ^ " example files");
      Log.debug ("[PACKAGE] " ^ name ^ ": discovered " ^ Int.to_string (List.length bench_binaries) ^ " benchmark binaries from " ^ Int.to_string (List.length sources.bench) ^ " benchmark files");
      let all_binaries =
        merge_binaries ~declared:binaries
          ~autodiscovered:(test_binaries @ example_binaries @ bench_binaries)
      in
      
      (* Parse commands using Package_command module *)
      let commands = 
        match List.assoc_opt "command" items with
        | Some (Toml.Array cmd_entries) ->
            Package_command.parse_from_toml cmd_entries ~package_name:name ~package_path:path
        | _ -> []
      in
      Ok
        {
          name;
          path;
          relative_path;
          dependencies;
          dev_dependencies;
          build_dependencies;
          foreign_dependencies = foreign;
          binaries = all_binaries;
          library;
          sources;
          compiler;
          commands;
          fix_providers;
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
  let dev_dependencies_json =
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
         pkg.dev_dependencies)
  in
  let build_dependencies_json =
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
         pkg.build_dependencies)
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
  let fix_providers_json =
    Json.Array (List.map Fix_provider.to_json pkg.fix_providers)
  in
  Json.Object
    [
      ("name", Json.String pkg.name);
      ("path", Json.String (Path.to_string pkg.path));
      ("relative_path", Json.String (Path.to_string pkg.relative_path));
      ("dependencies", dependencies_json);
      ("dev_dependencies", dev_dependencies_json);
      ("build_dependencies", build_dependencies_json);
      ("binaries", binaries_json);
      ("library", library_json);
      ("fix_providers", fix_providers_json);
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
          let parse_dependencies_field field_name =
            match List.assoc_opt field_name fields with
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
          let dependencies = parse_dependencies_field "dependencies" in
          let dev_dependencies = parse_dependencies_field "dev_dependencies" in
          let build_dependencies =
            parse_dependencies_field "build_dependencies"
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
              dev_dependencies;
              build_dependencies;
              foreign_dependencies = [];
              binaries;
              library;
              sources = { src = []; native = []; tests = []; examples = []; bench = [] };
              compiler = { profile_overrides = []; target_overrides = [] };
              commands = [];
              fix_providers = [];
            }
          | Error _, _ -> Error ("Invalid path in package JSON: " ^ path_str)
          | _, Error _ -> Error ("Invalid relative_path in package JSON: " ^ rel_path_str))
      | _ -> Error "Invalid package JSON")
  | _ -> Error "Package must be a JSON object"

(** Hash package metadata into a hasher state *)
let hash state (pkg : t) =
  let module H = Crypto.Sha256 in
  H.write_string state pkg.name;
  
  (* Dependencies metadata *)
  let sorted_deps =
    List.sort (fun (a : dependency) (b : dependency) -> String.compare a.name b.name)
      (build_graph_dependencies pkg)
  in
  List.iter (fun (dep : dependency) ->
    H.write_string state dep.name;
    match dep.source with
    | Workspace -> H.write_string state "workspace"
    | Path path -> H.write_string state (Path.to_string path)
  ) sorted_deps;
  
  (* Binaries metadata *)
  let sorted_bins =
    List.sort (fun (a : binary) (b : binary) -> String.compare a.name b.name) pkg.binaries
  in
  List.iter (fun (bin : binary) ->
    H.write_string state bin.name;
    H.write_string state (Path.to_string bin.path)
  ) sorted_bins;

  let sorted_providers =
    List.sort
      (fun (a : Fix_provider.t) (b : Fix_provider.t) ->
        String.compare a.name b.name)
      pkg.fix_providers
  in
  List.iter
    (fun (provider : Fix_provider.t) ->
      H.write_string state provider.name;
      H.write_string state (Path.to_string provider.source_path);
      List.iter (H.write_string state) provider.rules)
    sorted_providers;
  
  (* Library metadata *)
  (match pkg.library with
  | Some lib ->
      H.write_string state "true";
      H.write_string state (Path.to_string lib.path)
  | None -> H.write_string state "false");
  
  (* Compiler configuration - profile and target overrides *)
  let hash_override (override : profile_override) =
    (match override.kind with
    | Inherit -> H.write_string state "inherit"
    | Override kind -> H.write_string state (match kind with Ocaml_compiler.Bytecode -> "bytecode" | Native -> "native"));
    (match override.inline with
    | Inherit -> H.write_string state "inherit"
    | Override (Some n) -> H.write_string state (Int.to_string n)
    | Override None -> H.write_string state "none");
    (match override.no_assert with
    | Inherit -> H.write_string state "inherit"
    | Override b -> H.write_string state (Bool.to_string b));
    (match override.compact with
    | Inherit -> H.write_string state "inherit"
    | Override b -> H.write_string state (Bool.to_string b));
    (match override.unsafe with
    | Inherit -> H.write_string state "inherit"
    | Override b -> H.write_string state (Bool.to_string b));
    (match override.no_alias_deps with
    | Inherit -> H.write_string state "inherit"
    | Override b -> H.write_string state (Bool.to_string b));
    (match override.open_modules with
    | Inherit -> H.write_string state "inherit"
    | Override mods -> List.iter (H.write_string state) mods);
    (match override.cc_flags with
    | Inherit -> H.write_string state "inherit"
    | Override flags -> List.iter (H.write_string state) flags);
    (match override.ocamlc_flags with
    | Inherit -> H.write_string state "inherit"
    | Override flags -> List.iter (H.write_string state) flags);
  in
  
  let sorted_profile_overrides =
    List.sort (fun (a, _) (b, _) -> String.compare a b) pkg.compiler.profile_overrides
  in
  List.iter (fun (profile_name, override : string * profile_override) ->
    H.write_string state profile_name;
    hash_override override
  ) sorted_profile_overrides;

  let sorted_target_overrides =
    List.sort (fun (a, _) (b, _) -> String.compare a b) pkg.compiler.target_overrides
  in
  List.iter (fun (platform_name, target : string * target_override) ->
    H.write_string state platform_name;
    (match target.profile_override with
    | Some override -> hash_override override
    | None -> H.write_string state "none");
  ) sorted_target_overrides;
  
  (* Source file contents - include explicit [[bin]] entries that may not be in source dirs *)
  let explicit_bin_files =
    List.filter_map
      (fun (bin : binary) ->
        let path_str = Path.to_string bin.path in
        (* Only include if it's a .ml file and not already in sources *)
        if String.ends_with ~suffix:".ml" path_str ||
           String.ends_with ~suffix:".mli" path_str
        then Some bin.path
        else None)
      pkg.binaries
  in
  
  let all_source_files =
    pkg.sources.src 
    @ pkg.sources.native 
    @ pkg.sources.tests
    @ pkg.sources.examples
    @ pkg.sources.bench
    @ explicit_bin_files  (* Include explicit binary sources *)
  in
  let sorted_files =
    List.sort_uniq  (* Use sort_uniq to avoid duplicates *)
      (fun a b -> String.compare (Path.to_string a) (Path.to_string b))
      all_source_files
  in
  List.iter
    (fun file_path ->
      let abs_path =
        if Path.is_absolute file_path then file_path
        else Path.(pkg.path / file_path)
      in
      let path_str = Path.to_string file_path in
      match Fs.read abs_path with
      | Ok content ->
          H.write_string state path_str;
          H.write_string state content
      | Error _ ->
          (* File read error - include path only *)
          H.write_string state path_str)
    sorted_files;
  
  (* Foreign dependency sources *)
  let sorted_foreign_deps =
    List.sort
      (fun (a : foreign_dependency) (b : foreign_dependency) ->
        String.compare a.name b.name)
      pkg.foreign_dependencies
  in
  
  List.iter
    (fun (fdep : foreign_dependency) ->
      H.write_string state fdep.name;
      H.write_string state (Path.to_string fdep.path);
      List.iter (H.write_string state) fdep.build_cmd;
      
      (* Hash all input files *)
      let sorted_inputs =
        List.sort
          (fun a b -> String.compare (Path.to_string a) (Path.to_string b))
          fdep.inputs
      in
      
      List.iter
        (fun input_path ->
          let abs_path = Path.(fdep.path / input_path) in
          match Fs.read abs_path with
          | Ok content ->
              H.write_string state (Path.to_string input_path);
              H.write_string state content
          | Error _ ->
              H.write_string state (Path.to_string input_path))
        sorted_inputs)
    sorted_foreign_deps

module Tests = struct
  let test_parse_dependency_classes () : (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
std = { workspace = true }

[dev-dependencies]
propane = { workspace = true }

[build-dependencies]
fixme = { path = "../fixme" }
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let workspace_dep name = { name; source = Workspace } in
    let pkg =
      from_toml toml ~workspace_deps:[ workspace_dep "std" ]
        ~workspace_dev_deps:[ workspace_dep "propane" ]
        ~workspace_build_deps:[]
        ~path:(Path.v "/tmp/example")
        ~relative_path:(Path.v "packages/example")
      |> Result.expect ~msg:"expected package manifest"
    in
    if
      List.map (fun (dep : dependency) -> dep.name) pkg.dependencies = [ "std" ]
      && List.map (fun (dep : dependency) -> dep.name) pkg.dev_dependencies
         = [ "propane" ]
      && List.map (fun (dep : dependency) -> dep.name) pkg.build_dependencies
         = [ "fixme" ]
    then Ok ()
    else Error "expected dependency classes to round-trip"
  [@test]

  let test_build_graph_dependencies_exclude_build_only_deps () :
      (unit, string) result =
    let pkg =
      {
        name = "example";
        path = Path.v "/tmp/example";
        relative_path = Path.v "packages/example";
        dependencies = [ { name = "std"; source = Workspace } ];
        dev_dependencies = [ { name = "propane"; source = Workspace } ];
        build_dependencies = [ { name = "fixme"; source = Workspace } ];
        foreign_dependencies = [];
        binaries = [];
        library = None;
        sources =
          { src = []; native = []; tests = []; examples = []; bench = [] };
        compiler = { profile_overrides = []; target_overrides = [] };
        commands = [];
        fix_providers = [];
      }
    in
    let build_graph =
      build_graph_dependencies pkg
      |> List.map (fun (dep : dependency) -> dep.name)
    in
    let all =
      all_dependencies pkg |> List.map (fun (dep : dependency) -> dep.name)
    in
    if
      build_graph = [ "std"; "propane" ]
      && all = [ "std"; "propane"; "fixme" ]
    then Ok ()
    else Error "expected build graph dependencies to exclude build-only deps"
  [@test]
end
