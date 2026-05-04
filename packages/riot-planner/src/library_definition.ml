open Std
open Std.Collections
open Riot_model

(**
   Library Definition - Analyzes directory contents to determine library
   structure

   A library is a directory containing source files that gets compiled into a
   namespace. This module handles the complex logic of:

   1. Identifying child modules (files and subdirectories) 2. Detecting whether
   the library has a concrete interface (lib.ml/lib.mli) 3. Filtering out
   binary sources and the library interface itself 4. Determining which
   children should be dependencies of the library interface

   Key edge cases handled:
   - Binary files must be excluded from library compilation
   - Library interface files (dir/dir.ml, dir/dir.mli) must be excluded from
     children
   - Subdirectories are only included if they don't have a corresponding file
   - Binary path comparison requires converting absolute paths to relative
     paths
*)
type t = {
  library_module_name: string;
  child_files: Module.t list;
  child_dirs: Module.t list;
  child_modules: Module.t list;
  has_concrete_ml: bool;
  has_concrete_mli: bool;
  concrete_ml_path: Path.t option;
  concrete_mli_path: Path.t option;
  children_without_lib: Module_scanner.entry list;
}

(**
   Convert an absolute path to be relative to a base path.

   Example: base = "/home/user/project" path = "/home/user/project/src/main.ml"
   result = "src/main.ml"

   If path doesn't start with base, returns path unchanged.
*)
let make_relative = fun ~base ~path ->
  let base_str = Path.to_string base in
  let path_str = Path.to_string path in
  let prefix = base_str ^ "/" in
  if String.starts_with ~prefix path_str then
    let len = String.length prefix in
    Path.v (String.sub path_str ~offset:len ~len:(String.length path_str - len))
  else
    path

(**
   Check if a path is a binary source file.

   Binary paths in Package.binary are stored as ABSOLUTE paths (e.g.,
   /full/path/packages/riot/src/main.ml), while scanned file paths are RELATIVE
   to package root (e.g., src/main.ml).

   We must convert both to relative paths before comparing to avoid:
   - False matches: src/main.ml shouldn't match some/other/src/main.ml
   - Basename collisions: a.ml binary shouldn't exclude src/utils/a.ml
*)
let is_binary_module = fun ~package_path ~binaries path ->
  let bin_rel = make_relative ~base:package_path ~path in
  List.any
    binaries
    ~fn:(fun (bin: Package.binary) ->
      let bin_abs_rel = make_relative ~base:package_path ~path:bin.path in
      Path.equal bin_rel bin_abs_rel)

(**
   Analyze directory entries to build a library definition.

   This function implements the core library analysis algorithm:

   1. CHILD FILES: Scan for .ml/.mli files, excluding:
   - The library interface itself (e.g., in dir "foo", exclude foo.ml/foo.mli)
   - Binary source files (declared in Package.binary list)

   2. CHILD DIRECTORIES: Scan for subdirectories, but only include them if:
   - There's NO file with the same module name (file takes precedence)
   - Example: if dir/bar.ml exists, ignore dir/bar/ subdirectory

   3. LIBRARY INTERFACE: Check if the library has concrete interface files:
   - foo/foo.ml - concrete implementation
   - foo/foo.mli - concrete interface If these don't exist, they will be
     GENERATED

   4. DEPENDENCIES: Determine which children the library interface depends on:
   - If concrete: only depend on child_files (avoid cycles with subdirs)
   - If generated: depend on all child_modules (must reference subdirs)

   Edge case: Module names are case-insensitive on some filesystems but
   case-sensitive in OCaml, so we use Module_name.to_string for comparison.
*)
let from_entries = fun
  ~namespace ~library_name ~package_path ~concrete_library_path ~binaries children ->
  let library_module_name =
    Module_name.from_string library_name
    |> Module_name.to_string
  in
  let explicit_concrete_ml_path =
    match concrete_library_path with
    | Some path when Path.extension path = Some ".ml" ->
        Some (make_relative ~base:package_path ~path)
    | _ -> None
  in
  let explicit_concrete_mli_path =
    match concrete_library_path with
    | Some path when Path.extension path = Some ".mli" ->
        Some (make_relative ~base:package_path ~path)
    | Some path when Path.extension path = Some ".ml" ->
        Some (
          make_relative ~base:package_path ~path
          |> Path.replace_extension ~ext:"mli"
        )
    | _ -> None
  in
  let path_matches expected actual =
    match expected with
    | Some expected ->
        let normalize_for_library_match path =
          path
          |> Path.normalize
          |> Path.to_string
          |> String.lowercase_ascii
        in
        Path.equal expected actual
        || String.equal (normalize_for_library_match expected) (normalize_for_library_match actual)
    | None -> false
  in
  let is_explicit_library_path path =
    path_matches explicit_concrete_ml_path path || path_matches explicit_concrete_mli_path path
  in
  let is_library_source_file ~file_module_name ~path =
    file_module_name = library_module_name || is_explicit_library_path path
  in
  let child_files =
    List.filter_map
      children
      ~fn:(fun e ->
        match e with
        | Module_scanner.ML (n, p)
        | Module_scanner.MLI (n, p) ->
            let file_module_name =
              Path.remove_extension (Path.v n)
              |> Path.to_string
              |> Module_name.from_string
              |> Module_name.to_string
            in
            if is_library_source_file ~file_module_name ~path:p then
              None
            else if not (is_binary_module ~package_path ~binaries p) then
              Some (Module.make ~namespace ~filename:p)
            else
              None
        | _ -> None)
  in
  let child_dirs =
    List.filter_map
      children
      ~fn:(fun e ->
        match e with
        | Module_scanner.Dir (n, p, _) ->
            let module_name = Module_name.from_string n in
            let module_name_str = Module_name.to_string module_name in
            let has_file =
              List.any
                child_files
                ~fn:(fun m -> Module_name.to_string (Module.module_name m) = module_name_str)
            in
            if has_file then
              None
            else
              Some (Module.make ~namespace ~filename:Path.(p / Path.v (n ^ ".ml")))
        | _ -> None)
  in
  let child_modules = child_files @ child_dirs in
  let concrete_ml_path =
    match List.find
      children
      ~fn:(fun e ->
        match e with
        | Module_scanner.ML (n, p) ->
            let file_module_name =
              Path.remove_extension (Path.v n)
              |> Path.to_string
              |> Module_name.from_string
              |> Module_name.to_string
            in
            (
              match explicit_concrete_ml_path with
              | Some explicit_path -> path_matches (Some explicit_path) p
              | None -> file_module_name = library_module_name
            )
        | _ -> false) with
    | Some (Module_scanner.ML (_, p)) -> Some p
    | _ -> None
  in
  let concrete_mli_path =
    match List.find
      children
      ~fn:(fun e ->
        match e with
        | Module_scanner.MLI (n, p) ->
            let file_module_name =
              Path.remove_extension (Path.v n)
              |> Path.to_string
              |> Module_name.from_string
              |> Module_name.to_string
            in
            (
              match explicit_concrete_mli_path with
              | Some explicit_path -> path_matches (Some explicit_path) p
              | None -> file_module_name = library_module_name
            )
        | _ -> false) with
    | Some (Module_scanner.MLI (_, p)) -> Some p
    | _ -> None
  in
  let has_concrete_ml = concrete_ml_path != None in
  let has_concrete_mli = concrete_mli_path != None in
  let children_without_lib =
    List.filter
      children
      ~fn:(fun e ->
        match e with
        | Module_scanner.ML (n, p)
        | Module_scanner.MLI (n, p) ->
            let file_module_name =
              Path.remove_extension (Path.v n)
              |> Path.to_string
              |> Module_name.from_string
              |> Module_name.to_string
            in
            not (is_library_source_file ~file_module_name ~path:p)
        | _ -> true)
  in
  {
    library_module_name;
    child_files;
    child_dirs;
    child_modules;
    has_concrete_ml;
    has_concrete_mli;
    concrete_ml_path;
    concrete_mli_path;
    children_without_lib;
  }

let library_module_name = fun t -> t.library_module_name

let child_files = fun t -> t.child_files

let child_dirs = fun t -> t.child_dirs

let child_modules = fun t -> t.child_modules

let has_concrete_ml = fun t -> t.has_concrete_ml

let has_concrete_mli = fun t -> t.has_concrete_mli

let concrete_ml_path = fun t -> t.concrete_ml_path

let concrete_mli_path = fun t -> t.concrete_mli_path

let children_without_lib = fun t -> t.children_without_lib

(**
   Determine which children the library interface should depend on.

   CONCRETE library interfaces (user-written foo.ml/foo.mli):
   - Only depend on child_files in the same directory
   - Avoids cycles: sublibrary foo/bar/ creates foo__bar.ml which the concrete
     foo.ml shouldn't depend on (would create foo.ml -> foo__bar.ml -> foo.ml)

   GENERATED library interfaces (auto-generated from directory structure):
   - Depend on ALL child_modules (files + subdirectories)
   - Must reference sublibraries: "module Bar = Foo__Bar"
   - Safe from cycles because generated content is explicit
*)
let deps_for_library_interface = fun t ->
  if t.has_concrete_ml && t.has_concrete_mli then
    t.child_files
  else
    t.child_modules
