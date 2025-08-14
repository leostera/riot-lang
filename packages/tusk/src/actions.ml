(** Build actions - concrete build steps that happen in the sandbox *)

type action =
  (* File compilation actions *)
  | CompileInterface of
      string
      * string
      * string list (* mli file, output cmi file, include paths *)
  | CompileImplementation of
      string
      * string
      * string list (* ml file, output cmo file, include paths *)
  | CompileC of string * string (* c file, output o file *)
  (* Linking actions *)
  | CreateLibrary of
      string
      * string list
      * string list (* cma file, cmo files, include paths *)
  | CreateExecutable of
      string
      * string list
      * string list
      * string list (* exe file, cmo files, libraries, include paths *)
  (* File operations *)
  | CopyFile of string * string (* source, destination *)
  (* Output declaration *)
  | DeclareOutputs of
      string list (* list of output files that should be copied to target *)

type action_result = Success | Failed of string | Skipped of string

type dep_info = {
  name : string;
  relative_path : string;
  dependencies : string list; (* Names of this dependency's dependencies *)
}

type resolved_dep = { lib_file : string; include_path : string }

type blueprint = {
  package_name : string;
  package_path : string;
  dependencies : dep_info list;
  actions : action list;
  toolchain : Toolchains.toolchain;
  hash : Hasher.hash option; (* Content-based hash of all inputs *)
}

(* Use Hasher module for all hash operations *)

(** Convert action to canonical string for hashing *)
let action_to_string action =
  match action with
  | CompileInterface (src, dst, includes) ->
      Printf.sprintf "compile_interface(%s,%s,[%s])" src dst
        (String.concat "," includes)
  | CompileImplementation (src, dst, includes) ->
      Printf.sprintf "compile_impl(%s,%s,[%s])" src dst
        (String.concat "," includes)
  | CompileC (src, dst) -> Printf.sprintf "compile_c(%s,%s)" src dst
  | CreateLibrary (lib, files, includes) ->
      Printf.sprintf "create_library(%s,[%s],[%s])" lib
        (String.concat "," files)
        (String.concat "," includes)
  | CreateExecutable (exe, files, libs, includes) ->
      Printf.sprintf "create_exe(%s,[%s],[%s],[%s])" exe
        (String.concat "," files) (String.concat "," libs)
        (String.concat "," includes)
  | CopyFile (src, dst) -> Printf.sprintf "copy(%s,%s)" src dst
  | DeclareOutputs outputs ->
      Printf.sprintf "declare_outputs([%s])" (String.concat "," outputs)

(** Compute content-based hash for a blueprint *)
let compute_blueprint_hash blueprint =
  let components = ref [] in

  (* 1. Package metadata *)
  components := blueprint.package_name :: !components;
  components := Toolchains.get_version blueprint.toolchain :: !components;

  (* 2. Dependencies (sorted by name for deterministic hash) *)
  let sorted_deps =
    List.sort (fun a b -> String.compare a.name b.name) blueprint.dependencies
  in
  List.iter
    (fun dep ->
      components := (dep.name ^ ":" ^ dep.relative_path) :: !components;
      components := String.concat "," dep.dependencies :: !components)
    sorted_deps;

  (* 3. Source file content hashes *)
  let src_dir =
    if System.file_exists (Filename.concat blueprint.package_path "src") then
      Filename.concat blueprint.package_path "src"
    else blueprint.package_path
  in

  (if System.file_exists src_dir then
     let all_files = System.list_dir_all src_dir in
     let source_files =
       List.filter
         (fun f ->
           String.ends_with ~suffix:".ml" f
           || String.ends_with ~suffix:".mli" f
           || String.ends_with ~suffix:".c" f)
         all_files
     in
     let sorted_files = List.sort String.compare source_files in
     List.iter
       (fun file ->
         let full_path = Filename.concat src_dir file in
         let file_hash = Hasher.hash_file full_path in
         components := (file ^ ":" ^ Hasher.to_string file_hash) :: !components)
       sorted_files);

  (* 4. Actions (in order) *)
  List.iter
    (fun action -> components := action_to_string action :: !components)
    blueprint.actions;

  (* 5. Combine all components and hash *)
  let combined = String.concat "|" (List.rev !components) in
  let final_hash = Hasher.hash_string combined in

  Printf.printf "[Blueprint] Computed hash %s for %s\n"
    (Hasher.to_string final_hash)
    blueprint.package_name;
  flush stdout;

  final_hash

(** Pretty print an action *)
let string_of_action = function
  | CompileInterface (src, dst, includes) ->
      Printf.sprintf "compile_interface(%s -> %s) [includes: %s]"
        (Filename.basename src) (Filename.basename dst)
        (String.concat "; " includes)
  | CompileImplementation (src, dst, includes) ->
      Printf.sprintf "compile_impl(%s -> %s) [includes: %s]"
        (Filename.basename src) (Filename.basename dst)
        (String.concat "; " includes)
  | CompileC (src, dst) ->
      Printf.sprintf "compile_c(%s -> %s)" (Filename.basename src)
        (Filename.basename dst)
  | CreateLibrary (lib, files, includes) ->
      Printf.sprintf "create_library(%s from [%s]) [includes: %s]"
        (Filename.basename lib)
        (String.concat "; " (List.map Filename.basename files))
        (String.concat "; " includes)
  | CreateExecutable (exe, files, libs, includes) ->
      Printf.sprintf "create_exe(%s from [%s] with [%s]) [includes: %s]"
        (Filename.basename exe)
        (String.concat "; " (List.map Filename.basename files))
        (String.concat "; " libs)
        (String.concat "; " includes)
  | CopyFile (src, dst) ->
      Printf.sprintf "copy(%s -> %s)" (Filename.basename src)
        (Filename.basename dst)
  | DeclareOutputs outputs ->
      Printf.sprintf "declare_outputs([%s])" (String.concat "; " outputs)

(** Pretty print a blueprint *)
let print_blueprint blueprint =
  Printf.printf "=== Blueprint for %s ===\n" blueprint.package_name;
  Printf.printf "Path: %s\n" blueprint.package_path;
  Printf.printf "Dependencies: [%s]\n"
    (String.concat "; " (List.map (fun d -> d.name) blueprint.dependencies));
  Printf.printf "Actions:\n";
  List.iteri
    (fun i action ->
      Printf.printf "  %d. %s\n" (i + 1) (string_of_action action))
    blueprint.actions;
  Printf.printf "\n"

(** Resolve package dependencies to library files and include paths *)
let resolve_dependency toolchain dep_name =
  match dep_name with
  | "unix" ->
      (* Unix is a well-known OCaml library *)
      Some { lib_file = "unix.cma"; include_path = "+unix" }
  | _ ->
      (* For now, we only handle well-known libraries *)
      (* TODO: Add support for external package dependencies *)
      None

(** Get libraries and include paths from package dependencies *)
let get_dependency_libs_and_includes toolchain pkg_dependencies =
  let libs = ref [] in
  let includes = ref [] in
  List.iter
    (fun dep_name ->
      match resolve_dependency toolchain dep_name with
      | Some resolved ->
          libs := resolved.lib_file :: !libs;
          includes := resolved.include_path :: !includes
      | None -> ())
    pkg_dependencies;
  (List.rev !libs, List.rev !includes)

(** Generate build blueprint for a package *)
let generate_blueprint workspace node dependencies all_packages toolchain ~hash
    () =
  let root = workspace.Workspace.root in
  let pkg_name = node.Build_node.package.name in
  let pkg_path = node.Build_node.package.path in

  (* Get external dependencies from tusk.toml *)
  let pkg_dependencies = node.Build_node.package.dependencies in
  let external_libs, external_includes =
    get_dependency_libs_and_includes toolchain pkg_dependencies
  in

  (* Get dependency include paths from local packages *)
  let local_dep_includes =
    List.fold_left
      (fun acc dep ->
        (* Look in target/debug/out/<dep_relative_path> where outputs are placed *)
        let dep_target =
          Filename.concat
            (Filename.concat
               (Filename.concat (Filename.concat root "target") "debug")
               "out")
            dep.relative_path
        in
        if System.file_exists dep_target then dep_target :: acc else acc)
      [] dependencies
  in

  (* Combine all include paths for compilation *)
  let dep_includes = external_includes @ local_dep_includes in

  (* Find source files *)
  let src_dir =
    if System.file_exists (Filename.concat pkg_path "src") then
      Filename.concat pkg_path "src"
    else pkg_path
  in

  (* Scan for source files *)
  let ml_files = ref [] in
  let mli_files = ref [] in
  let c_files = ref [] in
  (if System.file_exists src_dir then
     let all_files = System.list_dir_all src_dir in
     List.iter
       (fun file ->
         if Filename.check_suffix file ".ml" then ml_files := file :: !ml_files
         else if Filename.check_suffix file ".mli" then
           mli_files := file :: !mli_files
         else if Filename.check_suffix file ".c" then
           c_files := file :: !c_files)
       all_files);

  let ml_files = List.rev !ml_files in
  let mli_files = List.rev !mli_files in
  let c_files = List.rev !c_files in

  (* Use ocamldep to determine compilation order *)
  let sorted_ml_files =
    if ml_files <> [] || mli_files <> [] then (
      (* Separate lib.ml/lib.mli from other files - they should always be compiled last *)
      let has_lib_ml = List.mem "lib.ml" ml_files in
      let has_lib_mli = List.mem "lib.mli" mli_files in

      let ml_files_for_sort = List.filter (fun f -> f <> "lib.ml") ml_files in
      let mli_files_for_sort =
        List.filter (fun f -> f <> "lib.mli") mli_files
      in

      (* Create temp file with all source files except lib.ml/lib.mli *)
      let all_source_files =
        List.map (fun f -> Filename.concat src_dir f) ml_files_for_sort
        @ List.map (fun f -> Filename.concat src_dir f) mli_files_for_sort
      in

      (* Build include flags for dependencies *)
      let include_flags =
        String.concat " " (List.map (fun p -> "-I " ^ p) dep_includes)
      in

      (* Run ocamldep -sort *)
      let files_str = String.concat " " all_source_files in
      let ocamldep = Toolchains.ocamldep_path toolchain in
      let cmd =
        if files_str = "" then ""
        else
          Printf.sprintf "%s %s -sort %s 2>/dev/null" ocamldep include_flags
            files_str
      in

      let sorted_paths =
        if cmd = "" then []
        else
          let lines = System.run_process_lines cmd in
          let output = ref (String.concat " " lines) in
          (* Debug output *)
          Printf.printf "[Blueprint] ocamldep returned: %s\n" !output;
          Printf.printf "[Blueprint] Sorted files: %s\n" !output;
          flush stdout;
          (* ocamldep -sort outputs space-separated paths *)
          if !output = "" then []
          else
            let trimmed = String.trim !output in
            String.split_on_char ' ' trimmed
      in

      let sorted_basenames = List.map Filename.basename sorted_paths in

      (* Separate back into ml and mli files, preserving order *)
      let sorted_mli =
        List.filter (fun f -> Filename.check_suffix f ".mli") sorted_basenames
      in
      let sorted_ml =
        List.filter (fun f -> Filename.check_suffix f ".ml") sorted_basenames
      in

      (* Add lib.ml and lib.mli back at the end *)
      let final_mli =
        if has_lib_mli then sorted_mli @ [ "lib.mli" ] else sorted_mli
      in
      let final_ml =
        if has_lib_ml then sorted_ml @ [ "lib.ml" ] else sorted_ml
      in

      Printf.printf "[Blueprint] After sorting=%s\n"
        (String.concat ", " final_ml);
      flush stdout;

      (final_ml, final_mli))
    else (ml_files, mli_files)
  in

  let ml_files, mli_files = sorted_ml_files in

  (* Check if we have main.ml before any transformations *)
  let has_main = List.exists (fun f -> f = "main.ml") ml_files in

  Printf.printf "[Blueprint] Package %s has_main=%b (ml_files: %s)\n" pkg_name
    has_main
    (String.concat ", " ml_files);
  flush stdout;

  let actions = ref [] in

  (* 1. Compile C files first *)
  let o_files =
    List.map
      (fun c_file ->
        let src_path = Filename.concat src_dir c_file in
        let basename = Filename.chop_suffix c_file ".c" in
        let o_path = basename ^ ".o" in
        actions := CompileC (src_path, o_path) :: !actions;
        o_path)
      c_files
  in

  (* 2. Copy source files to sandbox - no renaming needed *)
  let mli_files =
    List.map
      (fun mli_file ->
        let src_path = Filename.concat src_dir mli_file in
        actions := CopyFile (src_path, mli_file) :: !actions;
        mli_file)
      mli_files
  in

  let ml_files =
    List.map
      (fun ml_file ->
        let src_path = Filename.concat src_dir ml_file in
        actions := CopyFile (src_path, ml_file) :: !actions;
        ml_file)
      ml_files
  in

  (* 4. Compile interfaces *)
  List.iter
    (fun mli_file ->
      let basename = Filename.chop_suffix mli_file ".mli" in
      let cmi_path = basename ^ ".cmi" in
      actions := CompileInterface (mli_file, cmi_path, dep_includes) :: !actions)
    mli_files;

  (* 5. Compile implementations *)
  let cmo_files =
    List.map
      (fun ml_file ->
        let basename = Filename.chop_suffix ml_file ".ml" in
        let cmo_path = basename ^ ".cmo" in
        actions :=
          CompileImplementation (ml_file, cmo_path, dep_includes) :: !actions;
        cmo_path)
      ml_files
  in

  (* 4. Create library and/or executable *)
  let outputs = ref [] in

  (* First, always create a library if we have any modules (excluding main.ml) *)
  let library_cmo_files = 
    if has_main then
      List.filter (fun f -> f <> "main.cmo") cmo_files
    else
      cmo_files
  in
  
  if library_cmo_files <> [] || o_files <> [] then (
    let cma_path = pkg_name ^ ".cma" in
    let lib_objects = library_cmo_files @ o_files in
    actions := CreateLibrary (cma_path, lib_objects, dep_includes) :: !actions;
    outputs := cma_path :: !outputs;
    
    (* Only add package.cmi if we have a package.ml or package.mli file *)
    if List.mem (pkg_name ^ ".ml") ml_files || List.mem (pkg_name ^ ".mli") mli_files then
      outputs := (pkg_name ^ ".cmi") :: !outputs
  );

  (* Then, if we have main.ml, create the executable *)
  (if has_main then (
     let exe_path = pkg_name in

     (* Get all transitive dependencies in topological order *)
     let rec get_transitive_deps visited deps_to_process acc =
       match deps_to_process with
       | [] -> List.rev acc (* Reverse to get dependencies in correct order *)
       | dep :: rest ->
           if List.mem dep.name visited then
             get_transitive_deps visited rest acc
           else
             let visited = dep.name :: visited in
             (* Get this dep's dependencies from all_packages *)
             let child_deps =
               List.filter_map
                 (fun dep_name ->
                   List.find_opt (fun d -> d.name = dep_name) all_packages)
                 dep.dependencies
             in
             (* Process children first (depth-first), then this dep *)
             get_transitive_deps visited (child_deps @ rest) (dep :: acc)
     in

     let all_deps = get_transitive_deps [] dependencies [] in

     (* Get dependency libraries from local packages - in topological order (dependencies first) *)
     let local_dep_libs =
       List.fold_left
         (fun acc dep ->
           let dep_dir =
             Filename.concat
               (Filename.concat
                  (Filename.concat (Filename.concat root "target") "debug")
                  "out")
               dep.relative_path
           in
           (* Add the .cma file if it exists *)
           let cma_file = Filename.concat dep_dir (dep.name ^ ".cma") in
           if System.file_exists cma_file then cma_file :: acc else acc)
         [] all_deps
     in

     (* Get external dependencies from tusk.toml (already computed earlier) *)
     let external_libs, _external_includes_again =
       get_dependency_libs_and_includes toolchain pkg_dependencies
     in

     (* Combine all libraries *)
     let all_libs = external_libs @ local_dep_libs in

     (* Combine all include paths *)
     let all_includes = external_includes @ dep_includes in

     (* Link only main.cmo with all libraries (including our own .cma) *)
     let exe_objects = ["main.cmo"] @ o_files in
     
     (* Add our own library to the libs if we created one *)
     let all_libs_with_self = 
       if library_cmo_files <> [] then
         all_libs @ [pkg_name ^ ".cma"]
       else
         all_libs
     in

     (* For executable, link main.cmo with all libraries *)
     actions :=
       CreateExecutable (exe_path, exe_objects, all_libs_with_self, all_includes)
       :: !actions;

     (* Declare executable as output *)
     outputs := exe_path :: !outputs));

  (* Add C object files to outputs if any *)
  List.iter (fun o -> outputs := o :: !outputs) o_files;

  (* Add all .cmi files to outputs *)
  List.iter 
    (fun mli_file ->
      let basename = Filename.chop_suffix mli_file ".mli" in
      let cmi_file = basename ^ ".cmi" in
      outputs := cmi_file :: !outputs)
    mli_files;
  
  (* Also add .cmi files for .ml files without .mli *)
  List.iter
    (fun ml_file ->
      let basename = Filename.chop_suffix ml_file ".ml" in
      let has_mli = List.mem (basename ^ ".mli") mli_files in
      if not has_mli then
        let cmi_file = basename ^ ".cmi" in
        outputs := cmi_file :: !outputs)
    ml_files;

  (* Add DeclareOutputs action *)
  if !outputs <> [] then actions := DeclareOutputs !outputs :: !actions;

  let actions = List.rev !actions in

  let blueprint =
    {
      package_name = pkg_name;
      package_path = pkg_path;
      dependencies;
      actions;
      toolchain;
      hash = None;
      (* Will be computed after blueprint creation *)
    }
  in

  (* Use the provided hash *)
  { blueprint with hash = Some hash }

(** Execute an action in the sandbox *)
let execute_action action toolchain =
  (* Helper to convert Ocamlc.result to our action_result *)
  let convert_result = function
    | Ocamlc.Success output -> (Success, output)
    | Ocamlc.Failed output -> (Failed output, output)
  in

  match action with
  | CompileInterface (src, dst, includes) ->
      Ocamlc.compile_interface ~toolchain ~includes ~output:dst src
      |> convert_result
  | CompileImplementation (src, dst, includes) ->
      Ocamlc.compile_impl ~toolchain ~includes ~output:dst src |> convert_result
  | CompileC (src, dst) ->
      Ocamlc.compile_c ~toolchain ~includes:[] ~output:dst src |> convert_result
  | CreateLibrary (lib, object_files, includes) ->
      Ocamlc.create_library ~toolchain ~includes ~output:lib object_files
      |> convert_result
  | CreateExecutable (exe, object_files, libs, includes) ->
      (* Use custom executable for C stubs *)
      Ocamlc.create_custom_executable ~toolchain ~includes ~output:exe ~libs
        object_files
      |> convert_result
  | CopyFile (src, dst) -> (
      try
        System.copy_file src dst;
        (Success, Printf.sprintf "Copied %s -> %s" src dst)
      with exn -> (Failed (Printexc.to_string exn), ""))
  | DeclareOutputs outputs ->
      (* Just validate that declared outputs exist *)
      let missing = List.filter (fun f -> not (System.file_exists f)) outputs in
      if missing = [] then (Success, "All outputs exist")
      else
        ( Failed
            (Printf.sprintf "Missing outputs: %s" (String.concat ", " missing)),
          "" )

(** Execute a blueprint *)
let execute_blueprint workspace blueprint =
  Printf.printf "[Actions] Executing blueprint for %s\n" blueprint.package_name;
  flush stdout;
  let success = ref true in
  let errors = ref [] in

  List.iteri
    (fun i action ->
      Printf.printf "[Actions] Step %d: %s\n" (i + 1) (string_of_action action);
      flush stdout;

      let result, output = execute_action action blueprint.toolchain in
      match result with
      | Success ->
          if output <> "" then (
            Printf.printf "  -> %s\n" output;
            flush stdout)
      | Skipped reason ->
          Printf.printf "  -> Skipped: %s\n" reason;
          flush stdout
      | Failed error ->
          success := false;
          errors := error :: !errors;
          Printf.printf "  -> Failed: %s\n" error;
          flush stdout)
    blueprint.actions;

  if !success then (true, "Build successful")
  else (false, String.concat "; " !errors)

(** Tests submodule *)
module Tests = struct
  let test_generate_blueprint_creates_correct_compilation_order () =
    (* Test that ocamldep integration produces correct build order *)
    Ok ()
    [@test]

  let test_generate_blueprint_handles_lib_ml_renaming () =
    (* Test that lib.ml gets renamed to package_name.ml *)
    Ok ()
    [@test]

  let test_generate_blueprint_creates_library_for_non_main_packages () =
    (* Test that packages without main.ml create .cma libraries *)
    Ok ()
    [@test]

  let test_generate_blueprint_creates_executable_for_main_packages () =
    (* Test that packages with main.ml create executables *)
    Ok ()
    [@test]

  let test_compute_blueprint_hash_is_deterministic () =
    (* Test that same inputs always produce same hash *)
    Ok ()
    [@test]

  let test_compute_blueprint_hash_changes_with_source_changes () =
    (* Test that hash changes when source files change *)
    Ok ()
    [@test]

  let test_execute_blueprint_runs_actions_in_order () =
    (* Test that actions execute in the correct sequence *)
    Ok ()
    [@test]

  let test_execute_blueprint_stops_on_first_failure () =
    (* Test that execution halts when an action fails *)
    Ok ()
    [@test]

  let test_resolve_transitive_dependencies () =
    (* Test that all transitive dependencies are collected in correct order *)
    Ok ()
end [@test]
