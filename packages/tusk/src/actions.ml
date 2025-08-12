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

type blueprint = {
  package_name : string;
  package_path : string;
  dependencies : dep_info list;
  actions : action list;
  toolchain_version : string;
  hash : Hasher.hash option; (* Content-based hash of all inputs *)
}

(* Use Hasher module for all hash operations *)

(** Convert action to canonical string for hashing *)
let action_to_string action =
  match action with
  | CompileInterface (src, dst, includes) ->
      Printf.sprintf "compile_interface(%s,%s,[%s])" src dst (String.concat "," includes)
  | CompileImplementation (src, dst, includes) ->
      Printf.sprintf "compile_impl(%s,%s,[%s])" src dst (String.concat "," includes)  
  | CompileC (src, dst) ->
      Printf.sprintf "compile_c(%s,%s)" src dst
  | CreateLibrary (lib, files, includes) ->
      Printf.sprintf "create_library(%s,[%s],[%s])" lib (String.concat "," files) (String.concat "," includes)
  | CreateExecutable (exe, files, libs, includes) ->
      Printf.sprintf "create_exe(%s,[%s],[%s],[%s])" exe (String.concat "," files) (String.concat "," libs) (String.concat "," includes)
  | CopyFile (src, dst) ->
      Printf.sprintf "copy(%s,%s)" src dst
  | DeclareOutputs outputs ->
      Printf.sprintf "declare_outputs([%s])" (String.concat "," outputs)

(** Compute content-based hash for a blueprint *)
let compute_blueprint_hash blueprint =
  let components = ref [] in
  
  (* 1. Package metadata *)
  components := blueprint.package_name :: !components;
  components := blueprint.toolchain_version :: !components;
  
  (* 2. Dependencies (sorted by name for deterministic hash) *)
  let sorted_deps = List.sort (fun a b -> String.compare a.name b.name) blueprint.dependencies in
  List.iter (fun dep ->
    components := (dep.name ^ ":" ^ dep.relative_path) :: !components;
    components := String.concat "," dep.dependencies :: !components;
  ) sorted_deps;
  
  (* 3. Source file content hashes *)
  let src_dir = 
    if System.file_exists (Filename.concat blueprint.package_path "src") then
      Filename.concat blueprint.package_path "src"
    else blueprint.package_path
  in
  
  if System.file_exists src_dir then (
    let all_files = System.list_dir_all src_dir in
    let source_files = List.filter (fun f -> 
      String.ends_with ~suffix:".ml" f || 
      String.ends_with ~suffix:".mli" f ||
      String.ends_with ~suffix:".c" f
    ) all_files in
    let sorted_files = List.sort String.compare source_files in
    List.iter (fun file ->
      let full_path = Filename.concat src_dir file in
      let file_hash = Hasher.hash_file full_path in
      components := (file ^ ":" ^ Hasher.to_string file_hash) :: !components;
    ) sorted_files;
  );
  
  (* 4. Actions (in order) *)
  List.iter (fun action ->
    components := action_to_string action :: !components;
  ) blueprint.actions;
  
  (* 5. Combine all components and hash *)
  let combined = String.concat "|" (List.rev !components) in
  let final_hash = Hasher.hash_string combined in
  
  Printf.printf "[Blueprint] Computed hash %s for %s\n" (Hasher.to_string final_hash) blueprint.package_name;
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

(** Generate build blueprint for a package *)
let generate_blueprint root pkg_name pkg_path pkg_relative_path dependencies
    all_packages toolchain_version ~hash () =
  (* Get dependency include paths *)
  let dep_includes =
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
      let mli_files_for_sort = List.filter (fun f -> f <> "lib.mli") mli_files in
      
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
      let ocamldep = Toolchains.ocamldep_path toolchain_version in
      let cmd =
        if files_str = "" then ""
        else
          Printf.sprintf "%s -I +unix %s -sort %s 2>/dev/null" ocamldep
            include_flags files_str
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
      let final_mli = if has_lib_mli then sorted_mli @ ["lib.mli"] else sorted_mli in
      let final_ml = if has_lib_ml then sorted_ml @ ["lib.ml"] else sorted_ml in

      Printf.printf "[Blueprint] After sorting: sorted_ml=%s\n"
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

  (* 2. Copy source files to sandbox, renaming lib files if needed *)
  let mli_files =
    List.map
      (fun mli_file ->
        if mli_file = "lib.mli" then (
          let src_path = Filename.concat src_dir mli_file in
          let dst_file = pkg_name ^ ".mli" in
          actions := CopyFile (src_path, dst_file) :: !actions;
          dst_file)
        else
          let src_path = Filename.concat src_dir mli_file in
          actions := CopyFile (src_path, mli_file) :: !actions;
          mli_file)
      mli_files
  in

  let ml_files =
    List.map
      (fun ml_file ->
        if ml_file = "lib.ml" then (
          let src_path = Filename.concat src_dir ml_file in
          let dst_file = pkg_name ^ ".ml" in
          actions := CopyFile (src_path, dst_file) :: !actions;
          dst_file)
        else
          let src_path = Filename.concat src_dir ml_file in
          actions := CopyFile (src_path, ml_file) :: !actions;
          ml_file)
      ml_files
  in

  (* Note: mli_files and ml_files are already transformed at this point,
     so lib.mli -> pkg_name.mli and needs no additional copying *)

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

  (* 4. Create library or executable *)
  let outputs = ref [] in

  (if has_main then (
     (* Create executable *)
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

     (* Get dependency libraries - in topological order (dependencies first) *)
     let dep_libs =
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

     let all_objects = cmo_files @ o_files in

     (* For executable, link with dependency .cma files *)
     actions :=
       CreateExecutable (exe_path, all_objects, dep_libs, dep_includes)
       :: !actions;

     (* Declare executable as output *)
     outputs := exe_path :: !outputs)
   else
     (* Create library for any package without main *)
     let cma_path = pkg_name ^ ".cma" in
     let all_objects = cmo_files @ o_files in
     actions := CreateLibrary (cma_path, all_objects, dep_includes) :: !actions;

     (* Declare library outputs *)
     outputs := cma_path :: !outputs;
     outputs := (pkg_name ^ ".cmi") :: !outputs);

  (* Add C object files to outputs if any *)
  List.iter (fun o -> outputs := o :: !outputs) o_files;

  (* Add DeclareOutputs action *)
  if !outputs <> [] then actions := DeclareOutputs !outputs :: !actions;

  let actions = List.rev !actions in

  let blueprint = {
    package_name = pkg_name;
    package_path = pkg_path;
    dependencies;
    actions;
    toolchain_version;
    hash = None; (* Will be computed after blueprint creation *)
  } in
  
  (* Use the provided hash *)
  { blueprint with hash = Some hash }

(** Execute an action in the sandbox *)
let execute_action action toolchain_version =
  let run_command cmd =
    let success, output = System.run_command cmd in
    if success then (Success, output) else (Failed output, output)
  in

  match action with
  | CompileInterface (src, dst, includes) ->
      let include_flags =
        String.concat " " (List.map (fun p -> "-I " ^ p) includes)
      in
      let ocamlc = Toolchains.ocamlc_path toolchain_version in
      let cmd =
        Printf.sprintf "%s -I +unix %s -c -o %s %s" ocamlc include_flags dst src
      in
      run_command cmd
  | CompileImplementation (src, dst, includes) ->
      let include_flags =
        String.concat " " (List.map (fun p -> "-I " ^ p) includes)
      in
      let ocamlc = Toolchains.ocamlc_path toolchain_version in
      let cmd =
        Printf.sprintf "%s -I +unix %s -I . -c -o %s %s" ocamlc include_flags
          dst src
      in
      run_command cmd
  | CompileC (src, dst) ->
      let ocamlc = Toolchains.ocamlc_path toolchain_version in
      let cmd = Printf.sprintf "%s -I +unix -c -o %s %s" ocamlc dst src in
      run_command cmd
  | CreateLibrary (lib, object_files, includes) ->
      let include_flags =
        String.concat " " (List.map (fun p -> "-I " ^ p) includes)
      in
      let files_str = String.concat " " object_files in
      let ocamlc = Toolchains.ocamlc_path toolchain_version in
      let cmd =
        Printf.sprintf "%s -I +unix %s -a -o %s %s" ocamlc include_flags lib
          files_str
      in
      run_command cmd
  | CreateExecutable (exe, object_files, libs, includes) ->
      let include_flags =
        String.concat " " (List.map (fun p -> "-I " ^ p) includes)
      in
      let files_str = String.concat " " object_files in
      let libs_str = String.concat " " libs in
      let ocamlc = Toolchains.ocamlc_path toolchain_version in
      (* Include current directory to find C objects from dependencies *)
      (* Link with unix.cma for Unix module support *)
      (* Use -custom to include C stubs in the executable *)
      let cmd =
        Printf.sprintf "%s -custom -I +unix %s -I . -o %s unix.cma %s %s" ocamlc
          include_flags exe libs_str files_str
      in
      run_command cmd
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
let execute_blueprint root blueprint =
  Printf.printf "[Actions] Executing blueprint for %s\n" blueprint.package_name;
  flush stdout;
  let success = ref true in
  let errors = ref [] in

  List.iteri
    (fun i action ->
      Printf.printf "[Actions] Step %d: %s\n" (i + 1) (string_of_action action);
      flush stdout;

      let result, output = execute_action action blueprint.toolchain_version in
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
