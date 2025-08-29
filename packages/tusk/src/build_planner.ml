open Std

type skip_reason = DependenciesFailed of string list

type plan_result =
  | Planned of Build_node.t
  | MissingDependencies of { node : Build_node.t; deps : Build_node.t list }
  | Skipped of { node : Build_node.t; reason : skip_reason }

type error = string

let get_dependency_libs_and_includes toolchain dependencies =
  let libs = ref [] in
  let includes = ref [] in
  List.iter
    (fun (dep : Workspace.dependency) ->
      match dep.Workspace.name with
      | "unix" ->
          libs := "unix.cma" :: !libs;
          includes := "+unix" :: !includes
      | "threads" ->
          libs := "threads.cma" :: !libs;
          includes := "+threads" :: !includes
      | _ -> ())
    dependencies;
  (!libs, !includes)

(* Check if any transitive dependency requires unix *)
let needs_unix_transitively ~graph ~node =
  let rec check_node visited current_node =
    if List.mem current_node.Build_node.package.name visited then false
    else
      let visited = current_node.Build_node.package.name :: visited in
      (* Check if this node directly depends on unix *)
      let has_unix_dep =
        List.exists
          (fun (dep : Workspace.dependency) -> dep.Workspace.name = "unix")
          current_node.Build_node.package.dependencies
      in
      if has_unix_dep then true
      else
        (* Check transitive dependencies *)
        let dep_nodes =
          List.map (Build_graph.get_node graph) current_node.Build_node.deps
        in
        List.exists (check_node visited) dep_nodes
  in
  check_node [] node

let generate_actions ~graph ~node ~toolchain ~package ~srcs ~deps =
  let actions = ref [] in
  let outputs = ref [] in
  (* List of output file paths as strings *)

  (* Get external dependencies from package *)
  let external_libs, external_includes =
    get_dependency_libs_and_includes toolchain package.Workspace.dependencies
  in

  (* Get dependency include paths from local packages *)
  (* Since all dependency artifacts are copied to the sandbox directory,
     we just need to include the current directory (.) where they'll be available *)
  let local_dep_includes =
    if deps <> [] then [ "." ]
      (* Sandbox directory contains all dependency artifacts *)
    else []
  in

  (* Check if we need unix transitively for includes too *)
  let needs_unix_trans = needs_unix_transitively ~graph ~node in
  let all_external_includes =
    if needs_unix_trans && not (List.mem "+unix" external_includes) then
      "+unix" :: external_includes
    else external_includes
  in
  (* Combine all include paths *)
  let dep_includes = all_external_includes @ local_dep_includes in

  (* Separate source files by type using the kind field *)
  let ml_sources =
    List.filter (fun s -> match s.Build_node.kind with Build_node.ML _ -> true | _ -> false) srcs
  in
  let mli_sources =
    List.filter (fun s -> match s.Build_node.kind with Build_node.MLI _ -> true | _ -> false) srcs
  in
  let c_sources =
    List.filter (fun s -> match s.Build_node.kind with Build_node.C_stub -> true | _ -> false) srcs
  in

  (* Sort ML and MLI files in dependency order using ocamldep *)
  let sorted_ml_sources, sorted_mli_sources =
    if ml_sources <> [] || mli_sources <> [] then
      (* Get the directory containing the source files *)
      let src_dir =
        match srcs with
        | [] -> "."
        | hd :: _ -> Filename.dirname (Std.Path.to_string hd.Build_node.file)
      in
      (* Convert to basenames for ocamldep *)
      let ml_basenames =
        List.map (fun s -> Filename.basename (Std.Path.to_string s.Build_node.file)) ml_sources
      in
      let mli_basenames =
        List.map (fun s -> Filename.basename (Std.Path.to_string s.Build_node.file)) mli_sources
      in
      let all_basenames = mli_basenames @ ml_basenames in

      let sorted_basenames =
        Ocamldep.sort ~toolchain ~cwd:src_dir ~files:all_basenames
      in

      (* Map sorted basenames back to source records *)
      let basename_to_source lst =
        List.filter_map (fun basename ->
            List.find_opt
              (fun s -> Filename.basename (Std.Path.to_string s.Build_node.file) = basename)
              lst)
      in

      let sorted_mli =
        List.filter (fun f -> Filename.check_suffix f ".mli") sorted_basenames
        |> fun names -> basename_to_source mli_sources names
      in
      let sorted_ml =
        List.filter (fun f -> Filename.check_suffix f ".ml") sorted_basenames
        |> fun names -> basename_to_source ml_sources names
      in
      (sorted_ml, sorted_mli)
    else (ml_sources, mli_sources)
  in

  (* Prepare alias module configuration but don't add actions yet *)
  let alias_module_name_opt, open_flags, alias_cmo_opt = 
    (* Collect all modules that need aliases *)
    let module_aliases = 
      (sorted_ml_sources @ sorted_mli_sources)
      |> List.filter_map (fun source ->
          match source.Build_node.kind with
          | Build_node.ML { simple_name; namespaced_name; _ } 
          | Build_node.MLI { simple_name; namespaced_name; _ } ->
              (* Skip if it's the main package module or if names are the same *)
              if simple_name = namespaced_name then None
              else Some (simple_name, namespaced_name)
          | _ -> None)
      |> List.sort_uniq compare  (* Remove duplicates *)
    in
    
    if module_aliases <> [] then (
      (* Generate alias module content *)
      let alias_content = 
        "(* Auto-generated module aliases for package " ^ package.Workspace.name ^ " *)\n" ^
        (module_aliases
         |> List.map (fun (simple, namespaced) ->
             Printf.sprintf "module %s = %s" simple namespaced)
         |> String.concat "\n")
      in
      
      (* Create a unique alias module name *)
      let safe_name = String.map (fun c -> if c = '-' then '_' else c) package.Workspace.name in
      let alias_module_name = String.capitalize_ascii safe_name ^ "__aliases" in
      let alias_ml = alias_module_name ^ ".ml" in
      let alias_cmo = alias_module_name ^ ".cmo" in
      
      (Some alias_module_name, [Ocamlc.Open alias_module_name], Some (alias_ml, alias_cmo, alias_content))
    ) else (None, [], None)
  in

  (* Compile .mli files to .cmi with -open flag if we have aliases *)
  List.iter
    (fun mli_source ->
      let mli_str = Std.Path.to_string mli_source.Build_node.file in
      (* Use the pre-computed namespaced module name *)
      let cmi_path = 
        match mli_source.Build_node.kind with
        | Build_node.MLI { namespaced_name; _ } -> namespaced_name ^ ".cmi"
        | _ -> failwith "Internal error: non-MLI source in mli_sources"
      in
      (* Add -open flag if we have an alias module *)
      actions :=
        Actions.CompileInterface
          { source = mli_str; output = cmi_path; includes = dep_includes; flags = open_flags }
        :: !actions;
      outputs := cmi_path :: !outputs)
    sorted_mli_sources;

  (* Compile .c files to .o *)
  let o_files = ref [] in
  List.iter
    (fun c_source ->
      let c_str = Std.Path.to_string c_source.Build_node.file in
      (* C files just use their basename without extension *)
      let basename = Filename.chop_extension (Filename.basename c_str) in
      let o_path = basename ^ ".o" in
      actions :=
        Actions.CompileC { source = c_str; output = o_path } :: !actions;
      o_files := o_path :: !o_files;
      outputs := o_path :: !outputs)
    c_sources;

  (* Compile .ml files to .cmo - DON'T reverse, we're prepending so it reverses naturally *)
  let cmo_files = ref [] in
  (* Add alias module to cmo_files if it exists *)
  (match alias_module_name_opt with
   | Some alias_name -> cmo_files := (alias_name ^ ".cmo") :: !cmo_files
   | None -> ());
  
  List.iter
    (fun ml_source ->
      let ml_str = Std.Path.to_string ml_source.Build_node.file in
      (* Use the pre-computed namespaced module name *)
      let cmo_path = 
        match ml_source.Build_node.kind with
        | Build_node.ML { namespaced_name; _ } -> namespaced_name ^ ".cmo"
        | _ -> failwith "Internal error: non-ML source in ml_sources"
      in
      (* Add -open flag if we have an alias module *)
      actions :=
        Actions.CompileImplementation
          { source = ml_str; output = cmo_path; includes = dep_includes; flags = open_flags }
        :: !actions;
      cmo_files := cmo_path :: !cmo_files;
      outputs := cmo_path :: !outputs)
    sorted_ml_sources;


  (* Always build a library if we have any .ml files *)
  if !cmo_files <> [] then (
    (* Create library *)
    let cma_path = Workspace.(package.name) ^ ".cma" in
    let lib_objects = List.rev !cmo_files @ !o_files in
    actions :=
      Actions.CreateLibrary
        { output = cma_path; objects = lib_objects; includes = dep_includes }
      :: !actions;
    outputs := cma_path :: !outputs);

  (* Check if we should build executables *)
  (* For now, we build an executable if main.ml exists *)
  (* TODO: In the future, use [[bin]] definitions from package config *)
  let has_main_ml =
    List.exists
      (fun s -> Filename.basename (Std.Path.to_string s.Build_node.file) = "main.ml")
      sorted_ml_sources
  in

  if has_main_ml then (
    (* Create executable *)
    let exe_path = Workspace.(package.name) in
    let exe_objects = List.rev !cmo_files @ !o_files in
    (* Include dependency libraries (.cma files) from local packages *)
    (* We need to collect ALL transitive dependencies, not just direct ones *)
    let rec collect_all_deps visited current_node =
      if List.mem current_node.Build_node.package.name visited then []
      else
        let visited = current_node.Build_node.package.name :: visited in
        (* Resolve dependency IDs to nodes *)
        let current_dep_nodes =
          List.map (Build_graph.get_node graph) current_node.Build_node.deps
        in
        let child_deps =
          List.concat_map (collect_all_deps visited) current_dep_nodes
        in
        current_node :: child_deps
    in
    let all_dep_nodes =
      List.concat_map (collect_all_deps []) deps
      |> List.rev (* Reverse to get dependencies before dependents *)
      |> List.fold_left
           (fun acc node ->
             if
               List.exists
                 (fun n ->
                   n.Build_node.package.name = node.Build_node.package.name)
                 acc
             then acc (* Already in list, skip duplicate *)
             else node :: acc)
           [] (* Add unique nodes *)
      |> List.rev (* Reverse back to get correct order: dependencies first *)
    in
    let dep_libs =
      List.map (fun dep -> dep.Build_node.package.name ^ ".cma") all_dep_nodes
    in
    (* Check if we need to add unix.cma transitively *)
    let needs_unix = needs_unix_transitively ~graph ~node in
    let all_external_libs =
      if needs_unix && not (List.mem "unix.cma" external_libs) then
        "unix.cma" :: external_libs
      else external_libs
    in
    let all_libs = all_external_libs @ dep_libs in
    actions :=
      Actions.CreateExecutable
        {
          output = exe_path;
          objects = exe_objects;
          libraries = all_libs;
          includes = dep_includes;
        }
      :: !actions;
    outputs := exe_path :: !outputs);

  (* Add alias module to outputs BEFORE DeclareOutputs *)
  (match alias_cmo_opt with
  | Some (_, alias_cmo, _) ->
      (* Add both .cmo and .cmi files for the alias module *)
      let alias_cmi = Filename.chop_extension alias_cmo ^ ".cmi" in
      outputs := alias_cmi :: alias_cmo :: !outputs
  | None -> ());

  (* Add DeclareOutputs action *)
  if !outputs <> [] then
    actions := Actions.DeclareOutputs { outputs = !outputs } :: !actions;

  (* Convert output strings to Path.t for the return value *)
  let output_paths =
    List.filter_map
      (fun s ->
        match Std.Path.of_string s with Ok p -> Some p | Error _ -> None)
      !outputs
  in
  
  (* Build final action list with alias module FIRST *)
  let final_actions = 
    match alias_cmo_opt with
    | Some (alias_ml, alias_cmo, alias_content) ->
        (* Alias module actions go first *)
        let alias_actions = [
          Actions.WriteFile { destination = alias_ml; content = alias_content };
          Actions.CompileImplementation 
            { source = alias_ml; output = alias_cmo; 
              includes = dep_includes;
              flags = [Ocamlc.NoAliasDeps] }
        ] in
        (* Alias actions first, then all other actions (reversed) *)
        alias_actions @ (List.rev !actions)
    | None -> 
        List.rev !actions
  in
  (List.rev output_paths, final_actions)

let compute_hash_for_planned_node ~toolchain ~package ~srcs ~deps ~outs ~actions
    =
  (* Collect all the hash seeds *)
  let seeds = ref [] in

  (* Hash the toolchain *)
  let toolchain_hash = Toolchains.hash toolchain in
  seeds := Hasher.to_string toolchain_hash :: !seeds;

  (* Add package name *)
  seeds := package.Workspace.name :: !seeds;

  (* Hash source files - extract the file paths from source records *)
  let src_files = List.map (fun s -> s.Build_node.file) srcs in
  let srcs_hash = Hasher.hash_files src_files in
  seeds := Hasher.to_string srcs_hash :: !seeds;

  (* Add dependency hashes *)
  List.iter
    (fun dep ->
      match dep.Build_node.spec with
      | Unplanned -> ()
      | Planned { hash; _ } -> seeds := Hasher.to_string hash :: !seeds)
    deps;

  (* Hash actions *)
  let actions_hash = Actions.hash actions in
  seeds := Hasher.to_string actions_hash :: !seeds;

  (* Combine all seeds into final hash *)
  Hasher.hash_strings (List.rev !seeds)

let plan_node ~graph ~node ~build_results ~session_id () =
  (* Step 1: Check if immediate dependencies have been planned/built *)
  (* Resolve dependency IDs to nodes *)
  let dep_nodes = List.map (Build_graph.get_node graph) node.Build_node.deps in

  (* First, check if any dependencies have failed *)
  let failed_deps =
    List.filter_map
      (fun dep ->
        let pkg_name = dep.Build_node.package.name in
        match Build_results.get_status build_results pkg_name with
        | Some (Build_results.Failed error) -> Some (pkg_name, error)
        | _ -> None)
      dep_nodes
  in

  if failed_deps <> [] then
    (* If any dependency has failed, fail this node immediately *)
    (* Just record which dependencies failed, not their full error messages *)
    let dep_errors = List.map (fun (name, _err) -> name) failed_deps in
    Ok (Skipped { node; reason = DependenciesFailed dep_errors })
  else
    (* Check for unplanned/unbuilt dependencies *)
    let unplanned_deps =
      List.filter
        (fun dep ->
          (* A dependency is ready only if it's successfully built *)
          let pkg_name = dep.Build_node.package.name in
          match Build_results.get_status build_results pkg_name with
          | Some (Build_results.Built _) -> false (* Already built - ready *)
          | Some Build_results.Building -> true (* Still building - not ready *)
          | Some Build_results.NotStarted -> true (* Not started - not ready *)
          | Some (Build_results.Failed _) ->
              true (* Should have been caught above *)
          | None -> true (* Not tracked - not ready *))
        dep_nodes
    in

    if unplanned_deps <> [] then
      (* Return MissingDependencies if any deps are unplanned *)
      Ok (MissingDependencies { node; deps = unplanned_deps })
    else
      (* Step 2: All deps are planned, we can proceed *)
      (* Step 3: Generate outputs and actions based on sources, deps, and package *)
      let outs, actions =
        generate_actions ~graph ~node ~toolchain:node.toolchain
          ~package:node.package ~srcs:node.srcs ~deps:dep_nodes
      in

      (* Step 4: Compute SHA512 hash of everything *)
      Log.computing_hash ~session_id ~package:node.package.name;
      let hash_start = time_ms () in
      let hash =
        compute_hash_for_planned_node ~toolchain:node.toolchain
          ~package:node.package ~srcs:node.srcs ~deps:dep_nodes ~outs ~actions
      in
      let hash_duration = time_ms () - hash_start in
      Log.hash_computed ~session_id ~package:node.package.name
        ~hash:(Hasher.to_string hash) ~duration_ms:hash_duration;

      (* Update the node's spec to Planned *)
      node.spec <- Planned { hash; outs; actions };

      Ok (Planned node)
