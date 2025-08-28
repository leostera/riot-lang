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

  (* Separate source files by type *)
  let ml_files =
    List.filter
      (fun f -> Filename.check_suffix (Std.Path.to_string f) ".ml")
      srcs
  in
  let mli_files =
    List.filter
      (fun f -> Filename.check_suffix (Std.Path.to_string f) ".mli")
      srcs
  in
  let c_files =
    List.filter
      (fun f -> Filename.check_suffix (Std.Path.to_string f) ".c")
      srcs
  in

  (* Sort ML and MLI files in dependency order using ocamldep *)
  let sorted_ml_files, sorted_mli_files =
    if ml_files <> [] || mli_files <> [] then
      (* Get the directory containing the source files *)
      let src_dir =
        match srcs with
        | [] -> "."
        | hd :: _ -> Filename.dirname (Std.Path.to_string hd)
      in
      (* Convert to basenames for ocamldep *)
      let ml_basenames =
        List.map (fun f -> Filename.basename (Std.Path.to_string f)) ml_files
      in
      let mli_basenames =
        List.map (fun f -> Filename.basename (Std.Path.to_string f)) mli_files
      in
      let all_basenames = mli_basenames @ ml_basenames in

      let sorted_basenames =
        Ocamldep.sort ~toolchain ~cwd:src_dir ~files:all_basenames
      in

      (* Map sorted basenames back to full paths *)
      let basename_to_path lst =
        List.filter_map (fun basename ->
            List.find_opt
              (fun p -> Filename.basename (Std.Path.to_string p) = basename)
              lst)
      in

      let sorted_mli =
        List.filter (fun f -> Filename.check_suffix f ".mli") sorted_basenames
        |> fun names -> basename_to_path mli_files names
      in
      let sorted_ml =
        List.filter (fun f -> Filename.check_suffix f ".ml") sorted_basenames
        |> fun names -> basename_to_path ml_files names
      in
      (sorted_ml, sorted_mli)
    else (ml_files, mli_files)
  in

  (* Compile .mli files to .cmi - DON'T reverse, we're prepending so it reverses naturally *)
  List.iter
    (fun mli_file ->
      let mli_str = Std.Path.to_string mli_file in
      let basename = Filename.chop_suffix (Filename.basename mli_str) ".mli" in
      let cmi_path = basename ^ ".cmi" in
      actions :=
        Actions.CompileInterface
          { source = mli_str; output = cmi_path; includes = dep_includes }
        :: !actions;
      outputs := cmi_path :: !outputs)
    sorted_mli_files;

  (* Compile .c files to .o *)
  let o_files = ref [] in
  List.iter
    (fun c_file ->
      let c_str = Std.Path.to_string c_file in
      let basename = Filename.chop_suffix (Filename.basename c_str) ".c" in
      let o_path = basename ^ ".o" in
      actions :=
        Actions.CompileC { source = c_str; output = o_path } :: !actions;
      o_files := o_path :: !o_files;
      outputs := o_path :: !outputs)
    c_files;

  (* Compile .ml files to .cmo - DON'T reverse, we're prepending so it reverses naturally *)
  let cmo_files = ref [] in
  List.iter
    (fun ml_file ->
      let ml_str = Std.Path.to_string ml_file in
      let basename = Filename.chop_suffix (Filename.basename ml_str) ".ml" in
      let cmo_path = basename ^ ".cmo" in
      actions :=
        Actions.CompileImplementation
          { source = ml_str; output = cmo_path; includes = dep_includes }
        :: !actions;
      cmo_files := cmo_path :: !cmo_files;
      outputs := cmo_path :: !outputs)
    sorted_ml_files;

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
      (fun f -> Filename.basename (Std.Path.to_string f) = "main.ml")
      sorted_ml_files
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
  (List.rev output_paths, List.rev !actions)

let compute_hash_for_planned_node ~toolchain ~package ~srcs ~deps ~outs ~actions
    =
  (* Collect all the hash seeds *)
  let seeds = ref [] in

  (* Hash the toolchain *)
  let toolchain_hash = Toolchains.hash toolchain in
  seeds := Hasher.to_string toolchain_hash :: !seeds;

  (* Add package name *)
  seeds := package.Workspace.name :: !seeds;

  (* Hash source files *)
  let srcs_hash = Hasher.hash_files srcs in
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
    let dep_errors =
      List.map (fun (name, _err) -> name) failed_deps
    in
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
      let hash_start = Global.time_ms () in
      let hash =
        compute_hash_for_planned_node ~toolchain:node.toolchain
          ~package:node.package ~srcs:node.srcs ~deps:dep_nodes ~outs ~actions
      in
      let hash_duration = Global.time_ms () - hash_start in
      Log.hash_computed ~session_id ~package:node.package.name
        ~hash:(Hasher.to_string hash) ~duration_ms:hash_duration;

      (* Update the node's spec to Planned *)
      node.spec <- Planned { hash; outs; actions };

      Ok (Planned node)
