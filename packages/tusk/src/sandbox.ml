(** Sandbox - isolated build execution environment *)

type t = {
  root : string;
  sandbox_dir : string;
  target_dir : string;
  node : Build_node.t;
  workspace : Workspace.workspace;
}

(** Create a new sandbox for a build graph node *)
let create ~node ~(workspace : Workspace.workspace) =
  let root = workspace.root in
  let target_dir_root = Filename.concat root "target" in
  let debug_dir = Filename.concat target_dir_root "debug" in
  let out_dir = Filename.concat debug_dir "out" in
  let target_dir =
    Filename.concat out_dir node.Build_node.package.relative_path
  in

  (* Create a unique sandbox directory for this build *)
  let sandbox_id =
    Printf.sprintf "%08x"
      (Hashtbl.hash
         (node.Build_node.package.name ^ string_of_float (System.time ())))
  in
  let sandbox_dir =
    Filename.concat (Filename.concat debug_dir "sandbox") sandbox_id
  in

  (* Create directories *)
  System.mkdir_safe target_dir_root 0o755;
  System.mkdir_safe debug_dir 0o755;
  System.mkdir_safe out_dir 0o755;
  System.mkdir_safe (Filename.concat debug_dir "sandbox") 0o755;
  System.mkdir_safe sandbox_dir 0o755;

  (* Create parent directories for target_dir *)
  System.mkdirp target_dir;

  { root; sandbox_dir; target_dir; node; workspace }

(** Get dependency include paths for the sandbox *)
let get_dependency_includes sandbox =
  List.fold_left
    (fun acc dep ->
      let dep_target =
        Filename.concat
          (Filename.concat
             (Filename.concat (Filename.concat sandbox.root "target") "debug")
             "out")
          dep.Build_node.package.relative_path
      in
      if System.file_exists dep_target then dep_target :: acc else acc)
    [] sandbox.node.Build_node.dependencies

(** Get all transitive dependencies of a node *)
let rec get_transitive_dependencies node visited =
  if List.mem node.Build_node.package.name visited then []
  else
    let visited = node.Build_node.package.name :: visited in
    let direct_deps = node.Build_node.dependencies in
    let transitive_deps =
      List.concat
        (List.map
           (fun dep -> get_transitive_dependencies dep visited)
           direct_deps)
    in
    direct_deps @ transitive_deps

(** Copy dependency artifacts into sandbox *)
let copy_dependency_artifacts sandbox =
  Printf.printf "[Sandbox] Copying dependency artifacts...\n";
  flush stdout;

  (* Get all transitive dependencies *)
  let all_deps = get_transitive_dependencies sandbox.node [] in

  (* Copy artifacts from each dependency *)
  List.iter
    (fun dep ->
      let dep_name = dep.Build_node.package.name in
      (* Look in target/debug/out/<relative_path> where the artifacts are stored *)
      let dep_target_dir =
        Filename.concat
          (Filename.concat
             (Filename.concat (Filename.concat sandbox.root "target") "debug")
             "out")
          dep.Build_node.package.relative_path
      in

      if System.file_exists dep_target_dir then (
        Printf.printf "[Sandbox] Copying artifacts from %s\n" dep_name;
        flush stdout;

        (* List files in dependency target directory *)
        let files =
          System.list_dir dep_target_dir (fun file ->
              (* Main library archive *)
              file = dep_name ^ ".cma"
              (* Main module interface *)
              || file = dep_name ^ ".cmi"
              ||
              (* C object files *)
              Filename.check_suffix file ".o")
        in
        List.iter
          (fun file ->
            let src = Filename.concat dep_target_dir file in
            let dst = Filename.concat sandbox.sandbox_dir file in

            (* Only copy if destination doesn't exist *)
            if not (System.file_exists dst) then (
              System.copy_file src dst;
              Printf.printf "  -> Copied %s\n" file;
              flush stdout))
          files)
      else (
        Printf.printf
          "[Sandbox] Warning: Dependency %s has no artifacts in %s\n" dep_name
          dep_target_dir;
        flush stdout))
    all_deps

(** Run a list of actions in the sandbox *)
let rec run_actions ~sandbox ~blueprint ~store =
  let pkg_name = sandbox.node.Build_node.package.name in

  (* Check if we have a blueprint hash and if artifacts are already cached *)
  match blueprint.Actions.hash with
  | Some hash ->
      if Store.exists store hash then (
        Printf.printf "[Sandbox] Cache hit for %s (hash: %s)\n" pkg_name
          (Hasher.to_string hash);
        flush stdout;

        (* Promote artifacts from store directly to target *)
        if Store.promote_from_store store hash sandbox.target_dir then
          (true, "Retrieved from cache")
        else (false, "Failed to promote from cache"))
      else (
        Printf.printf "[Sandbox] Cache miss for %s (hash: %s), building...\n"
          pkg_name (Hasher.to_string hash);
        flush stdout;

        (* Proceed with normal build *)
        build_in_sandbox ~sandbox ~blueprint ~store ~hash:(Some hash))
  | None ->
      Printf.printf
        "[Sandbox] No hash computed for %s, building without caching...\n"
        pkg_name;
      flush stdout;

      (* Proceed with normal build (no caching) *)
      build_in_sandbox ~sandbox ~blueprint ~store ~hash:None

(** Internal function to actually build in sandbox *)
and build_in_sandbox ~sandbox ~blueprint ~store ~hash =
  (* Print the blueprint since we're about to execute it *)
  Actions.print_blueprint blueprint;

  Printf.printf "[Sandbox] Running %d actions for %s in %s\n"
    (List.length blueprint.Actions.actions)
    sandbox.node.Build_node.package.name sandbox.sandbox_dir;
  flush stdout;

  (* Copy all transitive dependency artifacts into sandbox *)
  copy_dependency_artifacts sandbox;

  (* Change to sandbox directory *)
  let original_cwd = System.getcwd () in
  System.chdir sandbox.sandbox_dir;

  (* Track declared outputs *)
  let declared_outputs = ref [] in

  let result =
    try
      let success = ref true in
      let errors = ref [] in

      List.iteri
        (fun i action ->
          Printf.printf "[Sandbox] Step %d: %s\n" (i + 1)
            (Actions.string_of_action action);
          flush stdout;

          (* Track declared outputs *)
          (match action with
          | Actions.DeclareOutputs outputs -> declared_outputs := outputs
          | _ -> ());

          let result, output =
            Actions.execute_action action blueprint.Actions.toolchain
          in
          match result with
          | Actions.Success ->
              if output <> "" then (
                Printf.printf "  -> %s\n" output;
                flush stdout)
          | Actions.Skipped reason ->
              Printf.printf "  -> Skipped: %s\n" reason;
              flush stdout
          | Actions.Failed error ->
              success := false;
              errors := error :: !errors;
              Printf.printf "  -> Failed: %s\n" error;
              flush stdout)
        blueprint.Actions.actions;

      if !success then (true, "Build successful")
      else (false, String.concat "; " !errors)
    with exn ->
      let error_msg =
        Printf.sprintf "Sandbox execution failed: %s" (Printexc.to_string exn)
      in
      (false, error_msg)
  in

  (* Restore original working directory *)
  System.chdir original_cwd;

  (* Copy artifacts to target directory *)
  if fst result then (
    let copy_artifacts () =
      try
        (* Copy only declared outputs *)
        if !declared_outputs <> [] then (
          Printf.printf "[Sandbox] Copying declared outputs...\n";
          flush stdout;
          List.iter
            (fun output_file ->
              let src = Filename.concat sandbox.sandbox_dir output_file in
              if System.file_exists src then (
                let dst = Filename.concat sandbox.target_dir output_file in
                System.copy_file src dst;
                (* Make executable files executable *)
                if not (String.contains output_file '.') then (
                  System.chmod dst 0o755;
                  (* Also promote executable to target/<profile>/<name> *)
                  let profile_dir =
                    Filename.concat
                      (Filename.concat sandbox.root "target")
                      "debug"
                  in
                  let promoted_dst = Filename.concat profile_dir output_file in
                  System.copy_file src promoted_dst;
                  System.chmod promoted_dst 0o755;
                  Printf.printf "[Sandbox] Promoted executable %s to %s\n"
                    output_file promoted_dst;
                  flush stdout);
                Printf.printf "[Sandbox] Copied %s to target\n" output_file;
                flush stdout)
              else (
                Printf.printf
                  "[Sandbox] Warning: Declared output %s not found in sandbox\n"
                  output_file;
                flush stdout))
            !declared_outputs)
        else (
          (* Fallback: if no outputs declared, use heuristics *)
          Printf.printf
            "[Sandbox] Warning: No outputs declared, using fallback heuristics\n";
          flush stdout;
          let pkg_name = sandbox.node.Build_node.package.name in
          let files =
            System.list_dir sandbox.sandbox_dir (fun file ->
                (* Main library archive *)
                file = pkg_name ^ ".cma"
                (* Main module interface *)
                || file = pkg_name ^ ".cmi"
                (* C object files *)
                || Filename.check_suffix file ".o"
                ||
                (* Executable (no extension and matches package name) *)
                ((not (String.contains file '.')) && file = pkg_name))
          in
          List.iter
            (fun file ->
              let src = Filename.concat sandbox.sandbox_dir file in
              let dst = Filename.concat sandbox.target_dir file in
              System.copy_file src dst;
              (* Make executable files executable *)
              if not (String.contains file '.') then (
                System.chmod dst 0o755;
                (* Also promote executable to target/<profile>/<name> *)
                let profile_dir =
                  Filename.concat
                    (Filename.concat sandbox.root "target")
                    "debug"
                in
                let promoted_dst = Filename.concat profile_dir file in
                System.copy_file src promoted_dst;
                System.chmod promoted_dst 0o755;
                Printf.printf "[Sandbox] Promoted executable %s to %s\n" file
                  promoted_dst;
                flush stdout);
              Printf.printf "[Sandbox] Copied %s to target\n" file;
              flush stdout)
            files)
      with exn ->
        Printf.printf "[Sandbox] Warning: Failed to copy artifacts: %s\n"
          (Printexc.to_string exn);
        flush stdout
    in
    copy_artifacts ();

    (* After successful build, store artifacts in content-addressable cache *)
    match hash with
    | Some h when fst result ->
        (* Get declared outputs from the blueprint *)
        let declared_outputs = ref [] in
        List.iter
          (fun action ->
            match action with
            | Actions.DeclareOutputs outputs -> declared_outputs := outputs
            | _ -> ())
          blueprint.Actions.actions;

        if !declared_outputs <> [] then (
          Printf.printf "[Sandbox] Storing build artifacts in cache\n";
          flush stdout;
          let _artifact =
            Store.store_artifacts store h sandbox.sandbox_dir !declared_outputs
          in
          (* TODO: Use artifact witness to update build results *)
          ())
    | _ -> () (* No hash or build failed, skip caching *));

  result

(** Clean up sandbox directory *)
let cleanup sandbox = System.remove_dir sandbox.sandbox_dir
