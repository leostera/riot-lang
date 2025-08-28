(** Sandbox - isolated build execution environment *)

type t = {
  root : string;
  sandbox_dir : string;
  target_dir : string;
  node : Build_node.t;
  workspace : Workspace.t;
}

type error = string

(** Create a new sandbox for a build graph node *)
let create ~node ~(workspace : Workspace.t) =
  let root = Std.Path.to_string workspace.root in
  let target_dir_root = Filename.concat root "target" in
  let debug_dir = Filename.concat target_dir_root "debug" in
  let out_dir = Filename.concat debug_dir "out" in
  let target_dir =
    Filename.concat out_dir
      (Std.Path.to_string node.Build_node.package.relative_path)
  in

  (* Create a unique sandbox directory for this build *)
  let sandbox_id =
    Printf.sprintf "%08x"
      (Hashtbl.hash
         (node.Build_node.package.name ^ string_of_float (Std.time ())))
  in
  let sandbox_dir =
    Filename.concat (Filename.concat debug_dir "sandbox") sandbox_id
  in

  (* Create directories *)
  let _ =
    Fs.mkdir
      (Path.of_string target_dir_root |> Result.expect ~msg:"Invalid path")
      0o755
  in
  let _ =
    Fs.mkdir
      (Path.of_string debug_dir |> Result.expect ~msg:"Invalid path")
      0o755
  in
  let _ =
    Fs.mkdir (Path.of_string out_dir |> Result.expect ~msg:"Invalid path") 0o755
  in
  let _ =
    Fs.mkdir
      (Path.of_string (Filename.concat debug_dir "sandbox")
      |> Result.expect ~msg:"Invalid path")
      0o755
  in
  let _ =
    Fs.mkdir
      (Path.of_string sandbox_dir |> Result.expect ~msg:"Invalid path")
      0o755
  in

  (* Create parent directories for target_dir *)
  let _ =
    Fs.mkdirp (Path.of_string target_dir |> Result.expect ~msg:"Invalid path")
  in
  ();

  { root; sandbox_dir; target_dir; node; workspace }

(** Get sandbox directory *)
let get_sandbox_dir t = t.sandbox_dir

(** Get all transitive dependencies of a node *)
let rec get_transitive_dependencies node ~build_graph visited =
  if List.mem node.Build_node.package.name visited then []
  else
    let visited = node.Build_node.package.name :: visited in
    (* Resolve dep IDs to actual nodes *)
    let direct_deps =
      List.map (Build_graph.get_node build_graph) node.Build_node.deps
    in
    let transitive_deps =
      List.concat
        (List.map
           (fun dep -> get_transitive_dependencies dep ~build_graph visited)
           direct_deps)
    in
    direct_deps @ transitive_deps

(** Copy dependency artifacts into sandbox *)
let copy_dependency_artifacts sandbox ~store ~build_graph ~build_results =
  Printf.printf "[Sandbox] Copying dependency artifacts...\n";
  flush stdout;

  (* Get all transitive dependencies *)
  let all_deps = get_transitive_dependencies sandbox.node ~build_graph [] in

  (* Copy artifacts from each dependency *)
  List.iter
    (fun dep ->
      let dep_name = dep.Build_node.package.name in
      (* Check if this dependency has a hash - either from build_results or from its spec *)
      let dep_hash =
        match Build_results.get_status build_results dep_name with
        | Some (Build_results.Built hash) -> Some hash
        | _ -> (
            (* If not marked as Built in build_results, check if the node has a planned spec with hash *)
            match dep.Build_node.spec with
            | Build_node.Planned { hash; _ } ->
                (* The dependency has been planned and has a hash, artifacts should be in store *)
                Printf.printf "[Sandbox] Using hash from planned spec for %s\n"
                  dep_name;
                flush stdout;
                Some hash
            | _ -> None)
      in
      match dep_hash with
      | Some hash ->
          if
            (* Check if artifacts exist in the store *)
            Store.exists store hash
          then (
            Printf.printf
              "[Sandbox] Copying artifacts from store for %s (hash: %s)\n"
              dep_name (Hasher.to_string hash);
            flush stdout;

            (* Get list of artifacts *)
            let files = Store.list_artifacts store hash in
            Printf.printf "[Sandbox]   Files to copy: %s\n"
              (String.concat ", " files);
            flush stdout;

            (* Promote artifacts from store to sandbox *)
            match Store.promote_from_store store hash sandbox.sandbox_dir with
            | true ->
                Printf.printf
                  "[Sandbox]   - Successfully copied %d files for %s\n"
                  (List.length files) dep_name;
                flush stdout
            | false ->
                Printf.printf
                  "[Sandbox] ERROR: Failed to copy artifacts for %s\n" dep_name;
                flush stdout)
          else (
            Printf.printf
              "[Sandbox] Warning: No cached artifacts found for dependency %s \
               (hash: %s)\n"
              dep_name (Hasher.to_string hash);
            flush stdout)
      | None -> (
          (* No hash available - check why *)
          match Build_results.get_status build_results dep_name with
          | Some Build_results.Building ->
              Printf.printf
                "[Sandbox] Warning: Dependency %s is still building\n" dep_name;
              flush stdout
          | Some Build_results.NotStarted ->
              Printf.printf "[Sandbox] Warning: Dependency %s not started yet\n"
                dep_name;
              flush stdout
          | Some (Build_results.Failed err) ->
              Printf.printf "[Sandbox] Warning: Dependency %s failed: %s\n"
                dep_name err;
              flush stdout
          | _ ->
              Printf.printf
                "[Sandbox] Warning: Dependency %s not available (no hash)\n"
                dep_name;
              flush stdout))
    all_deps

(** Run actions in the sandbox and return output paths *)
let run_actions ~sandbox ~store ~build_graph ~build_results ~node ~session_id =
  let pkg_name = node.Build_node.package.name in

  (* Extract actions from the planned node *)
  let actions =
    match node.Build_node.spec with
    | Build_node.Planned { actions; _ } -> actions
    | Build_node.Unplanned -> []
  in

  Printf.printf "[Sandbox] Running %d actions for %s in %s\n"
    (List.length actions) sandbox.node.package.name sandbox.sandbox_dir;
  flush stdout;

  (* Copy all transitive dependency artifacts into sandbox *)
  copy_dependency_artifacts sandbox ~store ~build_graph ~build_results;

  (* Change to sandbox directory *)
  let original_cwd =
    Fs.getcwd () |> Result.expect ~msg:"Failed to get cwd" |> Path.to_string
  in
  let _ =
    Fs.chdir
      (Path.of_string sandbox.sandbox_dir |> Result.expect ~msg:"Invalid path")
  in
  ();

  (* Track declared outputs *)
  let declared_outputs = ref [] in

  let result =
    try
      let success = ref true in
      let errors = ref [] in

      List.iteri
        (fun i action ->
          (* Log compilation/linking events for LLM visibility *)
          (match action with
          | Actions.CompileInterface { source; _ } ->
              Log.compiling_interface ~session_id
                ~package:sandbox.node.package.name ~file:source
          | Actions.CompileImplementation { source; _ } ->
              Log.compiling_implementation ~session_id
                ~package:sandbox.node.package.name ~file:source
          | Actions.CreateLibrary { output; _ } ->
              Log.linking_library ~session_id ~package:sandbox.node.package.name
                ~output
          | Actions.CreateExecutable { output; _ } ->
              Log.linking_executable ~session_id
                ~package:sandbox.node.package.name ~output
          | _ -> ());
          Printf.printf "[Sandbox] Step %d: %s\n" (i + 1)
            (Actions.string_of_action action);
          flush stdout;

          (* Track declared outputs *)
          (match action with
          | Actions.DeclareOutputs { outputs } -> declared_outputs := outputs
          | _ -> ());

          let result, output =
            Actions.execute_action action node.Build_node.toolchain
          in
          match result with
          | Actions.Success ->
              if output <> "" then (
                Printf.printf "  -> %s\n" output;
                flush stdout)
          | Actions.Skipped reason ->
              Printf.printf "  -> Skipped: %s\n" reason;
              flush stdout
          | Actions.Failed error_msg ->
              success := false;
              errors := error_msg :: !errors;

              (* Parse OCaml compiler error for better reporting *)
              let compile_error =
                match Ocaml_error_parser.get_primary_error error_msg with
                | Some parsed ->
                    Event.
                      {
                        package = sandbox.node.package.name;
                        file = parsed.file;
                        line = parsed.line_start;
                        column = Some parsed.col_start;
                        message = Printf.sprintf "%s: %s" parsed.error_type parsed.message;
                        hint = parsed.hint;
                      }
                | None ->
                    (* Fallback to simple error if parsing fails *)
                    Event.
                      {
                        package = sandbox.node.package.name;
                        file = "";
                        line = 0;
                        column = None;
                        message = String.trim error_msg;
                        hint = None;
                      }
              in
              Log.compile_error ~session_id compile_error;

              Printf.printf "  -> Failed: %s\n" error_msg;
              flush stdout)
        actions;

      if !success then Ok !declared_outputs
      else Error (String.concat "; " !errors)
    with exn ->
      let error_msg =
        Printf.sprintf "Sandbox execution failed: %s" (Printexc.to_string exn)
      in
      Error error_msg
  in

  (* Restore original working directory *)
  let _ =
    Fs.chdir (Path.of_string original_cwd |> Result.expect ~msg:"Invalid path")
  in
  ();

  (* Copy artifacts to target directory *)
  match result with
  | Ok outputs ->
      (* Copy outputs to target directory *)
      List.iter
        (fun output_file ->
          let src = Filename.concat sandbox.sandbox_dir output_file in
          if File_utils.exists ~path:src then (
            let dst = Filename.concat sandbox.target_dir output_file in
            let _ =
              Fs.copy_file
                (Path.of_string src |> Result.expect ~msg:"Invalid src")
                (Path.of_string dst |> Result.expect ~msg:"Invalid dst")
            in
            (* Make executable files executable *)
            if not (String.contains output_file '.') then (
              let _ =
                Fs.chmod
                  (Path.of_string dst |> Result.expect ~msg:"Invalid dst")
                  0o755
              in
              (* Also promote executable to target/<profile>/<name> *)
              let profile_dir =
                Filename.concat (Filename.concat sandbox.root "target") "debug"
              in
              let promoted_dst = Filename.concat profile_dir output_file in
              let _ =
                Fs.copy_file
                  (Path.of_string src |> Result.expect ~msg:"Invalid src")
                  (Path.of_string promoted_dst
                  |> Result.expect ~msg:"Invalid dst")
              in
              let _ =
                Fs.chmod
                  (Path.of_string promoted_dst
                  |> Result.expect ~msg:"Invalid promoted_dst")
                  0o755
              in
              Printf.printf "[Sandbox] Promoted executable %s to %s\n"
                output_file promoted_dst;
              flush stdout);
            Printf.printf "[Sandbox] Copied %s to target\n" output_file;
            flush stdout))
        outputs;
      (* Return paths as Path.t *)
      Ok
        (List.filter_map
           (fun s ->
             match Std.Path.of_string s with Ok p -> Some p | Error _ -> None)
           outputs)
  | Error _ -> result |> Result.map (fun _ -> [])

(** Clean up sandbox directory *)
let cleanup sandbox =
  let _ =
    Fs.remove_dir
      (Path.of_string sandbox.sandbox_dir |> Result.expect ~msg:"Invalid path")
  in
  ()
