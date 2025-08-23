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
let rec get_transitive_dependencies node visited =
  if List.mem node.Build_node.package.name visited then []
  else
    let visited = node.Build_node.package.name :: visited in
    let direct_deps = node.Build_node.deps in
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
          (Std.Path.to_string dep.Build_node.package.relative_path)
      in

      if Miniriot.File.exists ~path:dep_target_dir then (
        Printf.printf "[Sandbox] Copying artifacts from %s\n" dep_name;
        flush stdout;

        (* List files in dependency target directory *)
        let files =
          let all_files =
            Fs.readdir
              (Path.of_string dep_target_dir
              |> Result.expect ~msg:"Invalid dep_target_dir")
            |> Result.expect ~msg:"Failed to read dep_target_dir"
          in
          List.filter
            (fun file ->
              (* Library archive *)
              Filename.check_suffix file ".cma"
              (* ALL compiled interfaces - needed for compilation *)
              || Filename.check_suffix file ".cmi"
              (* C object files *)
              || Filename.check_suffix file ".o")
            all_files
        in
        List.iter
          (fun file ->
            let src = Filename.concat dep_target_dir file in
            let dst = Filename.concat sandbox.sandbox_dir file in

            (* Only copy if destination doesn't exist *)
            if not (Miniriot.File.exists ~path:dst) then (
              let _ =
                Fs.copy_file
                  (Path.of_string src |> Result.expect ~msg:"Invalid src")
                  (Path.of_string dst |> Result.expect ~msg:"Invalid dst")
              in
              ();
              Printf.printf "  -> Copied %s\n" file;
              flush stdout))
          files)
      else (
        Printf.printf
          "[Sandbox] Warning: Dependency %s has no artifacts in %s\n" dep_name
          dep_target_dir;
        flush stdout))
    all_deps

(** Run actions in the sandbox and return output paths *)
let run_actions ~sandbox ~node ~session_id =
  let pkg_name = node.Build_node.package.name in

  (* Extract actions from the planned node *)
  let actions =
    match node.Build_node.spec with
    | Build_node.Planned { actions; _ } -> actions
    | Build_node.Unplanned -> []
  in

  Printf.printf "[Sandbox] Running %d actions for %s in %s\n"
    (List.length actions) sandbox.node.Build_node.package.name
    sandbox.sandbox_dir;
  flush stdout;

  (* Copy all transitive dependency artifacts into sandbox *)
  copy_dependency_artifacts sandbox;

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
              Log.compiling_interface ?sid:session_id
                ~package:sandbox.node.Build_node.package.name ~file:source
          | Actions.CompileImplementation { source; _ } ->
              Log.compiling_implementation ?sid:session_id
                ~package:sandbox.node.Build_node.package.name ~file:source
          | Actions.CreateLibrary { output; _ } ->
              Log.linking_library ?sid:session_id
                ~package:sandbox.node.Build_node.package.name ~output
          | Actions.CreateExecutable { output; _ } ->
              Log.linking_executable ?sid:session_id
                ~package:sandbox.node.Build_node.package.name ~output
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

              (* Log simple compile error for streaming visibility *)
              let compile_error =
                Log.
                  {
                    package = sandbox.node.Build_node.package.name;
                    file = "";
                    line = 0;
                    column = None;
                    message = String.trim error_msg;
                    hint = None;
                  }
              in
              Log.compile_error ?sid:session_id compile_error;

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
          if Miniriot.File.exists ~path:src then (
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
