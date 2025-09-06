(** Sandbox - isolated build execution environment *)

open Std

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
  let root = Path.to_string workspace.root in
  let target_dir_root = Filename.concat root "target" in
  let debug_dir = Filename.concat target_dir_root "debug" in
  let out_dir = Filename.concat debug_dir "out" in
  let target_dir =
    Filename.concat out_dir
      (Path.to_string node.Build_node.package.relative_path)
  in

  (* Create a unique sandbox directory for this build *)
  let sandbox_id =
    Printf.sprintf "%08x"
      (Hashtbl.hash (node.Build_node.package.name ^ string_of_float (time ())))
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
  Printf.printf "[Sandbox] Copying dependency artifacts...\n%!";
  flush stdout;

  (* Get all transitive dependencies *)
  let all_deps = get_transitive_dependencies sandbox.node ~build_graph [] in

  (* Copy artifacts from each dependency *)
  List.iter
    (fun dep ->
      let dep_name = dep.Build_node.package.name in
      (* Check if this dependency has been built and stored *)
      let dep_hash =
        match Build_results.get_status build_results dep_name with
        | Some (Build_results.Built hash) -> Some hash
        | _ -> None
      in
      match dep_hash with
      | Some hash ->
          if
            (* Check if artifacts exist in the store *)
            Store.exists store hash
          then (
            Printf.printf
              "[Sandbox] Copying artifacts from store for %s (hash: %s)\n%!"
              dep_name (Hasher.to_string hash);
            flush stdout;

            (* Get list of artifacts *)
            let files = Store.list_artifacts store hash in
            Printf.printf "[Sandbox]   Files to copy: %s\n%!"
              (String.concat ", " files);
            flush stdout;

            (* Only copy essential files needed for linking:
               - .cma files (library archives) 
               - .o files (C object files)
               - The main package .cmi file (needed for type checking)
               This prevents "inconsistent assumptions" errors from duplicate .cmi files *)
            let files_to_copy = 
              List.filter (fun file ->
                (* Copy .cma files - the compiled library *)
                Filename.check_suffix file ".cma" ||
                (* Copy .o files for C bindings *)
                Filename.check_suffix file ".o" ||
                (* Only copy the main package interface file *)
                (Filename.check_suffix file ".cmi" && 
                 let base = Filename.chop_suffix file ".cmi" in
                 let safe_name = String.map (fun c -> if c = '-' then '_' else c) dep_name in
                 let capitalized = String.capitalize_ascii safe_name in
                 base = capitalized)
              ) files 
            in
            
            Printf.printf "[Sandbox]   Filtered files to copy: %s\n%!"
              (String.concat ", " files_to_copy);
            
            (* Copy each filtered file individually *)
            List.iter (fun file ->
              let src = Filename.concat (Store.get_hash_dir store hash) file in
              let dst = Filename.concat sandbox.sandbox_dir file in
              match File_utils.copy_file ~src ~dst with
              | Ok () -> ()
              | Error _ ->
                  failwith (Printf.sprintf "Failed to copy %s to %s" src dst)
            ) files_to_copy;
            
            Printf.printf
              "[Sandbox]   - Successfully copied %d files for %s\n%!"
              (List.length files_to_copy) dep_name;
            flush stdout)
          else
            failwith
              (Printf.sprintf
                 "Missing cached artifacts for dependency %s (hash: %s)"
                 dep_name (Hasher.to_string hash))
      | None -> (
          (* No hash available - check why *)
          match Build_results.get_status build_results dep_name with
          | Some Build_results.Building ->
              failwith
                (Printf.sprintf "Dependency %s is still building" dep_name)
          | Some Build_results.NotStarted ->
              failwith (Printf.sprintf "Dependency %s not started yet" dep_name)
          | Some (Build_results.Failed err) ->
              failwith (Printf.sprintf "Dependency %s failed: %s" dep_name err)
          | _ ->
              failwith
                (Printf.sprintf "Dependency %s not available (no hash)" dep_name)
          ))
    all_deps

(** Run actions in the sandbox and return output paths *)
let run_actions ~sandbox ~store ~build_graph ~build_results ~node ~session_id =
  (* Extract actions from the planned node *)
  let actions =
    match node.Build_node.spec with
    | Build_node.Planned { actions; _ } -> actions
    | Build_node.Unplanned -> []
  in

  Printf.printf "[Sandbox] Running %d actions for %s in %s\n%!"
    (List.length actions) sandbox.node.package.name sandbox.sandbox_dir;
  flush stdout;

  (* Copy all transitive dependency artifacts into sandbox *)
  copy_dependency_artifacts sandbox ~store ~build_graph ~build_results;
  
  (* Copy unix.cma if needed - it's not in our Store *)
  if List.exists (fun (dep : Workspace.dependency) -> dep.name = "unix") 
       sandbox.node.package.dependencies then (
    let toolchain_path = Toolchains.get_toolchain_path sandbox.node.toolchain in
    let unix_cma = Filename.concat toolchain_path "lib/ocaml/unix.cma" in
    let unix_exists = 
      match Fs.file_exists (Path.of_string unix_cma |> Result.expect ~msg:"Invalid path") with
      | Ok exists -> exists
      | Error _ -> false
    in
    if unix_exists then (
      Printf.printf "[Sandbox] Copying unix.cma from toolchain\n%!";
      match File_utils.read ~path:unix_cma with
      | Ok content ->
          let _ = File_utils.write ~path:(Filename.concat sandbox.sandbox_dir "unix.cma") ~content in
          ()
      | Error _ -> ()
    )
  );

  (* Change to sandbox directory *)
  let original_cwd =
    Fs.getcwd () |> Result.expect ~msg:"Failed to get cwd" |> Path.to_string
  in
  let _ =
    Fs.chdir
      (Path.of_string sandbox.sandbox_dir |> Result.expect ~msg:"Invalid path")
  in

  (* Track declared outputs *)
  let declared_outputs = ref [] in

  let result =
    try
      let success = ref true in
      let errors = ref [] in

      (* Use exception to break out of iteration on first failure *)
      (try
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
                 Log.linking_library ~session_id
                   ~package:sandbox.node.package.name ~output
             | Actions.CreateExecutable { output; _ } ->
                 Log.linking_executable ~session_id
                   ~package:sandbox.node.package.name ~output
             | _ -> ());
             Printf.printf "[Sandbox] Step %d: %s\n%!" (i + 1)
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
                   Printf.printf "  -> %s\n%!" output;
                   flush stdout)
             | Actions.Skipped reason ->
                 Printf.printf "  -> Skipped: %s\n%!" reason;
                 flush stdout
             | Actions.Failed error_msg ->
                 (* Check if this is a fatal build system error *)
                 if String.starts_with ~prefix:"Missing outputs:" error_msg then (
                   (* Fatal build system error - don't continue *)
                   Printf.eprintf "\n\027[1;31mFATAL BUILD ERROR:\027[0m %s\n"
                     error_msg;
                   Printf.eprintf
                     "The build system detected missing expected outputs.\n";
                   Printf.eprintf
                     "This indicates a serious internal error. Build halted.\n\
                      %!";
                   (* Instead of exit, raise an exception that can be caught at higher level *)
                   failwith ("FATAL_BUILD_ERROR: " ^ error_msg))
                 else (
                   success := false;
                   errors := error_msg :: !errors;

                   (* Parse OCaml compiler error for better reporting *)
                   let compile_error =
                     match Ocaml_error_parser.get_primary_error error_msg with
                     | Some parsed ->
                         let error_kind =
                           match parsed.error with
                           | Ocaml_error_parser.SyntaxError -> Event.SyntaxError
                           | Ocaml_error_parser.TypeError s ->
                               Event.TypeError { description = s }
                           | Ocaml_error_parser.UnboundValue v ->
                               Event.UnboundValue { name = v }
                           | Ocaml_error_parser.UnboundModule m ->
                               Event.UnboundModule { name = m }
                           | Ocaml_error_parser.FileNotFound f ->
                               Event.FileNotFound { filename = f }
                           | Ocaml_error_parser.OtherError e ->
                               Event.OtherError { message = e }
                         in
                         Event.
                           {
                             file = parsed.file;
                             line = parsed.line;
                             span = parsed.span;
                             hint = parsed.hint;
                             kind = error_kind;
                             raw = parsed.raw;
                           }
                     | None ->
                         (* Fallback to simple error if parsing fails *)
                         Event.
                           {
                             file = "_unknown_";
                             line = 1;
                             span = (0, 0);
                             hint = "";
                             kind =
                               Event.OtherError
                                 { message = String.trim error_msg };
                             raw = error_msg;
                           }
                   in
                   Log.compile_error ~session_id
                     ~package:sandbox.node.package.name compile_error;

                   Printf.printf "  -> Failed: %s\n%!" error_msg;
                   flush stdout;
                   (* Stop processing further actions on failure *)
                   raise Exit))
           actions
       with Exit -> ());

      if !success then
        if !declared_outputs = [] then
          Error
            "No DeclareOutputs action found - build_planner must declare \
             expected outputs!"
        else Ok !declared_outputs
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
              Printf.printf "[Sandbox] Promoted executable %s to %s\n%!"
                output_file promoted_dst;
              flush stdout);
            Printf.printf "[Sandbox] Copied %s to target\n%!" output_file;
            flush stdout))
        outputs;
      (* Return paths as Path.t *)
      Ok
        (List.filter_map
           (fun s ->
             match Path.of_string s with Ok p -> Some p | Error _ -> None)
           outputs)
  | Error _ -> result |> Result.map (fun _ -> [])

(** Clean up sandbox directory *)
let cleanup sandbox =
  let _ =
    Fs.remove_dir
      (Path.of_string sandbox.sandbox_dir |> Result.expect ~msg:"Invalid path")
  in
  ()
