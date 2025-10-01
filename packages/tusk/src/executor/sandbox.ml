(** Sandbox - isolated build execution environment *)

open Std
open Core
open Ocaml
open Model

type t = {
  root : Path.t;
  sandbox_dir : Path.t;
  target_dir : Path.t;
  node : Build_node.t;
  workspace : Workspace.t;
}

type error = string

(** Create a new sandbox for a build graph node *)
let create ~node ~(workspace : Workspace.t) =
  let root = workspace.root in
  let target_dir_root = Path.(root / Path.v "target") in
  let debug_dir = Path.(target_dir_root / Path.v "debug") in
  let out_dir = Path.(debug_dir / Path.v "out") in
  let target_dir = Path.(out_dir / node.Build_node.package.relative_path) in

  (* Create a unique sandbox directory for this build *)
  let now = Time.Instant.now () in
  let nanos = Time.Instant.elapsed now |> Time.Duration.to_nanos in
  let sandbox_id =
    Printf.sprintf "%08x"
      (Hashtbl.hash (node.Build_node.package.name ^ string_of_int nanos))
  in
  let sandbox_dir = Path.(debug_dir / Path.v "sandbox" / Path.v sandbox_id) in

  (* Create directories *)
  let _ = Fs.create_dir_all target_dir_root in
  let _ = Fs.create_dir_all sandbox_dir in
  let _ = Fs.create_dir_all target_dir in
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
              dep_name
              (Std.Crypto.Digest.hex hash);

            (* Read manifest to get list of files to copy *)
            let manifest_path =
              Path.(Store.get_hash_dir store hash / Path.v "manifest.json")
            in
            let files_to_copy =
              match Store.Manifest.load ~path:manifest_path with
              | Ok manifest ->
                  (* Get ALL file paths from manifest - these are exactly what was declared as outputs *)
                  let manifest_files =
                    List.map
                      (fun entry -> Store.Manifest.(entry.path))
                      manifest.files
                  in
                  Printf.printf "[Sandbox]   Files from manifest: %s\n%!"
                    (String.concat ", " manifest_files);
                  manifest_files
              | Error msg ->
                  (* Missing manifest is a store error - should never happen *)
                  failwith
                    (Printf.sprintf
                       "Store integrity error: missing manifest for %s (hash: \
                        %s): %s"
                       dep_name
                       (Std.Crypto.Digest.hex hash)
                       msg)
            in

            (* Copy each file individually *)
            List.iter
              (fun file ->
                let src = Path.(Store.get_hash_dir store hash / Path.v file) in
                let dst_path = Path.(sandbox.sandbox_dir / Path.v file) in
                match Fs.copy ~src ~dst:dst_path with
                | Ok () -> ()
                | Error _ ->
                    failwith
                      (Printf.sprintf "Failed to copy %s to %s"
                         (Path.to_string src) (Path.to_string dst_path)))
              files_to_copy;

            Printf.printf
              "[Sandbox]   - Successfully copied %d files for %s\n%!"
              (List.length files_to_copy)
              dep_name)
          else
            failwith
              (Printf.sprintf
                 "Missing cached artifacts for dependency %s (hash: %s)"
                 dep_name
                 (Std.Crypto.Digest.hex hash))
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
    (List.length actions) sandbox.node.package.name
    (Path.to_string sandbox.sandbox_dir);

  (* Copy all transitive dependency artifacts into sandbox *)
  copy_dependency_artifacts sandbox ~store ~build_graph ~build_results;

  (* Copy unix.cma if needed - it's not in our Store *)
  (if
     List.exists
       (fun (dep : Workspace.dependency) -> dep.name = "unix")
       sandbox.node.package.dependencies
   then
     let toolchain_path =
       Toolchains.get_toolchain_path sandbox.node.toolchain
     in
     let unix_cma =
       Path.(toolchain_path / Path.v "lib" / Path.v "ocaml" / Path.v "unix.cma")
     in
     let unix_exists =
       match Fs.exists unix_cma with Ok exists -> exists | Error _ -> false
     in
     if unix_exists then (
       Printf.printf "[Sandbox] Copying unix.cma from toolchain\n%!";
       let dst_path = Path.(sandbox.sandbox_dir / Path.v "unix.cma") in
       match Fs.copy ~src:unix_cma ~dst:dst_path with
       | Ok () -> ()
       | Error _ -> ()));

  (* Change to sandbox directory *)
  let original_cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get cwd"
  in
  let _ = Env.set_current_dir sandbox.sandbox_dir in

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
                   ~package:sandbox.node.package.name
                   ~file:(Path.to_string source)
             | Actions.CompileImplementation { source; _ } ->
                 Log.compiling_implementation ~session_id
                   ~package:sandbox.node.package.name
                   ~file:(Path.to_string source)
             | Actions.CreateLibrary { output; _ } ->
                 Log.linking_library ~session_id
                   ~package:sandbox.node.package.name
                   ~output:(Path.to_string output)
             | Actions.CreateExecutable { output; _ } ->
                 Log.linking_executable ~session_id
                   ~package:sandbox.node.package.name
                   ~output:(Path.to_string output)
             | _ -> ());
             Printf.printf "[Sandbox] Step %d: %s\n%!" (i + 1)
               (Actions.string_of_action action);

             (* Track declared outputs *)
             (match action with
             | Actions.DeclareOutputs { outputs } -> declared_outputs := outputs
             | _ -> ());

             let result, output =
               Actions.execute_action action node.Build_node.toolchain
             in
             match result with
             | Actions.Success ->
                 if output <> "" then Printf.printf "  -> %s\n%!" output
             | Actions.Skipped reason ->
                 Printf.printf "  -> Skipped: %s\n%!" reason
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
  let _ = Env.set_current_dir original_cwd in

  (* Copy artifacts to target directory *)
  match result with
  | Ok outputs ->
      (* Copy outputs to target directory *)
      List.iter
        (fun output_file ->
          let src_path = Path.(sandbox.sandbox_dir / output_file) in
          let exists =
            Fs.exists src_path
            |> Result.expect
                 ~msg:
                   (Printf.sprintf "Failed to check if output file exists: %s"
                      (Path.to_string src_path))
          in
          if not exists then
            failwith
              (Printf.sprintf
                 "Missing declared output file: %s (expected at %s)"
                 (Path.to_string output_file)
                 (Path.to_string src_path));

          let dst_path = Path.(sandbox.target_dir / output_file) in
          let _ = Fs.copy ~src:src_path ~dst:dst_path in
          (* Make executable files executable *)
          (if not (String.contains (Path.to_string output_file) '.') then
             let _ =
               Fs.set_permissions dst_path (Fs.Permissions.of_mode 0o755)
             in
             (* Also promote executable to target/<profile>/<name> *)
             let profile_dir =
               Path.(sandbox.root / Path.v "target" / Path.v "debug")
             in
             let promoted_dst_path = Path.(profile_dir / output_file) in
             let _ = Fs.copy ~src:src_path ~dst:promoted_dst_path in
             let _ =
               Fs.set_permissions promoted_dst_path
                 (Fs.Permissions.of_mode 0o755)
             in
             Printf.printf "[Sandbox] Promoted executable %s to %s\n%!"
               (Path.to_string output_file)
               (Path.to_string promoted_dst_path));
          Printf.printf "[Sandbox] Copied %s to target\n%!"
            (Path.to_string output_file))
        outputs;
      Ok outputs
  | Error _ -> result |> Result.map (fun _ -> [])

(** Clean up sandbox directory *)
let cleanup sandbox =
  (* let _ = Fs.remove_dir_all sandbox.sandbox_dir in *)
  ()
