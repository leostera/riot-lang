(** Build planner - plans a build node. *)

open Std
open Model

type skip_reason = DependenciesFailed of string list

type plan_result =
  | Planned of Build_node.t
  | MissingDependencies of { node : Build_node.t; deps : Build_node.t list }
  | Skipped of { node : Build_node.t; reason : skip_reason }

type error = string

let plan_node ~graph ~node ~build_results ~workspace ~session_id () =
  (* Step 1: Check if all package-level dependencies are already built *)
  let missing_deps = ref [] in

  List.iter
    (fun dep_id ->
      let dep_node = Build_graph.get_node graph dep_id in
      let dep_name = dep_node.Build_node.package.name in

      match Build_results.get_status build_results dep_name with
      | Some (Build_results.Built _) ->
          (* Dependency is built - good! *)
          ()
      | Some Build_results.NotStarted | Some Build_results.Building | None ->
          (* Dependency not built yet *)
          missing_deps := dep_node :: !missing_deps
      | Some (Build_results.Failed err) ->
          (* Dependency failed - we should skip this node *)
          ())
    node.Build_node.deps;

  (* Step 2: If missing dependencies, return MissingDependencies *)
  if !missing_deps <> [] then
    Ok (MissingDependencies { node; deps = !missing_deps })
  else
    (* Step 3: Check if any dependencies failed *)
    let failed_deps = ref [] in
    List.iter
      (fun dep_id ->
        let dep_node = Build_graph.get_node graph dep_id in
        let dep_name = dep_node.Build_node.package.name in

        match Build_results.get_status build_results dep_name with
        | Some (Build_results.Failed _) ->
            failed_deps := dep_name :: !failed_deps
        | _ -> ())
      node.Build_node.deps;

    if !failed_deps <> [] then
      Ok (Skipped { node; reason = DependenciesFailed !failed_deps })
    else
      (* Step 4: All dependencies satisfied - build module graph and generate actions *)
      match Module_graph.build ~node ~workspace with
      | Error err -> Error err
      | Ok (_module_graph, actions) ->
          (* Step 5: Compute content-based hash *)
          let open Crypto in
          let hasher = Sha256.create () in

          (* Hash 0: Package metadata *)
          Workspace.Package.hash (module Sha256) hasher node.package;

          (* Hash 1: Source file contents *)
          let src_dir =
            Path.(
              workspace.root / Path.v "packages" / Path.v node.package.name
              / Path.v "src")
          in
          let rec hash_source_files dir =
            match Fs.read_dir dir with
            | Error _ -> ()
            | Ok iter ->
                let files = MutIterator.to_list iter in
                List.iter
                  (fun file ->
                    let file_path = Path.(dir / file) in
                    match Fs.is_dir file_path with
                    | Ok true -> hash_source_files file_path
                    | Ok false -> (
                        match Path.extension file_path with
                        | Some ".ml" | Some ".mli" | Some ".c" | Some ".h" -> (
                            match Fs.read_to_string file_path with
                            | Ok content -> Sha256.write_string hasher content
                            | Error _ -> ())
                        | _ -> ())
                    | Error _ -> ())
                  files
          in
          hash_source_files src_dir;

          (* Hash 2: All actions with full parameters *)
          Actions.hash_actions (module Sha256) hasher actions;

          (* Hash 3: Package dependencies (their hashes) *)
          List.iter
            (fun dep_id ->
              let dep_node = Build_graph.get_node graph dep_id in
              match dep_node.spec with
              | Planned { hash; _ } -> Sha256.write hasher (Digest.bytes hash)
              | Unplanned -> ())
            node.deps;

          let hash = Sha256.finish hasher in

          (* Extract outputs from actions *)
          let outs = ref [] in
          List.iter
            (fun action ->
              match action with
              | Actions.CompileInterface { output; _ }
              | Actions.CompileImplementation { output; _ }
              | Actions.GenerateInterface { output; _ }
              | Actions.CompileC { output; _ }
              | Actions.CreateLibrary { output; _ }
              | Actions.CreateExecutable { output; _ } ->
                  outs := Path.v output :: !outs
              | Actions.DeclareOutputs { outputs } ->
                  outs := List.rev_append (List.map Path.v outputs) !outs
              | Actions.CopyFile _ | Actions.WriteFile _ -> ())
            actions;

          node.spec <- Planned { hash; outs = !outs; actions };

          Ok (Planned node)
