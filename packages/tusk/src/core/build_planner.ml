(** Build planner - plans a build node. *)

open Std
open Std.Iter
open Model

type skip_reason = DependenciesFailed of string list

type plan_result =
  | Planned of Build_node.t
  | MissingDependencies of { node : Build_node.t; deps : Build_node.t list }
  | Skipped of { node : Build_node.t; reason : skip_reason }

type error = string

let plan_node ~graph ~node ~build_results ~workspace ~session_id ~sandbox () =
  Log.debug "[BUILD_PLANNER] Planning %s" node.Build_node.package.name;

  (* Step 1: Check if all package-level dependencies are already built *)
  let missing_deps = ref [] in

  List.iter
    (fun dep_id ->
      let dep_node = Build_graph.get_node graph dep_id in
      let dep_name = dep_node.Build_node.package.name in

      match Build_results.get_status build_results dep_name with
      | Some (Build_results.Built _) ->
          (* Dependency is built - good! *)
          Log.debug "[BUILD_PLANNER] Dependency %s is built" dep_name
      | Some Build_results.NotStarted | Some Build_results.Building | None ->
          (* Dependency not built yet *)
          Log.debug "[BUILD_PLANNER] Dependency %s not ready" dep_name;
          missing_deps := dep_node :: !missing_deps
      | Some (Build_results.Failed err) ->
          (* Dependency failed - we should skip this node *)
          Log.debug "[BUILD_PLANNER] Dependency %s failed" dep_name)
    node.Build_node.deps;

  (* Step 2: If missing dependencies, return MissingDependencies *)
  if !missing_deps <> [] then (
    Log.debug "[BUILD_PLANNER] %s has missing dependencies"
      node.Build_node.package.name;
    Ok (MissingDependencies { node; deps = !missing_deps }))
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

    if !failed_deps <> [] then (
      Log.debug "[BUILD_PLANNER] %s skipped - failed deps: %s"
        node.Build_node.package.name
        (String.concat ", " !failed_deps);
      Ok (Skipped { node; reason = DependenciesFailed !failed_deps }))
    else (
      (* Step 4: All dependencies satisfied - build module graph with provided sandbox *)
      Log.debug "[BUILD_PLANNER] Building module graph for %s"
        node.Build_node.package.name;
      match Module_graph.build ~node ~workspace ~build_graph:graph ~sandbox with
      | Error err ->
          Log.debug "[BUILD_PLANNER] Module graph failed for %s: %s"
            node.Build_node.package.name err;
          Error err
      | Ok (_module_graph, actions, outs) ->
          Log.debug "[BUILD_PLANNER] Generated %d actions for %s"
            (List.length actions) node.Build_node.package.name;
          (* Step 5: Compute content-based hash *)
          let open Crypto in
          let hasher = Sha256.create () in

          (* Hash 0: Package metadata *)
          Package.hash (module Sha256) hasher node.package;

          (* Hash 1: Source file contents *)
          let src_dir = Path.(node.package.path / Path.v "src") in
          let rec hash_source_files dir =
            match Fs.read_dir dir with
            | Error _ -> ()
            | Ok iter ->
                let files = MutIterator.to_list iter in
                (* Sort files for deterministic hashing - filesystem order is non-deterministic *)
                let sorted_files =
                  List.sort
                    (fun a b ->
                      String.compare (Path.to_string a) (Path.to_string b))
                    files
                in
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
                  sorted_files
          in
          hash_source_files src_dir;

          (* Hash 2: All actions with full parameters *)
          Actions.hash_actions (module Sha256) hasher actions;

          (* Hash 3: Package dependencies (their hashes) *)
          (* Sort deps by ID for deterministic hashing *)
          let sorted_deps = List.sort Node_id.compare node.deps in
          List.iter
            (fun dep_id ->
              let dep_node = Build_graph.get_node graph dep_id in
              match dep_node.spec with
              | Planned { hash; _ } -> Sha256.write hasher (Digest.bytes hash)
              | Unplanned -> ())
            sorted_deps;

          let hash = Sha256.finish hasher in

          node.spec <- Planned { hash; outs; actions };

          Ok (Planned node))
