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
      | Some Build_results.NotStarted
      | Some Build_results.Building
      | None ->
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
          (* Step 5: Update node spec to Planned *)
          (* For now, use a dummy hash - we'll compute proper hashes later *)
          let dummy_hash = Crypto.Sha256.hash_string "dummy" in
          let outs = [] in (* TODO: extract outputs from actions *)

          node.spec <- Planned { hash = dummy_hash; outs; actions };

          Ok (Planned node)
