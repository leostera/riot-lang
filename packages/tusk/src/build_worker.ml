open Miniriot
(** Build worker process - handles building individual packages *)

open Build_messages

(** Result type for build operations *)
type build_result = 
  | Success of string  (* success message *)
  | Failed of string   (* error message *)
  | MissingDependencies of Build_node.t list  (* missing dependency nodes *)
  | Cached of string   (* cached result message *)

(** Build a package using the Sandbox approach *)
let build_package_with_sandbox server_pid build_task =
  let { node; workspace } = build_task in
  let pkg_name = node.Build_node.package.name in
  let pkg_path = node.Build_node.package.path in

  Printf.printf "[Worker] Building package %s at %s\n" pkg_name pkg_path;
  flush stdout;

  try
    (* Generate build actions *)
    let deps =
      List.map
        (fun dep ->
          Actions.
            {
              name = dep.Build_node.package.name;
              relative_path = dep.Build_node.package.relative_path;
              dependencies = dep.Build_node.package.dependencies;
            })
        node.Build_node.dependencies
    in

    (* Convert all packages to dep_info for transitive dependency resolution *)
    let all_packages =
      List.map
        (fun pkg ->
          Actions.
            {
              name = pkg.Workspace.name;
              relative_path = pkg.Workspace.relative_path;
              dependencies = pkg.Workspace.dependencies;
            })
        workspace.packages
    in

    (* Create content-addressable store *)
    let store = Store.create ~root_dir:workspace.root in
    
    (* Compute hash for this task (recursively through dependencies) *)
    match Build_graph.get_node_hash Build_node.(node.toolchain) node store with
    | Build_graph.MissingDependencies missing_deps ->
        let missing_names = List.map (fun dep -> dep.Build_node.package.name) missing_deps in
        Printf.printf "[Worker] Missing dependencies for %s: %s\n" pkg_name (String.concat ", " missing_names);
        flush stdout;
        (* Send requeue message back to server *)
        send server_pid (RequeueWithDependencies (build_task, missing_deps));
        MissingDependencies missing_deps
    | Build_graph.Error msg ->
        Printf.printf "[Worker] Error computing hash for %s: %s\n" pkg_name msg;
        flush stdout;
        Failed msg
    | Build_graph.Ok node_hash ->
        Printf.printf "[Worker] Computed hash %s for %s\n" (Hasher.to_string node_hash) pkg_name;
        
        (* Check if we have cached artifacts for this hash *)
        if Store.exists store node_hash then (
          Printf.printf "[Worker] Cache hit for %s (hash: %s)\n" pkg_name (Hasher.to_string node_hash);
          flush stdout;
          
          (* Create sandbox for this build to get target directory *)
          let sandbox = Sandbox.create ~node ~workspace in
          
          (* Promote artifacts from store directly to target *)
          if Store.promote_from_store store node_hash sandbox.target_dir then (
            Sandbox.cleanup sandbox;
            Cached "Retrieved from cache"
          ) else (
            Sandbox.cleanup sandbox;
            Failed "Failed to promote from cache"
          )
        ) else (
          Printf.printf "[Worker] Cache miss for %s (hash: %s), building...\n" pkg_name (Hasher.to_string node_hash);
          flush stdout;
          
          (* Create sandbox for this build *)
          let sandbox = Sandbox.create ~node ~workspace in

          let blueprint =
            Actions.generate_blueprint workspace node deps all_packages
              Build_node.(node.toolchain) ~hash:node_hash ()
          in

          (* Run actions in sandbox with store *)
          let success, msg = Sandbox.run_actions ~sandbox ~blueprint ~store in

          (* Clean up sandbox *)
          Sandbox.cleanup sandbox;

          if success then Success msg else Failed msg
        )
  with exn ->
    let error_msg =
      Printf.sprintf "Sandbox build failed: %s" (Printexc.to_string exn)
    in
    Printf.printf "[Worker] %s\n" error_msg;
    flush stdout;
    Failed error_msg

(** Worker loop that processes build tasks *)
let rec worker_loop server_pid worker_id =
  (* Workers are pre-added to idle queue, so just wait for work *)
  let rec wait_for_work () =
    match receive () with
    | Task build_task -> Some build_task
    | NoTask ->
        (* No tasks available, request again after a short delay *)
        sleep 0.1;
        send server_pid (NextTask (self ()));
        wait_for_work ()
    | Shutdown ->
        Printf.printf "[Worker %d] Shutting down\n" worker_id;
        None
    | _ ->
        (* Ignore other messages and keep waiting *)
        wait_for_work ()
  in
  match wait_for_work () with
  | None -> Process.Normal (* Shutdown *)
  | Some build_task ->
      let pkg_name = build_task.node.Build_node.package.name in
      let pkg_path = build_task.node.Build_node.package.path in

      (if not (System.file_exists pkg_path) then (
         Printf.printf "[Worker %d] Package directory not found: %s\n" worker_id
           pkg_path;
         flush stdout;
         (* Even for failures, we need to compute the hash *)
         let store = Store.create ~root_dir:build_task.workspace.root in
         match Build_graph.get_node_hash Build_node.(build_task.node.toolchain) build_task.node store with
         | Build_graph.Ok current_hash -> send server_pid (TaskComplete (pkg_name, false, current_hash))
         | Build_graph.MissingDependencies deps -> send server_pid (RequeueWithDependencies (build_task, deps))
         | Build_graph.Error _ -> send server_pid (TaskComplete (pkg_name, false, Hasher.of_string "error")))
       else
         (* The build_package_with_sandbox function now computes hash internally *)
         let result = build_package_with_sandbox server_pid build_task in
         match result with
         | Success msg -> 
             Printf.printf "[Worker %d] Build succeeded for %s: %s\n" worker_id pkg_name msg;
             flush stdout;
             (* Compute hash and send TaskComplete *)
             let store = Store.create ~root_dir:build_task.workspace.root in
             (match Build_graph.get_node_hash Build_node.(build_task.node.toolchain) build_task.node store with
             | Build_graph.Ok current_hash -> send server_pid (TaskComplete (pkg_name, true, current_hash))
             | Build_graph.MissingDependencies deps -> send server_pid (RequeueWithDependencies (build_task, deps))
             | Build_graph.Error _ -> send server_pid (TaskComplete (pkg_name, false, Hasher.of_string "error")))
         | Failed msg ->
             Printf.printf "[Worker %d] Build failed for %s: %s\n" worker_id pkg_name msg;
             flush stdout;
             (* Compute hash and send TaskComplete *)
             let store = Store.create ~root_dir:build_task.workspace.root in
             (match Build_graph.get_node_hash Build_node.(build_task.node.toolchain) build_task.node store with
             | Build_graph.Ok current_hash -> send server_pid (TaskComplete (pkg_name, false, current_hash))
             | Build_graph.MissingDependencies deps -> send server_pid (RequeueWithDependencies (build_task, deps))
             | Build_graph.Error _ -> send server_pid (TaskComplete (pkg_name, false, Hasher.of_string "error")))
         | Cached msg ->
             Printf.printf "[Worker %d] Build cached for %s: %s\n" worker_id pkg_name msg;
             flush stdout;
             (* Compute hash and send TaskComplete *)
             let store = Store.create ~root_dir:build_task.workspace.root in
             (match Build_graph.get_node_hash Build_node.(build_task.node.toolchain) build_task.node store with
             | Build_graph.Ok current_hash -> send server_pid (TaskComplete (pkg_name, true, current_hash))
             | Build_graph.MissingDependencies deps -> send server_pid (RequeueWithDependencies (build_task, deps))
             | Build_graph.Error _ -> send server_pid (TaskComplete (pkg_name, false, Hasher.of_string "error")))
         | MissingDependencies deps ->
             let dep_names = List.map (fun dep -> dep.Build_node.package.name) deps in
             Printf.printf "[Worker %d] Missing dependencies for %s: %s\n" worker_id pkg_name (String.concat ", " dep_names);
             flush stdout;
             (* Don't send TaskComplete - RequeueWithDependencies already sent *));

      worker_loop server_pid worker_id

(** Main entry point for worker process *)
let main server_pid worker_id () =
  Printf.printf "[Worker %d] Started (pid: %s)\n" worker_id
    (Pid.to_string (self ()));
  flush stdout;
  worker_loop server_pid worker_id
