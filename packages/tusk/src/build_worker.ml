open Miniriot
(** Build worker process - handles building individual packages *)

open Build_messages

(** Calculate target directory for a package (same logic as Sandbox.create) *)
let get_target_dir workspace node =
  let root = Std.Path.to_string workspace.Workspace.root in
  let target_dir_root = Filename.concat root "target" in
  let debug_dir = Filename.concat target_dir_root "debug" in
  let out_dir = Filename.concat debug_dir "out" in
  Filename.concat out_dir node.Build_node.package.relative_path

(** Result type for build operations *)
type build_result =
  | Success of string (* success message *)
  | Failed of string (* error message *)
  | MissingDependencies of Build_node.t list (* missing dependency nodes *)
  | Cached of string (* cached result message *)

(** Build a package using the Sandbox approach *)
let build_package_with_sandbox server_pid build_task =
  let { node; workspace; session_id } = build_task in
  let pkg_name = node.Build_node.package.name in
  let pkg_path = node.Build_node.package.path in

  Log.package_started ?sid:session_id ~package:pkg_name;

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
        let missing_names =
          List.map (fun dep -> dep.Build_node.package.name) missing_deps
        in
        Log.dependency_missing ?sid:session_id ~package:pkg_name
          ~missing:missing_names;
        (* Send requeue message back to server *)
        send server_pid
          (RequeueWithDependencies { task = build_task; missing_deps });
        MissingDependencies missing_deps
    | Build_graph.Error msg ->
        Log.compile_error ?sid:session_id
          {
            package = pkg_name;
            file = "";
            line = 0;
            column = None;
            message = Printf.sprintf "Error computing hash: %s" msg;
            hint = None;
          };
        Failed msg
    | Build_graph.Ok node_hash ->
        Log.hash_computed ?sid:session_id ~package:pkg_name
          ~hash:(Hasher.to_string node_hash);

        (* Check if we have cached artifacts for this hash *)
        if Store.exists store node_hash then (
          Log.cache_hit ?sid:session_id ~package:pkg_name
            ~hash:(Hasher.to_string node_hash);

          (* Get target directory without creating full sandbox *)
          let target_dir = get_target_dir workspace node in

          (* Promote artifacts from store directly to target *)
          if Store.promote_from_store store node_hash target_dir then
            Cached "Retrieved from cache"
          else Failed "Failed to promote from cache")
        else (
          Log.cache_miss ?sid:session_id ~package:pkg_name
            ~hash:(Hasher.to_string node_hash);

          (* Create sandbox for this build *)
          let sandbox = Sandbox.create ~node ~workspace in

          let blueprint =
            Actions.generate_blueprint workspace node deps all_packages
              Build_node.(node.toolchain)
              ~hash:node_hash ()
          in

          (* Run actions in sandbox with store *)
          let result =
            Sandbox.run_actions ~sandbox ~blueprint ~store ~session_id
          in

          (* Clean up sandbox *)
          Sandbox.cleanup sandbox;

          match result with
          | Sandbox.Success msg -> Success msg
          | Sandbox.Failed msg -> Failed msg
          | Sandbox.Cached msg -> Cached msg)
  with exn ->
    let error_msg =
      Printf.sprintf "Sandbox build failed: %s" (Printexc.to_string exn)
    in
    Printf.printf "[Worker] %s\n" error_msg;
    flush stdout;
    Failed error_msg

(** Worker loop that processes build tasks *)
let rec worker_loop server_pid worker_id =
  (* Request work from the pool *)
  Printf.printf "[Worker %s] Requesting work\n" (Worker_id.to_string worker_id);
  flush stdout;
  send server_pid (NextTask { worker_pid = self () });

  let rec wait_for_work () =
    let selector = function
      | Task build_task -> `select (`task build_task)
      | NoTask -> `select `no_task
      | Shutdown -> `select `shutdown
      | _ -> `skip
    in
    match receive ~selector () with
    | `task build_task -> Some build_task
    | `no_task ->
        (* No tasks available, request again after a short delay *)
        sleep 0.1;
        send server_pid (NextTask { worker_pid = self () });
        wait_for_work ()
    | `shutdown ->
        Printf.printf "[Worker %s] Shutting down\n"
          (Worker_id.to_string worker_id);
        None
  in
  match wait_for_work () with
  | None -> Process.Normal (* Shutdown *)
  | Some build_task ->
      let pkg_name = build_task.node.Build_node.package.name in
      let pkg_path = build_task.node.Build_node.package.path in

      (if not (System.file_exists pkg_path) then (
         Printf.printf "[Worker %s] Package directory not found: %s\n"
           (Worker_id.to_string worker_id)
           pkg_path;
         flush stdout;
         (* For failures, don't compute hash - failed builds should never be cached *)
         send server_pid
           (TaskFailed
              { package_name = pkg_name; error = "Package directory not found" }))
       else
         (* The build_package_with_sandbox function now computes hash internally *)
         let result = build_package_with_sandbox server_pid build_task in
         match result with
         | Success msg -> (
             Printf.printf "[Worker %s] Build succeeded for %s: %s\n"
               (Worker_id.to_string worker_id)
               pkg_name msg;
             flush stdout;
             (* Compute hash and send TaskComplete *)
             let store = Store.create ~root_dir:build_task.workspace.root in
             match
               Build_graph.get_node_hash
                 Build_node.(build_task.node.toolchain)
                 build_task.node store
             with
             | Build_graph.Ok current_hash ->
                 send server_pid
                   (TaskCompleted
                      { package_name = pkg_name; hash = current_hash })
             | Build_graph.MissingDependencies deps ->
                 send server_pid
                   (RequeueWithDependencies
                      { task = build_task; missing_deps = deps })
             | Build_graph.Error err ->
                 send server_pid
                   (TaskFailed { package_name = pkg_name; error = err }))
         | Failed msg ->
             Printf.printf "[Worker %s] Build failed for %s: %s\n"
               (Worker_id.to_string worker_id)
               pkg_name msg;
             (* Log detailed error information for streaming *)
             (* TODO: Parse compiler output and create individual CompileError events *)
             Log.compile_error ?sid:build_task.session_id
               {
                 package = pkg_name;
                 file = "_compilation_summary";
                 line = 0;
                 column = None;
                 message = msg;
                 hint = Some "Check individual compilation errors above";
               };
             flush stdout;
             (* For failures, don't compute hash - failed builds should never be cached *)
             send server_pid
               (TaskFailed { package_name = pkg_name; error = msg })
         | Cached msg -> (
             Printf.printf "[Worker %s] Build cached for %s: %s\n"
               (Worker_id.to_string worker_id)
               pkg_name msg;
             flush stdout;
             (* Compute hash and send TaskComplete *)
             let store = Store.create ~root_dir:build_task.workspace.root in
             match
               Build_graph.get_node_hash
                 Build_node.(build_task.node.toolchain)
                 build_task.node store
             with
             | Build_graph.Ok current_hash ->
                 send server_pid
                   (TaskCompleted
                      { package_name = pkg_name; hash = current_hash })
             | Build_graph.MissingDependencies deps ->
                 send server_pid
                   (RequeueWithDependencies
                      { task = build_task; missing_deps = deps })
             | Build_graph.Error err ->
                 send server_pid
                   (TaskFailed { package_name = pkg_name; error = err }))
         | MissingDependencies deps ->
             let dep_names =
               List.map (fun dep -> dep.Build_node.package.name) deps
             in
             Printf.printf "[Worker %s] Missing dependencies for %s: %s\n"
               (Worker_id.to_string worker_id)
               pkg_name
               (String.concat ", " dep_names);
             flush stdout
         (* Don't send TaskComplete - RequeueWithDependencies already sent *));

      worker_loop server_pid worker_id

(** Main entry point for worker process *)
let main server_pid worker_id () =
  Printf.printf "[Worker %s] Started (pid: %s)\n"
    (Worker_id.to_string worker_id)
    (Pid.to_string (self ()));
  flush stdout;
  worker_loop server_pid worker_id
