open Miniriot
(** Build worker process - handles building individual packages *)

open Build_messages

(** Build a package using the Sandbox approach *)
let build_package_with_sandbox build_task =
  let { node; workspace } = build_task in
  let pkg_name = node.Build_node.package.name in
  let pkg_path = node.Build_node.package.path in

  Printf.printf "[Worker] Building package %s at %s\n" pkg_name pkg_path;
  flush stdout;

  try
    (* Create content-addressable store *)
    let store = Store.create ~root_dir:workspace.root in
    
    (* Create sandbox for this build *)
    let sandbox = Sandbox.create ~node ~workspace in

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

    (* Get the hash from the build node, computing it lazily if needed *)
    let node_hash = 
      let hash = Build_graph.get_node_hash build_task.toolchain_version node in
      Printf.printf "[Worker] Computed/retrieved hash %s for %s\n" hash pkg_name;
      Some hash
    in

    let blueprint =
      Actions.generate_blueprint workspace.root pkg_name pkg_path
        node.Build_node.package.relative_path deps all_packages
        build_task.toolchain_version ?hash:node_hash ()
    in

    (* Print the actions *)
    Actions.print_blueprint blueprint;

    (* Run actions in sandbox with store *)
    let result = Sandbox.run_actions ~sandbox ~blueprint ~store in

    (* Clean up sandbox *)
    Sandbox.cleanup sandbox;

    result
  with exn ->
    let error_msg =
      Printf.sprintf "Sandbox build failed: %s" (Printexc.to_string exn)
    in
    Printf.printf "[Worker] %s\n" error_msg;
    flush stdout;
    (false, error_msg)

(** Worker loop that processes build tasks *)
let rec worker_loop server_pid worker_id =
  (* Request next task from the server *)
  send server_pid (NextTask (self ()));

  (* Suspend and wait for Task or Shutdown message *)
  let rec wait_for_work () =
    match receive () with
    | Task build_task -> Some build_task
    | NoTask ->
        (* No tasks available, suspend and wait for server to send us work *)
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
         send server_pid (TaskComplete (pkg_name, false)))
       else
         let success, msg = build_package_with_sandbox build_task in
         Printf.printf "[Worker %d] Build %s for %s: %s\n" worker_id
           (if success then "succeeded" else "failed")
           pkg_name msg;
         flush stdout;
         send server_pid (TaskComplete (pkg_name, success)));

      worker_loop server_pid worker_id

(** Main entry point for worker process *)
let main server_pid worker_id () =
  Printf.printf "[Worker %d] Started (pid: %s)\n" worker_id
    (Pid.to_string (self ()));
  flush stdout;
  worker_loop server_pid worker_id
