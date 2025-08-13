(** Build server - Miniriot process that orchestrates builds *)

open Miniriot
open Build_messages
open Build_node

(** Import shared RPC message types *)
open Rpc_messages

type state = {
  workspace : Workspace.workspace option;
  build_graph : Build_graph.t option; (* Full workspace build graph *)
  active_build_graph : Build_graph.t option; (* Graph for current build (full or filtered) *)
  build_results : Build_results.t;
  worker_pool : Worker_pool.t option; (* Handle to the worker pool *)
  build_queue : Build_queue.t; (* Two-queue system for dependency ordering *)
  client_pid : Pid.t option; (* PID of client (CLI or RPC) to respond to *)
  toolchain : Toolchains.toolchain; (* Current toolchain *)
}
(** Server state *)


(** Get package dependencies from workspace *)
let get_package_deps state pkg_name =
  match state.workspace with
  | None -> []
  | Some workspace -> (
      match
        List.find_opt (fun p -> p.Workspace.name = pkg_name) workspace.packages
      with
      | None -> []
      | Some pkg -> pkg.dependencies)

(** Queue a package for building if not already tracked as building or built *)
(* Internal version - uses cached hashes, for use during build *)
let queue_package_if_needed_internal state node is_ready =
  let pkg_name = node.Build_node.package.name in
  (* Don't re-queue if already building, failed, built, or busy *)
  if Build_queue.is_busy state.build_queue pkg_name then
    Printf.printf "[Server] Skipping %s - already busy\n" pkg_name
  else
    match Build_results.get_status state.build_results pkg_name with
    | Some Build_results.Building ->
        Printf.printf "[Server] Skipping %s - already building\n" pkg_name
    | Some (Build_results.Failed _) ->
        Printf.printf "[Server] Skipping %s - already failed\n" pkg_name
    | Some (Build_results.Built _) ->
        (* Already built in this session - don't recheck *)
        Printf.printf "[Server] Skipping %s - already built in this session\n" pkg_name
    | Some Build_results.NotStarted | None ->
        (* Not started - queue it *)
        Printf.printf "[Server] Queueing package: %s\n" pkg_name;
        let task = { Build_messages.node; workspace = Option.get state.workspace } in
        if is_ready then
          Build_queue.add_ready state.build_queue task
        else
          Build_queue.add_waiting state.build_queue task

(** Queue a package at entry point - checks for hash changes *)
let queue_package_if_needed state node is_ready =
  let pkg_name = node.Build_node.package.name in
  (* Don't re-queue if already busy *)
  if Build_queue.is_busy state.build_queue pkg_name then
    Printf.printf "[Server] Skipping %s - already busy\n" pkg_name
  else
    (* Don't re-queue if already building or failed *)
    match Build_results.get_status state.build_results pkg_name with
    | Some Build_results.Building ->
        Printf.printf "[Server] Skipping %s - already building\n" pkg_name
    | Some (Build_results.Failed _) ->
        Printf.printf "[Server] Skipping %s - already failed\n" pkg_name
    | Some (Build_results.Built stored_hash) ->
        (* Check if the current hash matches the stored hash *)
        (match state.workspace with
        | Some workspace ->
            (* Force recomputation of hash to detect changes *)
            (match Build_graph.recompute_node_hash state.toolchain node with
            | Build_graph.Ok current_hash ->
                if Hasher.equal stored_hash current_hash then
                  Printf.printf "[Server] Skipping %s - already built with same hash\n" pkg_name
                else (
                  Printf.printf "[Server] Package %s hash changed (was: %s, now: %s), queueing for rebuild\n" 
                    pkg_name (Hasher.to_string stored_hash) (Hasher.to_string current_hash);
                  (* Reset the build status since hash changed *)
                  Build_results.init_package state.build_results pkg_name;
                  let task = { Build_messages.node; workspace } in
                  if is_ready then
                    Build_queue.add_ready state.build_queue task
                  else
                    Build_queue.add_waiting state.build_queue task)
            | _ ->
                (* Can't compute hash, queue it to be safe *)
                Printf.printf "[Server] Queueing package: %s (couldn't compute hash)\n" pkg_name;
                let task = { Build_messages.node; workspace } in
                if is_ready then
                  Build_queue.add_ready state.build_queue task
                else
                  Build_queue.add_waiting state.build_queue task)
        | None ->
            (* No workspace, skip it *)
            Printf.printf "[Server] Skipping %s - no workspace\n" pkg_name)
    | Some Build_results.NotStarted | None ->
        (* Not started - queue it *)
        Printf.printf "[Server] Queueing package: %s\n" pkg_name;
        let task = { Build_messages.node; workspace = Option.get state.workspace } in
        if is_ready then
          Build_queue.add_ready state.build_queue task
        else
          Build_queue.add_waiting state.build_queue task

(** Check if a package can be built (all deps ready) *)
let can_build_package state pkg_name =
  match state.active_build_graph with
  | None -> false
  | Some graph -> (
      let nodes = Build_graph.topological_sort graph in
      match
        List.find_opt (fun n -> n.Build_node.package.name = pkg_name) nodes
      with
      | None -> false
      | Some node ->
          let deps =
            List.map (fun dep -> dep.Build_node.package.name) node.dependencies
          in
          Build_results.dependencies_ready state.build_results deps)

(** Try to get next buildable task *)
let rec get_next_buildable_task state =
  match (state.active_build_graph, state.workspace) with
  | Some graph, Some workspace ->
      let nodes = Build_graph.topological_sort graph in
      
      (* Helper to check if a package should be built *)
      let should_build_package pkg_name node =
        if not (can_build_package state pkg_name) then false
        else
          (* Compute hash and check if already built with this hash *)
          let store = Store.create ~root_dir:workspace.root in
          match Build_graph.get_node_hash state.toolchain node store with
          | Build_graph.Ok hash ->
              not (Build_results.is_built_with_current_hash state.build_results pkg_name hash)
          | _ -> true  (* If we can't compute hash, try to build anyway *)
      in

      (* Try to get a ready task from the queue *)
      (match Build_queue.take_ready state.build_queue with
      | Some task ->
          let pkg_name = task.Build_messages.node.Build_node.package.name in
          let node = task.Build_messages.node in
          if should_build_package pkg_name node then
            Some task
          else (
            (* Either not buildable yet or already built - try next *)
            if not (can_build_package state pkg_name) then (
              (* Move back to waiting queue and mark as not busy *)
              Build_queue.add_waiting state.build_queue task;
              Build_queue.mark_completed state.build_queue pkg_name;
            ) else (
              (* Already built - just mark as not busy *)
              Build_queue.mark_completed state.build_queue pkg_name;
            );
            get_next_buildable_task state)
      | None -> None)  (* No work available *)
  | _, _ -> None

(** Check if a package can be built (all deps ready) *)

(** Check later queue and move ready packages to current queue *)
let check_later_queue state =
  match state.active_build_graph with
  | None -> ()
  | Some graph ->
      let nodes = Build_graph.topological_sort graph in
      List.iter (fun node ->
        let pkg_name = node.Build_node.package.name in
        if Build_queue.is_waiting state.build_queue pkg_name then (
          if can_build_package state pkg_name then (
            Printf.printf "[Server] Moving %s from waiting to ready queue\n" pkg_name;
            Build_queue.move_to_ready state.build_queue pkg_name
          )
        )
      ) nodes

(** Try to assign work to workers *)
let try_assign_work state =
  match state.worker_pool with
  | None -> ()
  | Some pool ->
      (* First check for tasks that can be promoted from waiting to ready *)
      check_later_queue state;
      
      (* Then send one batch of ready tasks *)
      let rec send_ready_tasks () =
        match get_next_buildable_task state with
        | Some build_task ->
            let pkg_name = build_task.node.Build_node.package.name in
            Printf.printf "[Server] Sending package %s to worker pool\n" pkg_name;
            Build_results.mark_building state.build_results pkg_name;
            Worker_pool.send_task pool build_task;
            send_ready_tasks ()
        | None -> ()
      in
      send_ready_tasks ()

(** Unified build execution handler *)
let execute_build state client_pid build_graph =
  Printf.printf "[Server] Starting build execution...\n";
  
  (* Get all packages in topological order *)
  let sorted = Build_graph.topological_sort build_graph in
  
  Printf.printf "[Server] All packages in dependency order: ";
  List.iter (fun node ->
    Printf.printf "%s " node.Build_node.package.name
  ) sorted;
  Printf.printf "\n";
  
  (* Initialize build results for all packages *)
  List.iter (fun node ->
    let pkg_name = node.Build_node.package.name in
    if not (Build_results.is_tracked state.build_results pkg_name) then
      Build_results.init_package state.build_results pkg_name
  ) sorted;

  (* Queue all packages - current if ready, later if waiting on deps *)
  List.iter
    (fun node ->
      let pkg_name = node.Build_node.package.name in
      let is_ready = can_build_package state pkg_name in
      if not is_ready then
        Printf.printf "[Server] Package %s has unmet dependencies, adding to later queue\n" pkg_name;
      queue_package_if_needed state node is_ready)
    sorted;

  let (ready_count, waiting_count, busy_count) = Build_queue.stats state.build_queue in
  Printf.printf "[Server] Queue stats - ready: %d, waiting: %d, busy: %d\n" ready_count waiting_count busy_count;

  (* Check if there's actually any work to do *)
  if ready_count = 0 && waiting_count = 0 && busy_count = 0 then (
    (* All packages already built - return success immediately *)
    Printf.printf "[Server] All packages already built!\n";
    let built_count = List.length sorted in
    send client_pid (ServerResponse (Rpc.BuildComplete { successful = built_count; failed = 0 }));
    (* Reset state for next build *)
    let fresh_build_queue = Build_queue.create state.build_results in
    { state with 
      client_pid = None;
      active_build_graph = None;
      build_queue = fresh_build_queue;
    }
  ) else (
    (* Start worker pool if not already started *)
    let pool = match state.worker_pool with
    | Some pool -> pool
    | None -> 
        let pool = Worker_pool.start ~listener:(self ()) () in
        Printf.printf "[Server] Started worker pool\n";
        pool
    in
    let new_state = { state with client_pid = Some client_pid; worker_pool = Some pool } in
    (* Try to assign initial work to workers *)
    try_assign_work new_state;
    new_state
  )


(** Handle task completion *)
let handle_task_complete state pkg_name success hash =
  (* Mark task as no longer busy *)
  Build_queue.mark_completed state.build_queue pkg_name;
  
  if success then (
    Printf.printf "[Server] Build complete: %s (handling completion...)\n" pkg_name;
    Build_results.mark_built_with_hash state.build_results pkg_name hash;

    (* Also check if any other packages are now unblocked *)
    (match state.active_build_graph with
    | Some graph ->
        let nodes = Build_graph.topological_sort graph in
        Printf.printf "[Server] Checking for newly unblocked packages after %s completed...\n" pkg_name;
        List.iter
          (fun node ->
            let pkg = node.Build_node.package.name in
            let is_building = Build_results.is_building state.build_results pkg in
            let can_build = can_build_package state pkg in
            Printf.printf "[Server]   Package %s: is_building=%b, can_build=%b\n" pkg is_building can_build;
            if (not is_building) && can_build then
              queue_package_if_needed_internal state node true)
          nodes
    | None ->
        ());
    try_assign_work state
  ) else (
    Printf.printf "[Server] Build failed: %s\n" pkg_name;
    Build_results.mark_failed state.build_results pkg_name "Build failed")

(** Generate .merlin file for LSP configuration *)
let generate_merlin_file workspace toolchain =
  let root = System.getcwd () in
  let profile = "debug" in
  (* TODO: make this configurable *)
  let target_dir = Filename.concat root (Printf.sprintf "target/%s" profile) in
  let merlin_path = Filename.concat target_dir ".merlin" in
  let home = System.get_home () in
  let stdlib_path =
    Printf.sprintf "%s/.tusk/toolchains/%s/lib/ocaml" home (Toolchains.get_version toolchain)
  in

  (* Ensure target directories exist *)
  let target_root = Filename.concat root "target" in
  (try System.mkdir target_root 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (try System.mkdir target_dir 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  (* Generate .merlin content with proper paths *)
  let merlin_content =
    Printf.sprintf
      "# Generated by tusk build\n\
       # Source directories\n\
       S ../../packages/*/src\n\
       S ../../packages/*/src/**\n\n\
       # Build directories\n\
       B out/packages/*\n\
       B sandbox/**\n\n\
       # Standard library\n\
       B %s\n\
       B %s/unix\n\n\
       # Packages\n\
       PKG unix\n\n\
       # Flags\n\
       FLG -w -a\n"
      stdlib_path stdlib_path
  in

  (* Write .merlin file *)
  let oc = open_out merlin_path in
  output_string oc merlin_content;
  close_out oc;

  (* Create symlink in root directory for LSP to find *)
  let root_merlin = Filename.concat root ".merlin" in
  (try System.remove_file root_merlin with _ -> ());
  System.symlink merlin_path root_merlin;
  Printf.printf "[Server] Generated .merlin file at %s (symlinked from %s)\n%!"
    merlin_path root_merlin

(** Handle scanning the workspace *)
let handle_scan_workspace state target_package =
  let root = System.getcwd () in
  Printf.printf "[Server] Scanning workspace from: %s\n" root;

  let workspace = Workspace.scan ~root in
  let build_graph =
    if workspace.packages = [] then None
    else
      let full_graph = Build_graph.create workspace state.toolchain in
      match target_package with
      | None -> Some full_graph
      | Some pkg_name ->
          Printf.printf "[Server] Filtering build graph for package: %s\n"
            pkg_name;
          Some (Build_graph.filter_for_package full_graph pkg_name)
  in

  (* Generate .merlin file for the workspace *)
  generate_merlin_file workspace state.toolchain;

  (* Print the build graph if we have one *)
  (match build_graph with
  | None -> Printf.printf "[Server] No packages found in workspace\n"
  | Some graph -> 
      Build_graph.print graph);

  { state with workspace = Some workspace; build_graph; active_build_graph = build_graph }

(** Handle RPC client request *)
let handle_client_request state client_pid request =
  match request with
  | Rpc.Ping ->
      send client_pid (ServerResponse Rpc.Pong);
      state
  | Rpc.Restart ->
      send client_pid (ServerResponse Rpc.Ok);
      (* Send restart message to self to handle after responding *)
      send (self ()) RestartServer;
      state
  | Rpc.Shutdown ->
      send client_pid (ServerResponse Rpc.Ok);
      (* Send shutdown message to self to handle after responding *)
      send (self ()) ShutdownServer;
      state
  | Rpc.GetWorkspace -> (
      match state.workspace with
      | Some ws ->
          let packages = List.map (fun p -> p.Workspace.name) ws.packages in
          send client_pid
            (ServerResponse (Rpc.WorkspaceInfo { packages; root = ws.root }));
          state
      | None ->
          send client_pid
            (ServerResponse (Rpc.Error { message = "No workspace loaded" }));
          state)
  | Rpc.GetBuildGraph -> (
      match state.build_graph with
      | Some graph ->
          let nodes = Build_graph.topological_sort graph in
          let packages =
            List.map
              (fun node ->
                let deps =
                  List.map
                    (fun d -> d.Build_node.package.name)
                    node.dependencies
                in
                (node.Build_node.package.name, deps))
              nodes
          in
          send client_pid (ServerResponse (Rpc.BuildGraphInfo { packages }));
          state
      | None ->
          send client_pid
            (ServerResponse
               (Rpc.Error { message = "No build graph available" }));
          state)
  | Rpc.GetPackageForFile { file_path } -> (
      match state.workspace with
      | Some ws -> (
          (* Find which package contains this file *)
          let package_opt =
            List.find_opt
              (fun pkg ->
                String.starts_with ~prefix:pkg.Workspace.path file_path)
              ws.packages
          in
          match package_opt with
          | Some pkg ->
              send client_pid
                (ServerResponse
                   (Rpc.PackageInfo
                      {
                        name = pkg.name;
                        path = pkg.path;
                        dependencies = pkg.dependencies;
                      }));
              state
          | None ->
              send client_pid
                (ServerResponse
                   (Rpc.Error { message = "File not in any package" }));
              state)
      | None ->
          send client_pid
            (ServerResponse (Rpc.Error { message = "No workspace loaded" }));
          state)
  | Rpc.GetConfigForFile { file_path } -> (
      match state.workspace with
      | Some ws -> (
          (* Find package and build configuration *)
          let package_opt =
            List.find_opt
              (fun pkg ->
                String.starts_with ~prefix:pkg.Workspace.path file_path)
              ws.packages
          in
          match package_opt with
          | Some pkg ->
              let home = Sys.getenv "HOME" in
              let stdlib_path =
                Printf.sprintf "%s/.tusk/toolchains/%s/lib/ocaml" home
                  (Toolchains.get_version state.toolchain)
              in
              let source_paths =
                [ Printf.sprintf "packages/%s/src" pkg.name ]
              in
              let build_paths =
                Printf.sprintf "target/debug/out/packages/%s" pkg.name
                :: List.map
                     (fun dep ->
                       Printf.sprintf "target/debug/out/packages/%s" dep)
                     pkg.dependencies
              in
              let flags = [ "-w"; "-a" ] in
              send client_pid
                (ServerResponse
                   (Rpc.FileConfig
                      { source_paths; build_paths; flags; stdlib_path }));
              state
          | None ->
              send client_pid
                (ServerResponse
                   (Rpc.Error { message = "File not in any package" }));
              state)
      | None ->
          send client_pid
            (ServerResponse (Rpc.Error { message = "No workspace loaded" }));
          state)
  | Rpc.BuildPackage { package; watch = _ } ->
      (* For RPC build, we need to scan workspace first if not done *)
      let new_state = 
        if state.workspace = None then
          handle_scan_workspace state None  (* Scan full workspace *)
        else state
      in
      (* Filter graph for the specific package *)
      let filtered_graph = 
        match new_state.build_graph with
        | Some graph -> 
            Printf.printf "[Server] Filtering build graph for package: %s\n" package;
            Some (Build_graph.filter_for_package graph package)
        | None -> None
      in
      (* Check if the package and its dependencies are already built *)
      (match filtered_graph with
      | Some graph ->
          let sorted = Build_graph.topological_sort graph in
          Printf.printf "[Server] Package %s requires: %s\n" package
            (String.concat ", " (List.map (fun n -> n.Build_node.package.name) sorted));
          let needs_build = sorted in  (* Simplified: queue all packages for now *)
          
          if needs_build = [] then (
            (* Everything already built, return success immediately *)
            Printf.printf "[Server] Package %s and dependencies already built!\n" package;
            let built_count = List.length sorted in
            send client_pid (ServerResponse (Rpc.BuildComplete { successful = built_count; failed = 0 }));
            new_state
          ) else (
            (* Mark this as an RPC client build and set current_build_graph *)
            let build_state = { new_state with 
              client_pid = Some client_pid;
              active_build_graph = filtered_graph  (* Use filtered graph for build *)
            } in
            (* Send the BuildPackage message to self to handle the actual build *)
            send (self ()) (BuildPackage (package, client_pid));
            (* Return the state with the filtered graph for this build *)
            build_state
          )
      | None ->
          (* No build graph, need to build *)
          let new_state = { new_state with client_pid = Some client_pid } in
          send (self ()) (BuildPackage (package, client_pid));
          new_state)
  | Rpc.BuildAll { watch = _ } ->
      (* For RPC build, we need to scan workspace first if not done *)
      let new_state = 
        if state.workspace = None then
          handle_scan_workspace state None
        else state
      in
      (* Check if all packages are already built *)
      (match new_state.build_graph with
      | Some graph ->
          let sorted = Build_graph.topological_sort graph in
          let needs_build = sorted in  (* Simplified: queue all packages for now *)
          
          if needs_build = [] then (
            (* Everything already built, return success immediately *)
            Printf.printf "[Server] All packages already built!\n";
            let built_count = List.length sorted in
            send client_pid (ServerResponse (Rpc.BuildComplete { successful = built_count; failed = 0 }));
            new_state
          ) else (
            (* Mark this as an RPC client build *)
            let new_state = { new_state with client_pid = Some client_pid } in
            (* Send the BuildAll message to self to handle the actual build *)
            send (self ()) (BuildAll client_pid);
            (* Return the updated state *)
            new_state
          )
      | None ->
          (* No build graph, need to build *)
          let new_state = { new_state with client_pid = Some client_pid } in
          send (self ()) (BuildAll client_pid);
          new_state)
  | _ ->
      send client_pid
        (ServerResponse (Rpc.Error { message = "Not implemented" }));
      state

(** Main server loop *)
let rec server_loop state =
  match receive () with
  | ClientRequest (client_pid, request) ->
      let new_state = handle_client_request state client_pid request in
      server_loop new_state
  | ScanWorkspace target_package ->
      let new_state = handle_scan_workspace state target_package in
      server_loop new_state
  | BuildAll cli_pid -> (
      Printf.printf "[Server] BuildAll command received\n";
      match state.build_graph with
      | None ->
          Printf.printf "[Server] No build graph available. Run ScanWorkspace first.\n";
          send cli_pid (ServerResponse (Rpc.Error { message = "No build graph available" }));
          server_loop state
      | Some graph ->
          Printf.printf "[Server] Building all packages...\n";
          let new_state = { state with active_build_graph = Some graph } in
          let updated_state = execute_build new_state cli_pid graph in
          server_loop updated_state)
  | BuildPackage (pkg_name, cli_pid) -> (
      Printf.printf "[Server] BuildPackage %s command received\n" pkg_name;
      (* active_build_graph should already be set to the filtered graph *)
      match state.active_build_graph with
      | None ->
          Printf.printf "[Server] No build graph available for package %s.\n" pkg_name;
          send cli_pid (ServerResponse (Rpc.Error { message = "No build graph available" }));
          server_loop state
      | Some graph ->
          Printf.printf "[Server] Building package %s and its dependencies...\n" pkg_name;
          let updated_state = execute_build state cli_pid graph in
          server_loop updated_state)
  | Worker_pool.TaskAssigned task ->
      (* Worker pool assigned a task - just log it *)
      let pkg_name = task.Build_messages.node.Build_node.package.name in
      Printf.printf "[Server] Worker pool assigned task for %s\n" pkg_name;
      server_loop state
  | Worker_pool.NoWorkersAvailable task ->
      (* No workers available - we'll retry later *)
      let pkg_name = task.Build_messages.node.Build_node.package.name in
      Printf.printf "[Server] No workers available for %s, will retry\n" pkg_name;
      (* The task is still in build_results as Building, it will be retried *)
      server_loop state
  | Worker_pool.TaskCompleted (pkg_name, success, hash) ->
      handle_task_complete state pkg_name success hash;

      (* Check if all done *)
      if Build_results.all_done state.build_results then (
        let built, failed, _building, _not_started =
          Build_results.get_stats state.build_results
        in
        Printf.printf "[Server] All builds complete! Built: %d, Failed: %d\n"
          built failed;

        (* Send completion response to client *)
        match state.client_pid with
        | Some client_pid ->
            send client_pid (ServerResponse (Rpc.BuildComplete { successful = built; failed }));
            
            (* Keep worker pool alive for future builds *)
            let fresh_build_queue = Build_queue.create state.build_results in
            let new_state = { state with 
              build_queue = fresh_build_queue;
              client_pid = None;
              active_build_graph = None;
              (* Keep worker_pool in state for reuse *)
            } in
            server_loop new_state
        | None ->
            (* This case shouldn't happen for RPC server, but keep pool alive anyway *)
            sleep 0.1;
            server_loop state
      ) else server_loop state
  | RequeueWithDependencies (task, missing_deps) ->
      let pkg_name = task.node.Build_node.package.name in
      let missing_names = List.map (fun dep -> dep.Build_node.package.name) missing_deps in
      Printf.printf "[Server] Requeuing %s, missing dependencies: %s\n" 
        pkg_name (String.concat ", " missing_names);
      flush stdout;
      
      List.iter (fun dep_node ->
        queue_package_if_needed_internal state dep_node true
      ) missing_deps;
      
      Printf.printf "[Server] Moving %s to waiting queue\n" pkg_name;
      Build_queue.add_waiting state.build_queue task;
      try_assign_work state;
      server_loop state
  | Shutdown ->
      Printf.printf "[Server] Shutting down...\n";
      (match state.worker_pool with
      | Some pool -> Worker_pool.shutdown pool
      | None -> ());
      sleep 0.5;
      Miniriot.shutdown ~status:0;
      Process.Normal
  | RestartServer ->
      Printf.printf "[Server] Restarting...\n";
      (* Shutdown worker pool if it exists *)
      (match state.worker_pool with
      | Some pool_pid -> 
          Worker_pool.shutdown pool_pid;
          sleep 0.1
      | None -> ());
      (* Re-scan workspace and restart *)
      let new_state = handle_scan_workspace { state with worker_pool = None } None in
      Printf.printf "[Server] Restarted successfully\n";
      server_loop new_state
  | ShutdownServer ->
      Printf.printf "[Server] Shutting down via RPC...\n";
      (* Shutdown worker pool if it exists *)
      (match state.worker_pool with
      | Some pool_pid -> 
          Worker_pool.shutdown pool_pid;
          sleep 0.1
      | None -> ());
      sleep 0.1;
      (* Gracefully shutdown the scheduler *)
      Miniriot.shutdown ~status:0;
      Process.Normal
  | _ ->
      (* Ignore unknown messages *)
      server_loop state

(** Start the build server *)
let start () =
  (* Scan workspace first *)
  let root = System.getcwd () in
  let workspace = Workspace.scan ~root in
  (* Ready toolchain with workspace *)
  let toolchain = Toolchains.ready_toolchains workspace in

  let build_results = Build_results.create () in
  let initial_state =
    {
      workspace = None;
      build_graph = None;
      active_build_graph = None;
      build_results;
      worker_pool = None;
      build_queue = Build_queue.create build_results;
      client_pid = None;
      toolchain;
    }
  in
  spawn (fun () ->
      Printf.printf "[Server] Build server started (pid: %s)\n"
        (Pid.to_string (self ()));
      server_loop initial_state)

(** Start the server with TCP listener for RPC *)
let start_with_listener () =
  (* Scan workspace first *)
  let root = Sys.getcwd () in
  let workspace = Workspace.scan ~root in
  (* Ready toolchain with workspace *)
  let toolchain = Toolchains.ready_toolchains workspace in
  let build_graph =
    if workspace.packages = [] then None
    else Some (Build_graph.create workspace toolchain)
  in

  (* Generate .merlin file *)
  (match workspace with
  | ws when ws.packages <> [] -> generate_merlin_file ws toolchain
  | _ -> ());

  let build_results = Build_results.create () in
  let initial_state =
    {
      workspace = Some workspace;
      build_graph;
      active_build_graph = build_graph;  (* Initially same as full graph *)
      build_results;
      worker_pool = None;
      build_queue = Build_queue.create build_results;
      client_pid = None;
      toolchain;
    }
  in

  (* Start server process *)
  let server_pid =
    spawn (fun () ->
        let my_pid = self () in
        Printf.printf "[Server] Build server started (pid: %s)\n"
          (Pid.to_string my_pid);
        server_loop initial_state)
  in

  (* Start TCP listener *)
  Listener.start server_pid
