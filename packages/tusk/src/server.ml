(** Build server - Miniriot process that orchestrates builds *)

open Miniriot
open Build_messages
open Build_node

(** Import shared RPC message types *)
open Rpc_messages

type state = {
  workspace : Workspace.workspace option;
  build_graph : Build_graph.t option; (* Full workspace build graph *)
  current_build_graph : Build_graph.t option; (* Filtered graph for current build *)
  build_results : Build_results.t;
  workers : Pid.t list;
  idle_workers : Pid.t Queue.t; (* Workers waiting for tasks *)
  current_queue : string Queue.t;
  later_queue : string Queue.t;
  cli_pid : Pid.t option; (* PID of CLI process to notify when done *)
  rpc_client_pid : Pid.t option; (* PID of RPC client to respond to *)
  toolchain : Toolchains.toolchain; (* Current toolchain *)
}
(** Server state *)

(** Get number of CPU cores *)
let get_num_cores () =
  try
    let ic =
      System.open_process_in
        "sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4"
    in
    let n = int_of_string (input_line ic) in
    close_in ic;
    n
  with _ -> 4 (* Default to 4 cores *)

(** Spawn worker processes *)
let spawn_workers server_pid num_workers =
  let rec spawn_n n acc =
    if n <= 0 then acc
    else
      let worker_pid = spawn (fun () -> Build_worker.main server_pid n ()) in
      spawn_n (n - 1) (worker_pid :: acc)
  in
  spawn_n num_workers []

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

(** Try to get next buildable task *)
let rec get_next_buildable_task state =
  match (state.current_build_graph, state.workspace) with
  | Some graph, Some workspace ->
      let nodes = Build_graph.topological_sort graph in

      (* First try current queue *)
      if not (Queue.is_empty state.current_queue) then
        let pkg_name = Queue.take state.current_queue in
        if can_build_package state pkg_name then
          (* Find the node for this package *)
          match
            List.find_opt (fun n -> n.Build_node.package.name = pkg_name) nodes
          with
          | Some node ->
              Some
                { node; workspace; toolchain_version = state.toolchain.version }
          | None -> None
        else (
          (* Put it in later queue and try again *)
          Queue.add pkg_name state.later_queue;
          get_next_buildable_task state) (* Then try later queue *)
      else if not (Queue.is_empty state.later_queue) then
        let pkg_name = Queue.take state.later_queue in
        if can_build_package state pkg_name then
          (* Find the node for this package *)
          match
            List.find_opt (fun n -> n.Build_node.package.name = pkg_name) nodes
          with
          | Some node ->
              Some
                { node; workspace; toolchain_version = state.toolchain.version }
          | None -> None
        else (
          (* Put it back and try next one *)
          Queue.add pkg_name state.later_queue;
          get_next_buildable_task state)
      else None
  | _, _ -> None

(** Check if a package can be built (all deps ready) *)
and can_build_package state pkg_name =
  match state.current_build_graph with
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

(** Try to assign work to idle workers *)
let try_assign_work state =
  let rec assign_loop () =
    if
      (not (Queue.is_empty state.idle_workers))
      && not
           (Queue.is_empty state.current_queue
           || Queue.is_empty state.later_queue)
    then
      match get_next_buildable_task state with
      | Some build_task ->
          let worker_pid = Queue.take state.idle_workers in
          let pkg_name = build_task.node.Build_node.package.name in
          Printf.printf "[Server] Assigning package %s to worker %s\n" pkg_name
            (Pid.to_string worker_pid);
          (* Mark as building *)
          Build_results.mark_building state.build_results pkg_name;
          send worker_pid (Task build_task);
          assign_loop () (* Try to assign more work *)
      | None -> ()
    else ()
  in
  assign_loop ()

(** Handle worker requesting next task *)
let handle_next_task state worker_pid =
  match get_next_buildable_task state with
  | Some build_task ->
      let pkg_name = build_task.node.Build_node.package.name in
      Printf.printf "[Server] Assigning package %s to worker %s\n" pkg_name
        (Pid.to_string worker_pid);
      (* Mark as building *)
      Build_results.mark_building state.build_results pkg_name;
      send worker_pid (Task build_task)
  | None ->
      (* No work available, add worker to idle queue *)
      Queue.add worker_pid state.idle_workers;
      send worker_pid NoTask

(** Handle task completion *)
let handle_task_complete state pkg_name success =
  if success then (
    Printf.printf "[Server] Build complete: %s\n" pkg_name;
    Build_results.mark_built state.build_results pkg_name;

    (* Check if this unblocks any packages in the later queue *)
    (* Move them to current queue if their deps are now ready *)
    let later_items = ref [] in
    while not (Queue.is_empty state.later_queue) do
      later_items := Queue.take state.later_queue :: !later_items
    done;

    List.iter
      (fun item ->
        if can_build_package state item then Queue.add item state.current_queue
        else Queue.add item state.later_queue)
      !later_items;

    (* Also queue any packages that were waiting only on this one *)
    match state.current_build_graph with
    | Some graph ->
        let nodes = Build_graph.topological_sort graph in
        List.iter
          (fun node ->
            let pkg = node.Build_node.package.name in
            if
              (not (Build_results.is_built state.build_results pkg))
              && (not (Build_results.is_building state.build_results pkg))
              && can_build_package state pkg
            then Queue.add pkg state.current_queue)
          nodes
    | None ->
        ();

        (* Try to assign work to idle workers now that new tasks may be available *)
        try_assign_work state)
  else (
    Printf.printf "[Server] Build failed: %s\n" pkg_name;
    Build_results.mark_failed state.build_results pkg_name "Build failed")

(** Generate .merlin file for LSP configuration *)
let generate_merlin_file workspace toolchain_version =
  let root = System.getcwd () in
  let profile = "debug" in
  (* TODO: make this configurable *)
  let target_dir = Filename.concat root (Printf.sprintf "target/%s" profile) in
  let merlin_path = Filename.concat target_dir ".merlin" in
  let home = System.get_home () in
  let stdlib_path =
    Printf.sprintf "%s/.tusk/toolchains/%s/lib/ocaml" home toolchain_version
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
      let full_graph = Build_graph.create workspace in
      match target_package with
      | None -> Some full_graph
      | Some pkg_name ->
          Printf.printf "[Server] Filtering build graph for package: %s\n"
            pkg_name;
          Some (Build_graph.filter_for_package full_graph pkg_name)
  in

  (* Generate .merlin file for the workspace *)
  generate_merlin_file workspace state.toolchain.version;

  (* Print the build graph if we have one *)
  (match build_graph with
  | None -> Printf.printf "[Server] No packages found in workspace\n"
  | Some graph -> Build_graph.print graph);

  { state with workspace = Some workspace; build_graph; current_build_graph = build_graph }

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
                  state.toolchain.version
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
              let flags = [ "-w"; "-a"; "-I"; "+unix" ] in
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
          let needs_build = List.filter (fun node ->
            let pkg_name = node.Build_node.package.name in
            match new_state.workspace with
            | Some workspace -> not (Build_results.is_built_with_outputs_check new_state.build_results pkg_name workspace)
            | None -> not (Build_results.is_built new_state.build_results pkg_name)
          ) sorted in
          
          if needs_build = [] then (
            (* Everything already built, return success immediately *)
            Printf.printf "[Server] Package %s and dependencies already built!\n" package;
            let built_count = List.length sorted in
            send client_pid (ServerResponse (Rpc.BuildComplete { successful = built_count; failed = 0 }));
            new_state
          ) else (
            (* Mark this as an RPC client build and set current_build_graph *)
            let build_state = { new_state with 
              rpc_client_pid = Some client_pid;
              current_build_graph = filtered_graph  (* Use filtered graph for build *)
            } in
            (* Send the BuildPackage message to self to handle the actual build *)
            send (self ()) (BuildPackage (package, client_pid));
            (* Return the state with the filtered graph for this build *)
            build_state
          )
      | None ->
          (* No build graph, need to build *)
          let new_state = { new_state with rpc_client_pid = Some client_pid } in
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
          let needs_build = List.filter (fun node ->
            let pkg_name = node.Build_node.package.name in
            match new_state.workspace with
            | Some workspace -> not (Build_results.is_built_with_outputs_check new_state.build_results pkg_name workspace)
            | None -> not (Build_results.is_built new_state.build_results pkg_name)
          ) sorted in
          
          if needs_build = [] then (
            (* Everything already built, return success immediately *)
            Printf.printf "[Server] All packages already built!\n";
            let built_count = List.length sorted in
            send client_pid (ServerResponse (Rpc.BuildComplete { successful = built_count; failed = 0 }));
            new_state
          ) else (
            (* Mark this as an RPC client build *)
            let new_state = { new_state with rpc_client_pid = Some client_pid } in
            (* Send the BuildAll message to self to handle the actual build *)
            send (self ()) (BuildAll client_pid);
            (* Return the updated state *)
            new_state
          )
      | None ->
          (* No build graph, need to build *)
          let new_state = { new_state with rpc_client_pid = Some client_pid } in
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
      (* For BuildAll, use the full build graph *)
      let new_state = { state with current_build_graph = state.build_graph } in
      match new_state.current_build_graph with
      | None ->
          Printf.printf
            "[Server] No build graph available. Run ScanWorkspace first.\n";
          server_loop state
      | Some graph ->
          Printf.printf "[Server] Building all packages...\n";

          (* Don't clear build results - keep track of what's already built *)
          let sorted = Build_graph.topological_sort graph in
          
          (* Check what actually needs to be built *)
          let needs_build = List.filter (fun node ->
            let pkg_name = node.Build_node.package.name in
            not (Build_results.is_built state.build_results pkg_name)
          ) sorted in
          
          Printf.printf "[Server] Packages to build: ";
          List.iter (fun node ->
            Printf.printf "%s " node.Build_node.package.name
          ) needs_build;
          Printf.printf "\n";
          
          (* If nothing needs to be built, immediately return success *)
          if needs_build = [] then (
            Printf.printf "[Server] All packages already built! Returning success immediately.\n";
            (* Get the count of already built packages *)
            let built_count = List.length sorted in
            (* Send BuildFinished for CLI client *)
            send cli_pid (BuildFinished { successful = built_count; failed = 0 });
            server_loop state
          ) else (
            (* Initialize only the packages we need to build *)
            List.iter (fun node ->
              let pkg_name = node.Build_node.package.name in
              if not (Build_results.is_tracked state.build_results pkg_name) then
                Build_results.init_package state.build_results pkg_name
            ) needs_build;

            (* Queue packages with no dependencies or whose deps are built *)
            List.iter
              (fun node ->
                let pkg_name = node.Build_node.package.name in
                if not (Build_results.is_built new_state.build_results pkg_name) &&
                   can_build_package new_state pkg_name then (
                  Printf.printf "[Server] Queueing package: %s\n" pkg_name;
                  Queue.add pkg_name new_state.current_queue))
              sorted;

            Printf.printf "[Server] Current queue size: %d\n"
              (Queue.length new_state.current_queue);

            (* Spawn workers if not already spawned *)
            let workers =
              if state.workers = [] then (
                let num_cores = get_num_cores () in
                Printf.printf "[Server] Spawning %d workers\n" num_cores;
                let workers = spawn_workers (self ()) num_cores in
                (* Give workers a moment to start *)
                sleep 0.1;
                workers)
              else state.workers
            in

            let new_state = { new_state with workers; cli_pid = Some cli_pid } in
            (* Try to assign initial work to workers *)
            try_assign_work new_state;
            server_loop new_state))
  | BuildPackage (pkg_name, cli_pid) -> (
      Printf.printf "[Server] BuildPackage %s command received\n" pkg_name;
      (* current_build_graph should already be set to the filtered graph *)
      match state.current_build_graph with
      | None ->
          Printf.printf
            "[Server] No build graph available. Run ScanWorkspace first.\n";
          send cli_pid (BuildFinished { successful = 0; failed = 1 });
          server_loop state
      | Some graph ->
          Printf.printf "[Server] Building package %s and its dependencies...\n"
            pkg_name;

          (* Don't clear build results - keep track of what's already built *)
          let sorted = Build_graph.topological_sort graph in
          
          (* Check what actually needs to be built *)
          let needs_build = List.filter (fun node ->
            let pkg_name = node.Build_node.package.name in
            not (Build_results.is_built state.build_results pkg_name)
          ) sorted in
          
          Printf.printf "[Server] Packages already built: ";
          List.iter (fun node ->
            let pkg_name = node.Build_node.package.name in
            if Build_results.is_built state.build_results pkg_name then
              Printf.printf "%s " pkg_name
          ) sorted;
          Printf.printf "\n";
          
          Printf.printf "[Server] Packages to build: ";
          List.iter (fun node ->
            Printf.printf "%s " node.Build_node.package.name
          ) needs_build;
          Printf.printf "\n";
          
          (* If nothing needs to be built, immediately return success *)
          if needs_build = [] then (
            Printf.printf "[Server] All packages already built! Returning success immediately.\n";
            (* Get the count of already built packages *)
            let built_count = List.length sorted in
            (* Send BuildFinished for CLI client *)
            send cli_pid (BuildFinished { successful = built_count; failed = 0 });
            server_loop state
          ) else (
            (* Initialize only the packages we need to build *)
            List.iter (fun node ->
              let pkg_name = node.Build_node.package.name in
              if not (Build_results.is_tracked state.build_results pkg_name) then
                Build_results.init_package state.build_results pkg_name
            ) needs_build;

            (* Queue packages with no dependencies or whose deps are built *)
            List.iter
              (fun node ->
                let pkg_name = node.Build_node.package.name in
                if not (Build_results.is_built state.build_results pkg_name) &&
                   can_build_package state pkg_name then (
                  Printf.printf "[Server] Queueing package: %s\n" pkg_name;
                  Queue.add pkg_name state.current_queue))
              sorted;

            Printf.printf "[Server] Current queue size: %d\n"
              (Queue.length state.current_queue);

            (* Spawn workers if not already spawned *)
            let workers =
              if state.workers = [] then (
                let num_cores = get_num_cores () in
                Printf.printf "[Server] Spawning %d workers\n" num_cores;
                let workers = spawn_workers (self ()) num_cores in
                (* Give workers a moment to start *)
                sleep 0.1;
                workers)
              else state.workers
            in

            let new_state = { state with workers; cli_pid = Some cli_pid } in
            (* Try to assign initial work to workers *)
            try_assign_work new_state;
            server_loop new_state))
  | NextTask worker_pid ->
      Printf.printf "[Server] Worker %s requesting task\n"
        (Pid.to_string worker_pid);
      handle_next_task state worker_pid;
      server_loop state
  | TaskComplete (pkg_name, success) ->
      handle_task_complete state pkg_name success;

      (* Check if all done *)
      if Build_results.all_done state.build_results then (
        let built, failed, _building, _not_started =
          Build_results.get_stats state.build_results
        in
        Printf.printf "[Server] All builds complete! Built: %d, Failed: %d\n"
          built failed;

        (* Notify CLI that we're done *)
        (match state.cli_pid with
        | Some pid -> 
            (* Send BuildFinished for CLI client *)
            send pid (BuildFinished { successful = built; failed })
        | None -> ());

        (* Shutdown workers *)
        List.iter (fun w -> send w Shutdown) state.workers;

        (* Give workers time to shutdown *)
        sleep 0.5;
        
        (* If this was an RPC client build, continue server loop. Otherwise exit. *)
        match state.rpc_client_pid with
        | Some _ ->
            (* Reset state for next build but keep build results *)
            let new_state = { state with 
              workers = [];
              cli_pid = None;
              rpc_client_pid = None;
              current_build_graph = None;  (* Clear current build graph *)
              (* Keep build_results to track what's already built *)
            } in
            server_loop new_state
        | None ->
            (* CLI build - exit normally *)
            Process.Normal)
      else server_loop state
  | Shutdown ->
      Printf.printf "[Server] Shutting down...\n";
      List.iter (fun w -> send w Shutdown) state.workers;
      sleep 0.5;
      Miniriot.shutdown ~status:0;
      Process.Normal
  | RestartServer ->
      Printf.printf "[Server] Restarting...\n";
      (* Shutdown workers *)
      List.iter (fun w -> send w Shutdown) state.workers;
      sleep 0.1;
      (* Re-scan workspace and restart *)
      let new_state = handle_scan_workspace state None in
      Printf.printf "[Server] Restarted successfully\n";
      server_loop new_state
  | ShutdownServer ->
      Printf.printf "[Server] Shutting down via RPC...\n";
      (* Shutdown workers *)
      List.iter (fun w -> send w Shutdown) state.workers;
      sleep 0.1;
      (* Gracefully shutdown the scheduler *)
      Miniriot.shutdown ~status:0;
      Process.Normal
  | _ ->
      (* Ignore unknown messages *)
      server_loop state

(** Start the build server *)
let start () =
  (* Ready toolchain at startup *)
  let root = System.getcwd () in
  let toolchain = Toolchains.ready_toolchains root in

  let initial_state =
    {
      workspace = None;
      build_graph = None;
      current_build_graph = None;
      build_results = Build_results.create ();
      workers = [];
      idle_workers = Queue.create ();
      current_queue = Queue.create ();
      later_queue = Queue.create ();
      cli_pid = None;
      rpc_client_pid = None;
      toolchain;
    }
  in
  spawn (fun () ->
      Printf.printf "[Server] Build server started (pid: %s)\n"
        (Pid.to_string (self ()));
      server_loop initial_state)

(** Start the server with TCP listener for RPC *)
let start_with_listener () =
  (* Ready toolchain at startup *)
  let root = Sys.getcwd () in
  let toolchain = Toolchains.ready_toolchains root in

  (* Scan workspace on startup *)
  let workspace = Workspace.scan ~root in
  let build_graph =
    if workspace.packages = [] then None
    else Some (Build_graph.create workspace)
  in

  (* Generate .merlin file *)
  (match workspace with
  | ws when ws.packages <> [] -> generate_merlin_file ws toolchain.version
  | _ -> ());

  let initial_state =
    {
      workspace = Some workspace;
      build_graph;
      current_build_graph = build_graph;  (* Initially same as full graph *)
      build_results = Build_results.create ();
      workers = [];
      idle_workers = Queue.create ();
      current_queue = Queue.create ();
      later_queue = Queue.create ();
      cli_pid = None;
      rpc_client_pid = None;
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
