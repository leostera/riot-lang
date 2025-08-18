(** Build server - Miniriot process that orchestrates builds *)

open Miniriot
open Build_messages
open Build_node
open Rpc

type state = {
  workspace : Workspace.workspace option;
  build_graph : Build_graph.t option; (* Full workspace build graph *)
  active_build_graph : Build_graph.t option;
      (* Graph for current build (full or filtered) *)
  build_results : Build_results.t;
  worker_pool : Worker_pool.t option; (* Handle to the worker pool *)
  build_queue : Build_queue.t; (* Two-queue system for dependency ordering *)
  client_pid : Pid.t option; (* PID of client to respond to *)
  session_id : Log.session_id option; (* Session ID for current build *)
  toolchain : Toolchains.toolchain; (* Current toolchain *)
  build_start_time : float option; (* Unix timestamp when build started *)
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

(* Internal version - uses cached hashes, for use during build *)

(** Queue a package for building if not already tracked as building or built *)
let queue_package_if_needed_internal state node is_ready =
  let pkg_name = node.Build_node.package.name in
  (* Don't re-queue if already building, failed, built, or busy *)
  if Build_queue.is_busy state.build_queue pkg_name then
    (* Package already busy - skip silently *)
    ()
  else
    match Build_results.get_status state.build_results pkg_name with
    | Some Build_results.Building ->
        (* Package already building - skip silently *)
        ()
    | Some (Build_results.Failed _) ->
        (* Package already failed - skip silently *)
        ()
    | Some (Build_results.Built _) ->
        (* Already built in this session - don't recheck *)
        (* Package already built - skip silently *)
        ()
    | Some Build_results.NotStarted | None ->
        (* Not started - queue it *)
        Log.queue_package ?sid:state.session_id ~package:pkg_name
          ~queue_type:(if is_ready then `Ready else `Waiting);
        let task =
          {
            Build_messages.node;
            workspace = Option.get state.workspace;
            session_id = state.session_id;
          }
        in
        if is_ready then Build_queue.add_ready state.build_queue task
        else Build_queue.add_waiting state.build_queue task

(** Queue a package at entry point - checks for hash changes *)
let queue_package_if_needed state node is_ready =
  let pkg_name = node.Build_node.package.name in
  (* Don't re-queue if already busy *)
  if Build_queue.is_busy state.build_queue pkg_name then
    (* Package already busy - skip silently *)
    ()
  else
    (* Don't re-queue if already building or failed *)
    match Build_results.get_status state.build_results pkg_name with
    | Some Build_results.Building ->
        (* Package already building - skip silently *)
        ()
    | Some (Build_results.Failed _) ->
        (* Package already failed - skip silently *)
        ()
    | Some (Build_results.Built stored_hash) -> (
        (* Check if the current hash matches the stored hash *)
        match state.workspace with
        | Some workspace -> (
            (* Force recomputation of hash to detect changes *)
            match Build_graph.recompute_node_hash state.toolchain node with
            | Build_graph.Ok current_hash ->
                if Hasher.equal stored_hash current_hash then
                  Log.cache_hit ?sid:state.session_id ~package:pkg_name
                    ~hash:(Hasher.to_string current_hash)
                else (
                  Log.hash_computed ?sid:state.session_id ~package:pkg_name
                    ~hash:(Hasher.to_string current_hash);
                  Log.queue_package ?sid:state.session_id ~package:pkg_name
                    ~queue_type:(if is_ready then `Ready else `Waiting);
                  (* Update the node's hash to the new computed hash *)
                  node.hash <- Some current_hash;
                  (* Reset the build status since hash changed *)
                  Build_results.init_package state.build_results pkg_name;
                  let task =
                    {
                      Build_messages.node;
                      workspace;
                      session_id = state.session_id;
                    }
                  in
                  if is_ready then Build_queue.add_ready state.build_queue task
                  else Build_queue.add_waiting state.build_queue task)
            | _ ->
                (* Can't compute hash, queue it to be safe *)
                Log.log ?sid:state.session_id
                  (HashComputed { package = pkg_name; hash = "error_computing" });
                Log.queue_package ?sid:state.session_id ~package:pkg_name
                  ~queue_type:(if is_ready then `Ready else `Waiting);
                let task =
                  {
                    Build_messages.node;
                    workspace;
                    session_id = state.session_id;
                  }
                in
                if is_ready then Build_queue.add_ready state.build_queue task
                else Build_queue.add_waiting state.build_queue task)
        | None ->
            (* No workspace, skip it *)
            Log.log ?sid:state.session_id
              (DependencyMissing
                 { package = pkg_name; missing = [ "workspace_unavailable" ] }))
    | Some Build_results.NotStarted | None ->
        (* Not started - queue it *)
        Log.queue_package ?sid:state.session_id ~package:pkg_name
          ~queue_type:(if is_ready then `Ready else `Waiting);
        let task =
          {
            Build_messages.node;
            workspace = Option.get state.workspace;
            session_id = state.session_id;
          }
        in
        if is_ready then Build_queue.add_ready state.build_queue task
        else Build_queue.add_waiting state.build_queue task

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
  | Some graph, Some workspace -> (
      let nodes = Build_graph.topological_sort graph in

      (* Helper to check if a package should be built *)
      let should_build_package pkg_name node =
        if not (can_build_package state pkg_name) then false
        else
          (* Compute hash and check if already built with this hash *)
          let store = Store.create ~root_dir:workspace.root in
          match Build_graph.get_node_hash state.toolchain node store with
          | Build_graph.Ok hash ->
              not
                (Build_results.is_built_with_current_hash state.build_results
                   pkg_name hash)
          | _ -> true (* If we can't compute hash, try to build anyway *)
      in

      (* Try to get a ready task from the queue *)
      match Build_queue.take_ready state.build_queue with
      | Some task ->
          let pkg_name = task.Build_messages.node.Build_node.package.name in
          let node = task.Build_messages.node in
          if should_build_package pkg_name node then Some task
          else (
            (* Either not buildable yet or already built - try next *)
            if not (can_build_package state pkg_name) then (
              (* Move back to waiting queue and mark as not busy *)
              Build_queue.add_waiting state.build_queue task;
              Build_queue.mark_completed state.build_queue pkg_name)
            else
              (* Already built - just mark as not busy *)
              Build_queue.mark_completed state.build_queue pkg_name;
            get_next_buildable_task state)
      | None -> None
      (* No work available *))
  | _, _ -> None

(** Check if a package can be built (all deps ready) *)

(** Check later queue and move ready packages to current queue *)
let check_later_queue state =
  match state.active_build_graph with
  | None -> ()
  | Some graph ->
      let nodes = Build_graph.topological_sort graph in
      List.iter
        (fun node ->
          let pkg_name = node.Build_node.package.name in
          if Build_queue.is_waiting state.build_queue pkg_name then
            if can_build_package state pkg_name then (
              (* Package now ready to build *)
              ();
              Build_queue.move_to_ready state.build_queue pkg_name))
        nodes

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
            (* Assign package to worker *)
            ();
            Build_results.mark_building state.build_results pkg_name;
            Worker_pool.send_task pool build_task;
            send_ready_tasks ()
        | None -> ()
      in
      send_ready_tasks ()

(** Unified build execution handler *)
let execute_build state client_pid build_graph =
  (* Create session ID for this build *)
  let sid = Session_id.make () in
  Printf.eprintf "[SERVER DEBUG] execute_build: session_id=%s\n"
    (Session_id.to_string sid);
  flush stderr;

  (* Send BuildStarted response with session ID *)
  Printf.eprintf "[SERVER DEBUG] Sending BuildStarted response to client\n";
  flush stderr;
  send client_pid (ServerResponse (Rpc.BuildStarted { session_id = sid }));

  (* Register the client as a log handler for this build session *)
  Printf.eprintf "[SERVER DEBUG] Registering log handler for session %s\n"
    (Session_id.to_string sid);
  flush stderr;
  Log.add_rpc_handler ~sid ~client:client_pid ~format:Log.Human;

  Printf.eprintf "[SERVER DEBUG] Logging build start...\n";
  flush stderr;

  (* Reset failed packages so they can be retried *)
  Build_results.reset_failed_packages state.build_results;

  (* Get all packages in topological order *)
  let sorted = Build_graph.topological_sort build_graph in

  (* Log the build plan *)
  let packages = List.map (fun node -> node.Build_node.package.name) sorted in
  let total_modules = List.length sorted in
  (* Just count packages for now *)
  Log.build_started ~sid ~packages ~total_modules ~workers:10;

  (* Initialize build results for all packages *)
  List.iter
    (fun node ->
      let pkg_name = node.Build_node.package.name in
      if not (Build_results.is_tracked state.build_results pkg_name) then
        Build_results.init_package state.build_results pkg_name)
    sorted;

  (* Update state with session_id first *)
  let state = { state with session_id = Some sid } in

  (* Queue all packages - current if ready, later if waiting on deps *)
  (* Analyzing package dependencies and queueing *)
  List.iter
    (fun node ->
      let pkg_name = node.Build_node.package.name in
      let is_ready = can_build_package state pkg_name in
      if not is_ready then
        Log.dependency_missing ?sid:state.session_id ~package:pkg_name
          ~missing:
            (List.filter_map
               (fun dep_node ->
                 let dep_name = dep_node.Build_node.package.name in
                 if not (can_build_package state dep_name) then Some dep_name
                 else None)
               node.Build_node.dependencies);
      queue_package_if_needed state node is_ready)
    sorted;

  (* Package queueing completed *)
  let ready_count, waiting_count, busy_count =
    Build_queue.stats state.build_queue
  in
  Log.queue_stats ?sid:state.session_id ~ready:ready_count
    ~waiting:waiting_count ~busy:busy_count;

  (* Check if there's actually any work to do *)
  if ready_count = 0 && waiting_count = 0 && busy_count = 0 then (
    (* All packages already built - return success immediately *)
    let duration_ms = 
      match state.build_start_time with
      | Some start_time -> int_of_float ((System.gettimeofday () -. start_time) *. 1000.)
      | None -> 0
    in
    Log.log ?sid:state.session_id
      (BuildComplete
         {
           duration_ms;
           results = [];
           succeeded = List.map (fun n -> n.Build_node.package.name) sorted;
           failed = [];
         });
    let built_count = List.length sorted in
    (* Send success response to client *)
    let stats = {
      Rpc.duration_ms;
      packages_built = built_count;
      packages_failed = 0;
      total_modules = built_count;
      cache_hits = built_count;  (* All were cached/already built *)
      cache_misses = 0;
    } in
    send client_pid (ServerResponse (Rpc.BuildComplete stats));
    (* Remove the RPC handler for this session *)
    (match state.session_id with
    | Some sid -> Log.remove_handler ~sid
    | None -> ());
    (* Reset state for next build *)
    let fresh_build_queue = Build_queue.create state.build_results in
    {
      state with
      client_pid = None;
      session_id = None;
      active_build_graph = None;
      build_queue = fresh_build_queue;
    })
  else
    (* Start worker pool if not already started *)
    let pool =
      match state.worker_pool with
      | Some pool -> pool
      | None ->
          let pool = Worker_pool.start ~listener:(self ()) () in
          Log.worker_pool_started ?sid:state.session_id ~workers:10;
          pool
    in
    let new_state =
      {
        state with
        client_pid = Some client_pid;
        session_id = Some sid;
        worker_pool = Some pool;
      }
    in
    (* Try to assign initial work to workers *)
    try_assign_work new_state;
    new_state

(** Check if build is complete and handle client response *)
let check_build_complete state =
  if Build_results.all_done state.build_results then (
    let built, failed, _building, _not_started =
      Build_results.get_stats state.build_results
    in
    let duration_ms = 
      match state.build_start_time with
      | Some start_time -> int_of_float ((System.gettimeofday () -. start_time) *. 1000.)
      | None -> 0
    in
    (* Collect succeeded and failed package names *)
    let succeeded = ref [] in
    let failed_list = ref [] in
    (match state.active_build_graph with
    | Some graph ->
        let nodes = Build_graph.topological_sort graph in
        List.iter (fun node ->
          let pkg_name = node.Build_node.package.name in
          match Build_results.get_status state.build_results pkg_name with
          | Some (Build_results.Built _) -> succeeded := pkg_name :: !succeeded
          | Some (Build_results.Failed _) -> failed_list := pkg_name :: !failed_list
          | _ -> ()
        ) nodes
    | None -> ());
    Log.log ?sid:state.session_id
      (BuildComplete
         { duration_ms; results = []; succeeded = List.rev !succeeded; failed = List.rev !failed_list });

    (* Send completion response to client *)
    match state.client_pid with
    | Some client_pid ->
        (* Calculate total modules and cache stats from build results *)
        let total_modules = built + failed in
        let cache_hits = 0 in  (* TODO: track from build results *)
        let cache_misses = 0 in  (* TODO: track from build results *)
        
        let stats = {
          Rpc.duration_ms;
          packages_built = built;
          packages_failed = failed;
          total_modules;
          cache_hits;
          cache_misses;
        } in
        
        let response =
          if failed = 0 then Rpc.BuildComplete stats
          else
            Rpc.BuildFailed { 
              stats; 
              error = Printf.sprintf "Build failed: %d packages failed" failed 
            }
        in
        send client_pid (ServerResponse response);
        
        (* Remove the RPC handler for this session *)
        (match state.session_id with
        | Some sid -> Log.remove_handler ~sid
        | None -> ());

        (* Keep worker pool alive for future builds *)
        let fresh_build_queue = Build_queue.create state.build_results in
        let new_state =
          {
            state with
            build_queue = fresh_build_queue;
            client_pid = None;
            session_id = None;
            active_build_graph = None;
            (* Keep worker_pool in state for reuse *)
          }
        in
        (true, new_state)
    | None ->
        (* This case shouldn't happen for RPC server, but keep pool alive anyway *)
        sleep 0.1;
        (true, state))
  else (false, state)

(** Handle task completion *)
let handle_task_complete state pkg_name success hash =
  (* Mark task as no longer busy *)
  Build_queue.mark_completed state.build_queue pkg_name;

  if success then (
    Log.package_complete ?sid:state.session_id
      {
        package = pkg_name;
        success = true;
        duration_ms = 0;
        modules_compiled = 0;
        cache_hits = 0;
        cache_misses = 0;
        errors = [];
      };
    Build_results.mark_built_with_hash state.build_results pkg_name hash;

    (* Also check if any other packages are now unblocked *)
    (match state.active_build_graph with
    | Some graph ->
        let nodes = Build_graph.topological_sort graph in
        Log.log ?sid:state.session_id
          (DependencySatisfied { package = pkg_name });
        List.iter
          (fun node ->
            let pkg = node.Build_node.package.name in
            let is_building =
              Build_results.is_building state.build_results pkg
            in
            let can_build = can_build_package state pkg in
            (* Debug logging removed - not needed for production *)
            if (not is_building) && can_build then
              queue_package_if_needed_internal state node true)
          nodes
    | None -> ());
    try_assign_work state)
  else (
    (* For build failures, we rely on the detailed CompileError events 
       that were logged during the build process in sandbox.ml.
       Just report the package failure without redundant error details. *)
    Log.package_complete ?sid:state.session_id
      {
        package = pkg_name;
        success = false;
        duration_ms = 0;
        modules_compiled = 0;
        cache_hits = 0;
        cache_misses = 0;
        errors = [];
        (* Detailed errors already logged as CompileError events *)
      };
    Build_results.mark_failed state.build_results pkg_name "Build failed")

(* Removed: .merlin file generation - tusk provides dynamic Merlin protocol support *)

(** Handle scanning the workspace *)
let handle_scan_workspace state target_package =
  let root = System.getcwd () in
  Log.server_scanning ?sid:None ~root;

  (* Loading workspace configuration *)
  let workspace = Workspace_manager.get_workspace ~root in
  (* Workspace configuration loaded *)
  let build_graph =
    if workspace.packages = [] then None
    else
      let full_graph = Build_graph.create workspace state.toolchain in
      match target_package with
      | None -> Some full_graph
      | Some pkg_name ->
          Log.log ?sid:None (ComputingHash { package = pkg_name });
          Some (Build_graph.filter_for_package full_graph pkg_name)
  in

  (* No longer generating .merlin files - using dynamic Merlin protocol support *)

  (* Print the build graph if we have one *)
  (match build_graph with
  | None -> Log.workspace_empty ?sid:None ()
  | Some graph -> Build_graph.print graph);

  {
    state with
    workspace = Some workspace;
    build_graph;
    active_build_graph = build_graph;
  }

(** Handle RPC client request *)
let handle_client_request state client_pid request =
  match request with
  | Rpc.Ping ->
      send client_pid (ServerResponse Rpc.Pong);
      state
  | Rpc.BuildAll ->
      (* Start a build all request *)
      let session_id = Session_id.make () in
      let start_time = System.gettimeofday () in
      send client_pid (ServerResponse (Rpc.BuildStarted { session_id }));
      send (self ()) (BuildAll { client_pid });
      { state with 
        client_pid = Some client_pid; 
        session_id = Some session_id;
        build_start_time = Some start_time }
  | Rpc.BuildPackage package ->
      (* Start a build package request *)
      let session_id = Session_id.make () in
      let start_time = System.gettimeofday () in
      send client_pid (ServerResponse (Rpc.BuildStarted { session_id }));
      send (self ()) (BuildPackage { package_name = package; client_pid });
      { state with 
        client_pid = Some client_pid; 
        session_id = Some session_id;
        build_start_time = Some start_time }
  | Rpc.Restart ->
      send client_pid (ServerResponse Rpc.RestartAck);
      (* Send restart message to self to handle after responding *)
      send (self ()) RestartServer;
      state
  | Rpc.Shutdown ->
      send client_pid (ServerResponse Rpc.ShutdownAck);
      (* Send shutdown message to self to handle after responding *)
      send (self ()) ShutdownServer;
      state
  | Rpc.GetWorkspaceConfig -> (
      match state.workspace with
      | Some ws ->
          let packages = List.map (fun p -> p.Workspace.name) ws.packages in
          send client_pid
            (ServerResponse
               (Rpc.WorkspaceConfig
                  {
                    workspace_root = ws.root;
                    toolchain = Toolchains.get_version state.toolchain;
                    packages;
                  }));
          state
      | None ->
          send client_pid (ServerResponse (Rpc.Error "No workspace loaded"));
          state)
  | Rpc.GetBuildGraph -> (
      match state.build_graph with
      | Some graph ->
          let nodes = Build_graph.topological_sort graph in
          let build_nodes =
            List.map
              (fun node ->
                let pkg = node.Build_node.package in
                {
                  Rpc.package_name = pkg.name;
                  src_dir = pkg.path;
                  out_dir =
                    Printf.sprintf "target/debug/out/packages/%s" pkg.name;
                  status =
                    (match
                       Build_results.get_status state.build_results pkg.name
                     with
                    | Some (Build_results.Built _) -> "built"
                    | Some Building -> "building"
                    | Some (Failed _) -> "failed"
                    | _ -> "pending");
                  deps = pkg.dependencies;
                })
              nodes
          in
          send client_pid
            (ServerResponse (Rpc.BuildGraph { nodes = build_nodes }));
          state
      | None ->
          send client_pid
            (ServerResponse (Rpc.Error "No build graph available"));
          state)
  (* | Rpc.GetPackageForFile { file_path } -> (
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
                   (Rpc.Error "File not in any package"));
              state)
      | None ->
          send client_pid
            (ServerResponse (Rpc.Error "No workspace loaded"));
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
                   (Rpc.Error "File not in any package"));
              state)
      | None ->
          send client_pid
            (ServerResponse (Rpc.Error "No workspace loaded"));
          state) *)
  | Rpc.BuildPackage package -> (
      (* For RPC build, we need to scan workspace first if not done *)
      let new_state =
        if state.workspace = None then
          handle_scan_workspace state None (* Scan full workspace *)
        else state
      in
      (* Filter graph for the specific package *)
      let filtered_graph =
        match new_state.build_graph with
        | Some graph ->
            Log.log ?sid:None (ComputingHash { package });
            Some (Build_graph.filter_for_package graph package)
        | None -> None
      in
      (* Check if the package and its dependencies are already built *)
      match filtered_graph with
      | Some graph ->
          let sorted = Build_graph.topological_sort graph in
          (* Package dependencies already logged via build graph *)
          let needs_build = sorted in
          (* Simplified: queue all packages for now *)

          if needs_build = [] then (
            (* Everything already built, return success immediately *)
            let duration_ms = 
              match state.build_start_time with
              | Some start_time -> int_of_float ((System.gettimeofday () -. start_time) *. 1000.)
              | None -> 0
            in
            Log.log ?sid:None
              (BuildComplete
                 {
                   duration_ms;
                   results = [];
                   succeeded = [ package ];
                   failed = [];
                 });
            let built_count = List.length sorted in
            let stats = {
              Rpc.duration_ms;
              packages_built = built_count;
              packages_failed = 0;
              total_modules = built_count;
              cache_hits = built_count;  (* All were cached/already built *)
              cache_misses = 0;
            } in
            send client_pid (ServerResponse (Rpc.BuildComplete stats));
            new_state)
          else
            (* Mark this as an RPC client build and set current_build_graph *)
            let build_state =
              {
                new_state with
                client_pid = Some client_pid;
                active_build_graph =
                  filtered_graph (* Use filtered graph for build *);
              }
            in
            (* Send the BuildPackage message to self to handle the actual build *)
            send (self ()) (BuildPackage { package_name = package; client_pid });
            (* Return the state with the filtered graph for this build *)
            build_state
      | None ->
          (* No build graph, need to build *)
          let new_state = { new_state with client_pid = Some client_pid } in
          send (self ()) (BuildPackage { package_name = package; client_pid });
          new_state)
  | Rpc.BuildAll -> (
      (* For RPC build, we need to scan workspace first if not done *)
      let new_state =
        if state.workspace = None then handle_scan_workspace state None
        else state
      in
      (* Check if all packages are already built *)
      match new_state.build_graph with
      | Some graph ->
          let sorted = Build_graph.topological_sort graph in
          let needs_build = sorted in
          (* Simplified: queue all packages for now *)

          if needs_build = [] then (
            (* Everything already built, return success immediately *)
            let duration_ms = 
              match state.build_start_time with
              | Some start_time -> int_of_float ((System.gettimeofday () -. start_time) *. 1000.)
              | None -> 0
            in
            Log.log ?sid:state.session_id
              (BuildComplete
                 {
                   duration_ms;
                   results = [];
                   succeeded =
                     List.map (fun n -> n.Build_node.package.name) sorted;
                   failed = [];
                 });
            let built_count = List.length sorted in
            let stats = {
              Rpc.duration_ms;
              packages_built = built_count;
              packages_failed = 0;
              total_modules = built_count;
              cache_hits = built_count;  (* All were cached/already built *)
              cache_misses = 0;
            } in
            send client_pid (ServerResponse (Rpc.BuildComplete stats));
            new_state)
          else
            (* Mark this as an RPC client build *)
            let new_state = { new_state with client_pid = Some client_pid } in
            (* Send the BuildAll message to self to handle the actual build *)
            send (self ()) (BuildAll { client_pid });
            (* Return the updated state *)
            new_state
      | None ->
          (* No build graph, need to build *)
          let new_state = { new_state with client_pid = Some client_pid } in
          send (self ()) (BuildAll { client_pid });
          new_state)
  | _ ->
      send client_pid (ServerResponse (Rpc.Error "Not implemented"));
      state

(** Main server loop *)
let rec server_loop state =
  let selector = function
    | ClientRequest (client_pid, request) ->
        `select (`client_request (client_pid, request))
    | ScanWorkspace target_package -> `select (`scan_workspace target_package)
    | BuildAll { client_pid = cli_pid } -> `select (`build_all cli_pid)
    | BuildPackage { package_name = pkg_name; client_pid = cli_pid } ->
        `select (`build_package (pkg_name, cli_pid))
    | Worker_pool.TaskAssigned { task; worker_id } ->
        `select (`task_assigned (task, worker_id))
    | Worker_pool.NoWorkersAvailable { task } -> `select (`no_workers task)
    | Worker_pool.TaskCompleted { package_name; hash } ->
        `select (`task_completed (package_name, hash))
    | Worker_pool.TaskFailed { package_name; error } ->
        `select (`task_failed (package_name, error))
    | RequeueWithDependencies { task; missing_deps } ->
        `select (`requeue (task, missing_deps))
    | Shutdown -> `select `shutdown
    | RestartServer -> `select `restart_server
    | ShutdownServer -> `select `shutdown_server
    | _ -> `skip
  in
  match receive ~selector () with
  | `client_request (client_pid, request) ->
      Printf.eprintf "[SERVER] Received ClientRequest from %s\n" (Pid.to_string client_pid);
      flush stderr;
      let new_state = handle_client_request state client_pid request in
      server_loop new_state
  | `scan_workspace target_package ->
      let new_state = handle_scan_workspace state target_package in
      server_loop new_state
  | `build_all cli_pid -> (
      Printf.eprintf "[SERVER DEBUG] BuildAll command received\n";
      flush stderr;
      Log.log ?sid:None
        (RpcRequestReceived
           { session_id = Session_id.make (); request_type = "BuildAll" });
      match state.build_graph with
      | None ->
          Log.log ?sid:None
            (CompileError
               {
                 package = "workspace";
                 file = "";
                 line = 0;
                 column = None;
                 message = "No build graph available. Run ScanWorkspace first.";
                 hint = None;
               });
          send cli_pid (ServerResponse (Rpc.Error "No build graph available"));
          server_loop state
      | Some graph ->
          Printf.eprintf "[SERVER DEBUG] Building all packages with graph...\n";
          flush stderr;
          (* Building all packages *)
          let new_state = { state with active_build_graph = Some graph } in
          let updated_state = execute_build new_state cli_pid graph in
          server_loop updated_state)
  | `build_package (pkg_name, cli_pid) -> (
      Printf.eprintf "[SERVER DEBUG] BuildPackage command received: pkg=%s\n"
        pkg_name;
      flush stderr;
      Log.package_started ?sid:state.session_id ~package:pkg_name;
      (* active_build_graph should already be set to the filtered graph *)
      match state.active_build_graph with
      | None ->
          Log.log ?sid:state.session_id
            (CompileError
               {
                 package = pkg_name;
                 file = "";
                 line = 0;
                 column = None;
                 message =
                   Printf.sprintf "No build graph available for package %s"
                     pkg_name;
                 hint = None;
               });
          send cli_pid (ServerResponse (Rpc.Error "No build graph available"));
          server_loop state
      | Some graph ->
          Printf.eprintf
            "[SERVER DEBUG] Building package %s with filtered graph...\n"
            pkg_name;
          flush stderr;
          (* Building package and dependencies *)
          let updated_state = execute_build state cli_pid graph in
          server_loop updated_state)
  | `task_assigned (task, worker_id) ->
      (* Worker pool assigned a task - just log it *)
      let pkg_name = task.Build_messages.node.Build_node.package.name in
      Log.worker_assigned ?sid:state.session_id ~worker_id ~package:pkg_name;
      server_loop state
  | `no_workers task ->
      (* No workers available - we'll retry later *)
      let pkg_name = task.Build_messages.node.Build_node.package.name in
      (* No workers available - will retry later *)
      ();
      (* The task is still in build_results as Building, it will be retried *)
      server_loop state
  | `task_completed (pkg_name, hash) ->
      handle_task_complete state pkg_name true hash;
      let is_complete, new_state = check_build_complete state in
      if is_complete then server_loop new_state else server_loop state
  | `task_failed (pkg_name, error) ->
      handle_task_complete state pkg_name false (Hasher.of_string "failed");
      let is_complete, new_state = check_build_complete state in
      if is_complete then server_loop new_state else server_loop state
  | `requeue (task, missing_deps) ->
      let pkg_name = task.node.Build_node.package.name in
      let missing_names =
        List.map (fun dep -> dep.Build_node.package.name) missing_deps
      in
      Log.dependency_missing ?sid:None ~package:pkg_name ~missing:missing_names;
      flush stdout;

      List.iter
        (fun dep_node -> queue_package_if_needed_internal state dep_node true)
        missing_deps;

      Log.log ?sid:state.session_id
        (QueuePackage { package = pkg_name; queue_type = `Waiting });
      Build_queue.add_waiting state.build_queue task;
      try_assign_work state;
      server_loop state
  | `shutdown ->
      Log.server_shutdown ?sid:None ();
      (match state.worker_pool with
      | Some pool -> Worker_pool.shutdown pool
      | None -> ());
      sleep 0.5;
      Miniriot.shutdown ~status:0;
      Process.Normal
  | `restart_server ->
      Log.log ?sid:None ServerShutdown;
      (* Shutdown worker pool if it exists *)
      (match state.worker_pool with
      | Some pool_pid ->
          Worker_pool.shutdown pool_pid;
          sleep 0.1
      | None -> ());
      (* Re-scan workspace and restart *)
      let new_state =
        handle_scan_workspace { state with worker_pool = None } None
      in
      Log.server_restarted ?sid:None
        ~packages:
          (match new_state.workspace with
          | Some ws -> List.length ws.packages
          | None -> 0)
        ~toolchain:(Toolchains.get_version new_state.toolchain);
      server_loop new_state
  | `shutdown_server ->
      Log.log ?sid:None ServerShutdown;
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

(** Start the build server *)
let start () =
  (* Initialize logger process first *)
  let _logger_pid = Log.init () in

  (* Scan workspace first *)
  let root = System.getcwd () in
  let workspace = Workspace_manager.get_workspace ~root in
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
      session_id = None;
      toolchain;
      build_start_time = None;
    }
  in
  spawn (fun () ->
      Log.server_started ?sid:None ~pid:(Pid.to_string (self ()));
      server_loop initial_state)

(** Start the server with TCP listener for RPC. This function makes the current
    process _become_ the Tusk server *)
let start_with_listener () =
  (* Initialize logger process first *)
  let _logger_pid = Log.init () in

  (* Scan workspace first *)
  let root = Sys.getcwd () in
  let workspace = Workspace_manager.get_workspace ~root in
  (* Ready toolchain with workspace *)
  let toolchain = Toolchains.ready_toolchains workspace in
  let build_graph =
    if workspace.packages = [] then None
    else Some (Build_graph.create workspace toolchain)
  in

  (* No longer generating .merlin files - using dynamic Merlin protocol support *)
  let build_results = Build_results.create () in
  let initial_state =
    {
      workspace = Some workspace;
      build_graph;
      active_build_graph = build_graph;
      (* Initially same as full graph *)
      build_results;
      worker_pool = None;
      build_queue = Build_queue.create build_results;
      client_pid = None;
      session_id = None;
      toolchain;
      build_start_time = None;
    }
  in

  (* Start server process *)
  let my_pid = self () in
  Log.server_started ?sid:None ~pid:(Pid.to_string my_pid);

  (* Start the TCP listener in a separate process *)
  let _ = spawn (fun () -> Listener.start my_pid) in

  server_loop initial_state
