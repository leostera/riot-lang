(** Build server - Miniriot process that orchestrates builds *)

open Miniriot
open Build_messages
open Build_node

(** Server state *)
type state = {
  workspace : Workspace.workspace option;
  build_graph : Build_graph.t option;
  build_results : Build_results.t;
  workers : Pid.t list;
  idle_workers : Pid.t Queue.t;  (* Workers waiting for tasks *)
  current_queue : string Queue.t;
  later_queue : string Queue.t;
  cli_pid : Pid.t option;  (* PID of CLI process to notify when done *)
  toolchain : Toolchains.toolchain;  (* Current toolchain *)
}

(** Get number of CPU cores *)
let get_num_cores () =
  try
    let ic = Unix.open_process_in "sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4" in
    let n = int_of_string (input_line ic) in
    close_in ic;
    n
  with _ -> 4  (* Default to 4 cores *)

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
  | Some workspace ->
      match List.find_opt (fun p -> p.Workspace.name = pkg_name) workspace.packages with
      | None -> []
      | Some pkg -> pkg.dependencies

(** Try to get next buildable task *)
let rec get_next_buildable_task state =
  match state.build_graph, state.workspace with
  | Some graph, Some workspace ->
      let nodes = Build_graph.topological_sort graph in
      
      (* First try current queue *)
      if not (Queue.is_empty state.current_queue) then
        let pkg_name = Queue.take state.current_queue in
        if can_build_package state pkg_name then
          (* Find the node for this package *)
          (match List.find_opt (fun n -> n.Build_node.package.name = pkg_name) nodes with
          | Some node -> Some { node; workspace; toolchain_version = state.toolchain.version }
          | None -> None)
        else begin
          (* Put it in later queue and try again *)
          Queue.add pkg_name state.later_queue;
          get_next_buildable_task state
        end
      (* Then try later queue *)
      else if not (Queue.is_empty state.later_queue) then
        let pkg_name = Queue.take state.later_queue in
        if can_build_package state pkg_name then
          (* Find the node for this package *)
          (match List.find_opt (fun n -> n.Build_node.package.name = pkg_name) nodes with
          | Some node -> Some { node; workspace; toolchain_version = state.toolchain.version }
          | None -> None)
        else begin
          (* Put it back and try next one *)
          Queue.add pkg_name state.later_queue;
          get_next_buildable_task state
        end
      else
        None
  | _, _ -> None

(** Check if a package can be built (all deps ready) *)
and can_build_package state pkg_name =
  match state.build_graph with
  | None -> false
  | Some graph ->
      let nodes = Build_graph.topological_sort graph in
      match List.find_opt (fun n -> n.Build_node.package.name = pkg_name) nodes with
      | None -> false
      | Some node ->
          let deps = List.map (fun dep -> dep.Build_node.package.name) node.dependencies in
          Build_results.dependencies_ready state.build_results deps

(** Try to assign work to idle workers *)
let try_assign_work state =
  let rec assign_loop () =
    if not (Queue.is_empty state.idle_workers) && not (Queue.is_empty state.current_queue || Queue.is_empty state.later_queue) then
      match get_next_buildable_task state with
      | Some build_task ->
          let worker_pid = Queue.take state.idle_workers in
          let pkg_name = build_task.node.Build_node.package.name in
          Printf.printf "[Server] Assigning package %s to worker %s\n" 
            pkg_name (Pid.to_string worker_pid);
          (* Mark as building *)
          Build_results.mark_building state.build_results pkg_name;
          send worker_pid (Task build_task);
          assign_loop ()  (* Try to assign more work *)
      | None -> ()
    else ()
  in
  assign_loop ()

(** Handle worker requesting next task *)
let handle_next_task state worker_pid =
  match get_next_buildable_task state with
  | Some build_task ->
      let pkg_name = build_task.node.Build_node.package.name in
      Printf.printf "[Server] Assigning package %s to worker %s\n" 
        pkg_name (Pid.to_string worker_pid);
      (* Mark as building *)
      Build_results.mark_building state.build_results pkg_name;
      send worker_pid (Task build_task)
  | None ->
      (* No work available, add worker to idle queue *)
      Queue.add worker_pid state.idle_workers;
      send worker_pid NoTask

(** Handle task completion *)
let handle_task_complete state pkg_name success =
  if success then begin
    Printf.printf "[Server] Build complete: %s\n" pkg_name;
    Build_results.mark_built state.build_results pkg_name;
    
    (* Check if this unblocks any packages in the later queue *)
    (* Move them to current queue if their deps are now ready *)
    let later_items = ref [] in
    while not (Queue.is_empty state.later_queue) do
      later_items := Queue.take state.later_queue :: !later_items
    done;
    
    List.iter (fun item -> 
      if can_build_package state item then
        Queue.add item state.current_queue
      else
        Queue.add item state.later_queue
    ) !later_items;
    
    (* Also queue any packages that were waiting only on this one *)
    match state.build_graph with
    | Some graph ->
        let nodes = Build_graph.topological_sort graph in
        List.iter (fun node ->
          let pkg = node.Build_node.package.name in
          if not (Build_results.is_built state.build_results pkg) &&
             not (Build_results.is_building state.build_results pkg) &&
             can_build_package state pkg then
            Queue.add pkg state.current_queue
        ) nodes
    | None -> ();
    
    (* Try to assign work to idle workers now that new tasks may be available *)
    try_assign_work state
  end else begin
    Printf.printf "[Server] Build failed: %s\n" pkg_name;
    Build_results.mark_failed state.build_results pkg_name "Build failed"
  end

(** Handle scanning the workspace *)
let handle_scan_workspace state =
  let root = Sys.getcwd () in
  Printf.printf "[Server] Scanning workspace from: %s\n" root;
  
  let workspace = Workspace.scan ~root in
  let build_graph = 
    if workspace.packages = [] then None
    else Some (Build_graph.create workspace)
  in
  
  (* Print the build graph if we have one *)
  (match build_graph with
  | None -> Printf.printf "[Server] No packages found in workspace\n"
  | Some graph -> Build_graph.print graph);
  
  { state with workspace = Some workspace; build_graph }

(** Main server loop *)
let rec server_loop state =
  match receive () with
  | ScanWorkspace ->
      let new_state = handle_scan_workspace state in
      server_loop new_state
      
  | BuildAll cli_pid ->
      Printf.printf "[Server] BuildAll command received\n";
      (match state.build_graph with
      | None -> 
          Printf.printf "[Server] No build graph available. Run ScanWorkspace first.\n";
          server_loop state
      | Some graph ->
          Printf.printf "[Server] Building all packages...\n";
          
          (* Initialize build results *)
          let sorted = Build_graph.topological_sort graph in
          let pkg_names = List.map (fun n -> n.Build_node.package.name) sorted in
          Build_results.init_packages state.build_results pkg_names;
          
          (* Queue packages with no dependencies *)
          List.iter (fun node ->
            if node.dependencies = [] then begin
              Printf.printf "[Server] Queueing package with no deps: %s\n" node.Build_node.package.name;
              Queue.add node.Build_node.package.name state.current_queue
            end
          ) sorted;
          
          Printf.printf "[Server] Current queue size: %d\n" (Queue.length state.current_queue);
          
          (* Spawn workers if not already spawned *)
          let workers = 
            if state.workers = [] then begin
              let num_cores = get_num_cores () in
              Printf.printf "[Server] Spawning %d workers\n" num_cores;
              let workers = spawn_workers (self ()) num_cores in
              (* Give workers a moment to start *)
              sleep 0.1;
              workers
            end else
              state.workers
          in
          
          let new_state = { state with workers; cli_pid = Some cli_pid } in
          (* Try to assign initial work to workers *)
          try_assign_work new_state;
          server_loop new_state)
      
  | BuildPackage (pkg_name, cli_pid) ->
      Printf.printf "[Server] BuildPackage %s command received\n" pkg_name;
      (match state.build_graph with
      | None -> 
          Printf.printf "[Server] No build graph available. Run ScanWorkspace first.\n";
          send cli_pid BuildFinished;
          server_loop state
      | Some graph ->
          (* Find the package node *)
          let nodes = Build_graph.topological_sort graph in
          (match List.find_opt (fun n -> n.Build_node.package.name = pkg_name) nodes with
          | None ->
              Printf.printf "[Server] Package '%s' not found in workspace\n" pkg_name;
              send cli_pid BuildFinished;
              server_loop state
          | Some target_node ->
              Printf.printf "[Server] Building package %s and its dependencies...\n" pkg_name;
              
              (* Get all dependencies of the target package *)
              let rec collect_deps node visited =
                if List.mem node.Build_node.package.name visited then
                  visited
                else
                  let visited = node.Build_node.package.name :: visited in
                  List.fold_left (fun acc dep ->
                    collect_deps dep acc
                  ) visited node.Build_node.dependencies
              in
              let all_deps = collect_deps target_node [] in
              
              Printf.printf "[Server] Need to build: %s\n" (String.concat ", " all_deps);
              
              (* Initialize build results for only the required packages *)
              Build_results.init_packages state.build_results all_deps;
              
              (* Find packages with no dependencies in our subset *)
              List.iter (fun dep_name ->
                match List.find_opt (fun n -> n.Build_node.package.name = dep_name) nodes with
                | Some node ->
                    (* Check if all dependencies are outside our build set or already built *)
                    let deps_ready = List.for_all (fun dep ->
                      not (List.mem dep.Build_node.package.name all_deps) ||
                      Build_results.is_built state.build_results dep.Build_node.package.name
                    ) node.Build_node.dependencies in
                    if deps_ready then begin
                      Printf.printf "[Server] Queueing package: %s\n" dep_name;
                      Queue.add dep_name state.current_queue
                    end
                | None -> ()
              ) all_deps;
              
              Printf.printf "[Server] Current queue size: %d\n" (Queue.length state.current_queue);
              
              (* Spawn workers if not already spawned *)
              let workers = 
                if state.workers = [] then begin
                  let num_cores = get_num_cores () in
                  Printf.printf "[Server] Spawning %d workers\n" num_cores;
                  let workers = spawn_workers (self ()) num_cores in
                  (* Give workers a moment to start *)
                  sleep 0.1;
                  workers
                end else
                  state.workers
              in
              
              let new_state = { state with workers; cli_pid = Some cli_pid } in
              (* Try to assign initial work to workers *)
              try_assign_work new_state;
              server_loop new_state))
      
  | NextTask worker_pid ->
      Printf.printf "[Server] Worker %s requesting task\n" (Pid.to_string worker_pid);
      handle_next_task state worker_pid;
      server_loop state
      
  | TaskComplete (pkg_name, success) ->
      handle_task_complete state pkg_name success;
      
      (* Check if all done *)
      if Build_results.all_done state.build_results then begin
        let (built, failed, _building, _not_started) = 
          Build_results.get_stats state.build_results in
        Printf.printf "[Server] All builds complete! Built: %d, Failed: %d\n" 
          built failed;
        
        (* Notify CLI that we're done *)
        (match state.cli_pid with
        | Some pid -> send pid BuildFinished
        | None -> ());
        
        (* Shutdown workers *)
        List.iter (fun w -> send w Shutdown) state.workers;
        
        (* Give workers time to shutdown *)
        sleep 0.5;
        Process.Normal
      end else
        server_loop state
      
  | Shutdown ->
      Printf.printf "[Server] Shutting down...\n";
      List.iter (fun w -> send w Shutdown) state.workers;
      sleep 0.5;
      Process.Normal
      
  | _ ->
      (* Ignore unknown messages *)
      server_loop state

(** Start the build server *)
let start () =
  (* Ready toolchain at startup *)
  let root = Sys.getcwd () in
  let toolchain = Toolchains.ready_toolchains root in
  
  let initial_state = { 
    workspace = None; 
    build_graph = None;
    build_results = Build_results.create ();
    workers = [];
    idle_workers = Queue.create ();
    current_queue = Queue.create ();
    later_queue = Queue.create ();
    cli_pid = None;
    toolchain;
  } in
  spawn (fun () -> 
    Printf.printf "[Server] Build server started (pid: %s)\n" 
      (Pid.to_string (self ()));
    server_loop initial_state
  )
