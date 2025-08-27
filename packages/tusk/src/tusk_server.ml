(** Build server - Miniriot process that orchestrates builds *)

open Miniriot
open Tusk_protocol

type t = Pid.t
(** Server handle is just a PID *)

type state = {
  active_build_graph : Build_graph.t;
      (* Graph for current build (full or filtered) *)
  build_graph : Build_graph.t; (* Full workspace build graph *)
  build_queue : Build_queue.t; (* Two-queue system for dependency ordering *)
  build_results : Build_results.t;
  build_start_time : float option; (* Start time for build stats *)
  toolchain : Toolchains.toolchain; (* Current toolchain *)
  worker_pool : Worker_pool.t option; (* Handle to the worker pool if active *)
  workspace : Workspace.t;
  workers : int; (* Number of workers to use *)
}
(** Server state *)

(** this is the main server loop where we'll handle all the incoming requests
    from clients, be it the CLI, MCP, LSP, or direct RPC communication.

    None of these handlers can really block the loop, so we gotta handle and
    dispatch, except restart and shutdown *)
let rec loop state =
  let selector msg =
    match msg with ServerRequest req -> `select req | _ -> `skip
  in

  match receive ~selector () with
  | Ping { client_pid } -> handle_ping state client_pid
  | Build { client_pid; target; session_id } ->
      handle_build state client_pid target session_id
  | ScanWorkspace { client_pid; current_dir } ->
      handle_scan_workspace state client_pid current_dir
  | GetWorkspaceConfig { client_pid } ->
      handle_get_workspace_config state client_pid
  | GetPackageInfo { client_pid; package_name } ->
      handle_get_package_info state client_pid package_name
  | GetBuildGraph { client_pid } -> handle_get_build_graph state client_pid

(** Handler for the ping message. *)
and handle_ping state client_pid =
  Printf.eprintf "Server: Received Ping from %s, sending Pong\n"
    (Pid.to_string client_pid);
  send client_pid (ServerResponse Pong);
  loop state

(** Handler for the scan workspace message. *)
and handle_scan_workspace state client_pid current_dir =
  (* Rescan the workspace and update state *)
  let workspace =
    Workspace_manager.scan current_dir
    |> Std.Result.expect ~msg:"tusk_server: operation failed"
  in
  let build_graph = Build_graph.create workspace state.toolchain in
  let new_state = { state with workspace; build_graph } in
  (* Send build completed to signal scan is done *)
  (* For scan workspace, we don't have a build session *)
  send client_pid
    (ServerResponse
       (BuildCompleted
          { session_id = Session_id.make (); completed_At = Datetime.now () }));
  loop new_state

(** Handler for getting workspace configuration. *)
and handle_get_workspace_config state client_pid =
  Printf.eprintf "Server: Received GetWorkspaceConfig from %s\n"
    (Pid.to_string client_pid);
  (* Send the current workspace and toolchain information *)
  send client_pid
    (ServerResponse
       (WorkspaceConfig
          { workspace = state.workspace; toolchain = state.toolchain }));
  loop state

(** Handler for getting package information. *)
and handle_get_package_info state client_pid package_name =
  Printf.eprintf "Server: Received GetPackageInfo for %s from %s\n" package_name
    (Pid.to_string client_pid);

  (* Find the package in the workspace *)
  let package_opt =
    List.find_opt
      (fun (pkg : Workspace.package) -> pkg.name = package_name)
      state.workspace.packages
  in

  match package_opt with
  | None ->
      (* Package not found *)
      Printf.eprintf "Server: Package %s not found\n" package_name;
      send client_pid
        (ServerResponse
           (PackageInfo
              {
                package =
                  {
                    name = package_name;
                    path = Std.Path.of_string "" |> Result.unwrap;
                    relative_path = Std.Path.of_string "" |> Result.unwrap;
                    dependencies = [];
                  };
                sources = [];
                dependencies = [];
              }));
      loop state
  | Some package -> (
      (* Find the build node for this package *)
      let node_opt = Build_graph.find_node state.build_graph package_name in

      match node_opt with
      | None ->
          (* No build node, return package without sources *)
          send client_pid
            (ServerResponse
               (PackageInfo { package; sources = []; dependencies = [] }));
          loop state
      | Some node ->
          (* Return package with sources and dependencies *)
          (* Resolve dependency IDs to actual nodes for the client *)
          let dep_nodes =
            List.map
              (Build_graph.get_node state.build_graph)
              node.Build_node.deps
          in
          send client_pid
            (ServerResponse
               (PackageInfo
                  {
                    package;
                    sources = node.Build_node.srcs;
                    dependencies = dep_nodes;
                  }));
          loop state)

(** Handler for getting the build graph. *)
and handle_get_build_graph state client_pid =
  Printf.eprintf "Server: Received GetBuildGraph from %s\n"
    (Pid.to_string client_pid);

  (* Get all nodes from the build graph using topological sort *)
  let nodes =
    try Build_graph.topological_sort state.build_graph
    with Build_graph.Cycle_detected cycle_nodes ->
      (* Just return empty list if there's a cycle - the client can detect this separately *)
      []
  in

  (* Send the build graph *)
  send client_pid (ServerResponse (BuildGraph { nodes }));
  loop state

(** Handler for the build message. *)
and handle_build state client_pid target session_id_opt =
  Printf.eprintf "Server: handle_build called for target: %s\n"
    (match target with
    | All -> "All"
    | Package p -> Printf.sprintf "Package(%s)" p);
  flush stderr;
  let server_pid = self () in
  let _ =
    spawn (fun () ->
        Printf.eprintf "Server: Build process spawned\n";
        flush stderr;

        (* Create session ID if not provided *)
        let session_id =
          match session_id_opt with
          | Some sid -> sid
          | None -> Session_id.make ()
        in

        (* Send BuildStarted response with session_id *)
        send client_pid
          (ServerResponse
             (BuildStarted { session_id; started_at = Datetime.now () }));

        (* 1. on every build we refresh the workspace *)
        Log.workspace_scanning ~session_id ();
        let workspace_start = Global.time_ms () in
        let workspace =
          Workspace_manager.scan state.workspace.root
          |> Std.Result.expect ~msg:"tusk_server: operation failed"
        in
        let workspace_duration = Global.time_ms () - workspace_start in
        Log.workspace_scanned ~session_id
          ~packages:(List.length workspace.packages)
          ~duration_ms:workspace_duration;
        Printf.eprintf "Server: Workspace scanned, found %d packages\n"
          (List.length workspace.packages);
        flush stderr;

        (* 2. recreate the build graph from the refreshed workspace *)
        Log.build_graph_creating ~session_id ();
        let graph_start = Global.time_ms () in
        let fresh_build_graph = Build_graph.create workspace state.toolchain in
        let graph_duration = Global.time_ms () - graph_start in
        let node_count = Build_graph.size fresh_build_graph in
        Log.build_graph_created ~session_id ~nodes:node_count
          ~duration_ms:graph_duration;

        (* 3. compute and queue the target build graph (this could be the whole build graph or a subset) *)
        let target_graph =
          match target with
          | All -> fresh_build_graph
          | Package pkg -> Build_graph.filter_for_package fresh_build_graph pkg
        in

        (* Try to sort the graph - if there's a cycle, report it and bail out *)
        let nodes_result =
          try Ok (Build_graph.topological_sort target_graph)
          with Build_graph.Cycle_detected cycle_nodes -> Error cycle_nodes
        in

        match nodes_result with
        | Error cycle_nodes ->
            Printf.eprintf "Server: Cycle detected involving packages: %s\n"
              (String.concat ", " cycle_nodes);
            flush stderr;

            (* Send cycle detected event to client *)
            send client_pid
              (ServerResponse
                 (CycleDetected
                    { session_id; cycle_nodes; detected_at = Datetime.now () }));

            (* Send build completed event to properly finish the build *)
            send client_pid
              (ServerResponse
                 (BuildCompleted { session_id; completed_At = Datetime.now () }));

            (* Exit the build process since we can't proceed *)
            Ok ()
        | Ok nodes ->
            Printf.eprintf "Server: Build starting with %d nodes\n"
              (List.length nodes);
            flush stderr;

            (* Log build started event *)
            let packages =
              List.map (fun n -> n.Build_node.package.name) nodes
            in
            let total_modules = 0 in
            (* TODO: Count actual modules when available *)
            Log.build_started ~session_id ~packages ~total_modules
              ~workers:state.workers;

            (* Create fresh build_results and build_queue for this build request *)
            let build_results = Build_results.create () in
            let build_queue = Build_queue.create build_results in

            (* Queue all nodes and mark them as pending *)
            List.iter
              (fun node ->
                Build_queue.queue build_queue node;
                Build_results.mark_pending build_results node)
              nodes;
            Printf.eprintf "Server: Queued %d packages: %s\n"
              (List.length packages)
              (String.concat ", " packages);
            flush stderr;

            (* Debug: Check queue state *)
            let ready, waiting, busy = Build_queue.get_stats build_queue in
            Printf.eprintf
              "Server: Queue state - Ready: %d, Waiting: %d, Busy: %d\n" ready
              waiting busy;
            flush stderr;

            (* 4. create a worker pool to execute this build *)
            let store = Store.create ~workspace in
            let worker_pool =
              Worker_pool.start ~workers:state.workers ~provider:(self ())
                ~build_graph:target_graph ~build_results ~workspace ~store
                ~worker_fn:Build_worker.main ()
            in

            (* Track build statistics *)
            let build_start_time = Global.time_ms () in
            let succeeded = ref [] in
            let failed = ref [] in

            (* 5. enter the build loop *)
            let selector msg =
              match msg with
              | Worker_pool_types.Worker msg -> `select msg
              | _ -> `skip
            in

            let rec build_loop () =
              if Build_results.all_done build_results then (
                Printf.eprintf "Server: All builds done\n";
                flush stderr;
                ())
              else
                match receive ~selector () with
                | Worker_pool_types.TaskCompleted { worker; node; artifact } ->
                    let pkg_name = node.Build_node.package.name in
                    Printf.eprintf "[DEBUG] Task completed: %s\n" pkg_name;
                    Build_queue.mark_as_completed build_queue node ~artifact;
                    succeeded := pkg_name :: !succeeded;
                    Printf.eprintf "[DEBUG] Marked %s as completed\n" pkg_name;

                    (* Log queue status after completion *)
                    let ready, later, busy =
                      Build_queue.get_stats build_queue
                    in
                    Printf.eprintf
                      "[DEBUG] Queue state after %s: Ready=%d, Later=%d, Busy=%d\n"
                      pkg_name ready later busy;
                    flush stderr;
                    build_loop ()
                | Worker_pool_types.TaskFailed { worker; node; error } ->
                    Build_queue.mark_as_failed build_queue node ~error;
                    failed := node.Build_node.package.name :: !failed;
                    Printf.eprintf "[DEBUG] Task failed: %s - %s\n"
                      node.Build_node.package.name error;
                    flush stderr;
                    build_loop ()
                | Worker_pool_types.WorkerReady worker ->
                    Printf.eprintf "Server: Worker ready\n";
                    flush stderr;
                    let () =
                      match Build_queue.next build_queue with
                      | None ->
                          Printf.eprintf
                            "Server: No work available for worker\n";
                          flush stderr;
                          ()
                      | Some node ->
                          Printf.eprintf "Server: Sending task %s to worker\n"
                            node.Build_node.package.name;
                          flush stderr;
                          (* Create simple task with just node and session *)
                          let task = Worker_pool_types.{ node; session_id } in
                          Worker_pool.send_task worker task
                    in
                    build_loop ()
                | Worker_pool_types.RequeueWithDependencies
                    { worker; node; deps } ->
                    let pkg_name = node.Build_node.package.name in
                    let dep_names =
                      List.map (fun d -> d.Build_node.package.name) deps
                    in
                    Printf.eprintf "[DEBUG] Requeuing %s with deps: [%s]\n"
                      pkg_name
                      (String.concat ", " dep_names);

                    (* requeue_with_deps will handle removing from busy tasks *)
                    Build_queue.requeue_with_deps build_queue node ~deps;

                    let ready, later, busy =
                      Build_queue.get_stats build_queue
                    in
                    Printf.eprintf
                      "[DEBUG] Queue state after requeue: Ready=%d, Later=%d, \
                       Busy=%d\n"
                      ready later busy;
                    flush stderr;
                    build_loop ()
                | _ ->
                    (* Ignore other messages *)
                    build_loop ()
            in

            Fun.protect
              ~finally:(fun () ->
                (* Log build complete event *)
                let duration_ms = Global.time_ms () - build_start_time in
                (* TODO: Get actual results from build_results *)
                Log.build_complete ~session_id ~duration_ms ~results:[];
                send client_pid
                  (ServerResponse
                     (BuildCompleted
                        { session_id; completed_At = Datetime.now () })))
              (fun () -> build_loop ());
            Ok ())
  in
  loop state

let start_tcp_server ~server ~port =
  spawn @@ fun () ->
  let addr = Net.Addr.tcp Net.Addr.loopback port in
  let jsonrpc_server = Tusk_jsonrpc.Server.create server in
  let handler ~req stream =
    let reply msg =
      let bytes = Bytes.of_string (msg ^ "\n") in
      let _ =
        Net.TcpStream.write stream bytes ~pos:0 ~len:(Bytes.length bytes) ()
        |> Std.Result.expect ~msg:"tusk_server: network operation failed"
      in
      ()
    in
    Jsonrpc.Server.handle_message jsonrpc_server reply req
  in
  let _ = Net.TcpServer.listen addr ~handler in
  Ok ()

(** Main server loop *)
let init ~current_dir ~workers ~port =
  let server_pid = self () in
  let workspace =
    Workspace_manager.scan current_dir
    |> Std.Result.expect ~msg:"tusk_server: operation failed"
  in
  let toolchain = Toolchains.ready_toolchains workspace in
  let build_graph = Build_graph.create workspace toolchain in
  let build_results = Build_results.create () in
  let build_queue = Build_queue.create build_results in
  let tcp_listener = start_tcp_server ~server:server_pid ~port in

  let state =
    {
      workspace;
      toolchain;
      build_graph;
      active_build_graph = build_graph;
      build_results;
      build_queue;
      worker_pool = None;
      build_start_time = None;
      workers;
    }
  in

  loop state

(** Start the server with TCP listener for RPC. This function makes the current
    process _become_ the Tusk server and spin up a sepaarate riot process for
    the listening in to tcp requests *)
let start () =
  spawn (fun () ->
      let current_dir =
        Std.Env.current_dir ()
        |> Std.Result.expect ~msg:"tusk_server: could not get current dir"
      in
      let workers = Std.available_parallelism () in
      let port = 9753 in
      init ~current_dir ~workers ~port)

(** Start with listener - makes current process become the server *)
let start_with_listener () =
  let current_dir =
    Std.Env.current_dir ()
    |> Std.Result.expect ~msg:"tusk_server: could not get current dir"
  in
  let workers = Std.available_parallelism () in
  let port = 9753 in
  init ~current_dir ~workers ~port

(** Scan workspace *)
let scan_workspace server =
  send server
    (ServerRequest
       (ScanWorkspace
          {
            client_pid = self ();
            current_dir =
              Std.Env.current_dir ()
              |> Std.Result.expect ~msg:"tusk_server: operation failed";
          }));
  let selector msg =
    match msg with
    | ServerResponse (BuildCompleted _) -> `select ()
    | _ -> `skip
  in
  match receive ~selector () with
  | () ->
      (* TODO: Return actual workspace from server response *)
      let workspace =
        Workspace_manager.scan
          (Std.Env.current_dir ()
          |> Std.Result.expect ~msg:"tusk_server: operation failed")
        |> Std.Result.expect ~msg:"tusk_server: operation failed"
      in
      Ok workspace
  | exception _ -> Error Error.ScanWorkspaceError

(** Shutdown server *)
let shutdown server =
  (* TODO: Implement proper shutdown *)
  Ok ()

(** Build all packages *)
let build_all server =
  send server
    (ServerRequest
       (Build { client_pid = self (); target = All; session_id = None }));
  let selector msg =
    match msg with
    | ServerResponse (BuildCompleted _) -> `select ()
    | _ -> `skip
  in
  match receive ~selector () with
  | () -> Ok (Build_results.create ())
  | exception _ -> Error Error.ScanWorkspaceError

(** Build specific package *)
let build_package ~name =
  (* For now, we'll need to start a server if not already running *)
  let server = start () in
  send server
    (ServerRequest
       (Build { client_pid = self (); target = Package name; session_id = None }));
  let selector msg =
    match msg with
    | ServerResponse (BuildCompleted _) -> `select ()
    | _ -> `skip
  in
  match receive ~selector () with
  | () -> Ok (Build_results.create ())
  | exception _ -> Error Error.ScanWorkspaceError
