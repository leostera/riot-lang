(** Build server - Miniriot process that orchestrates builds *)

open Std
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
  | FormatFile { client_pid; file_path; check_only } ->
      handle_format_file state client_pid file_path check_only
  | FormatCode { client_pid; code; file_path } ->
      handle_format_code state client_pid code file_path
  | FormatAll { client_pid; mode } -> handle_format_all state client_pid mode
  | NewPackage { client_pid; path; name; is_library } ->
      handle_new_package state client_pid path name is_library

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
    |> Result.expect ~msg:"tusk_server: operation failed"
  in
  let build_graph = Build_graph.create workspace state.toolchain in
  let new_state = { state with workspace; build_graph } in
  (* Send build completed to signal scan is done *)
  (* For scan workspace, we don't have a build session *)
  send client_pid
    (ServerResponse
       (BuildCompleted
          {
            session_id = Session_id.make ();
            completed_At = Datetime.now ();
            stats = Tusk_protocol.BuildStats.make ();
          }));
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
                    path = Path.of_string "" |> Result.unwrap;
                    relative_path = Path.of_string "" |> Result.unwrap;
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
                    sources = List.map (fun s -> s.Build_node.file) node.Build_node.srcs;
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

and handle_format_file state client_pid file_path check_only =
  Printf.eprintf "Server: Received FormatFile from %s for %s (check_only=%b)\n"
    (Pid.to_string client_pid) (Path.to_string file_path) check_only;

  let response =
    match
      Ocamlformat.format_file ~toolchain:state.toolchain ~file_path ~check_only
    with
    | Formatted { code; changed } ->
        FormatResult { formatted_code = code; changed }
    | Error error -> FormatError { error }
  in
  send client_pid (ServerResponse response);
  loop state

and handle_format_code state client_pid code file_path =
  Printf.eprintf "Server: Received FormatCode from %s\n"
    (Pid.to_string client_pid);

  let response =
    match
      Ocamlformat.format_code ~toolchain:state.toolchain ~code ~file_path
    with
    | Formatted { code; changed } ->
        FormatResult { formatted_code = code; changed }
    | Error error -> FormatError { error }
  in
  send client_pid (ServerResponse response);
  loop state

and handle_format_all state client_pid mode =
  Printf.eprintf "Server: Received FormatAll from %s (mode=%s)\n"
    (Pid.to_string client_pid)
    (match mode with `check -> "check" | `write -> "write");

  (* TODO: Implement using GenericWorkerPool for concurrent formatting *)
  send client_pid
    (ServerResponse
       (FormatError { error = "FormatAll not yet implemented with worker pool" }));
  loop state

and handle_new_package state client_pid path name is_library =
  Printf.eprintf "Server: Received NewPackage from %s for %s at %s\n"
    (Pid.to_string client_pid) name (Path.to_string path);

  let path_str = Path.to_string path in
  let src_dir = Filename.concat path_str "src" in

  (* Create directories *)
  let _ = Command.run_command (Printf.sprintf "mkdir -p %s" src_dir) in

  (* Create main module files *)
  let main_ml = Filename.concat src_dir (name ^ ".ml") in
  let main_mli = Filename.concat src_dir (name ^ ".mli") in

  let ml_content =
    if is_library then "(** Main module for " ^ name ^ " library *)\n"
    else
      "(** Main entry point for " ^ name
      ^ " *)\n\nlet () = print_endline \"Hello from " ^ name ^ "\"\n"
  in

  let mli_content =
    if is_library then
      "(** " ^ String.capitalize_ascii name ^ " library interface *)\n"
    else "(** " ^ String.capitalize_ascii name ^ " executable interface *)\n"
  in

  (* Write module files *)
  let _ = File.write ~path:main_ml ~content:ml_content in
  let _ = File.write ~path:main_mli ~content:mli_content in

  (* Create package tusk.toml *)
  let package_toml = Filename.concat path_str "tusk.toml" in
  let toml_content =
    Printf.sprintf
      "[package]\n\
       name = \"%s\"\n\
       version = \"0.1.0\"\n\n\
       [dependencies]\n\
       std = \"*\"\n\
       # Add dependencies here\n\n\
       %s\n"
      name
      (if is_library then "" else "[package.bin]\nmain = \"" ^ name ^ "\"")
  in
  let _ = File.write ~path:package_toml ~content:toml_content in

  (* Send success response *)
  send client_pid (ServerResponse (PackageCreated { path = path_str; name }));
  loop state

(** Handler for the build message. *)
and handle_build state client_pid target session_id_opt =
  Printf.eprintf "Server: handle_build called for target: %s\n"
    (match target with
    | All -> "All"
    | Package p -> Printf.sprintf "Package(%s)" p);
  flush stderr;
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
        let workspace_start = time_ms () in
        let workspace =
          Workspace_manager.scan state.workspace.root
          |> Result.expect ~msg:"tusk_server: operation failed"
        in
        let workspace_duration = time_ms () - workspace_start in
        Log.workspace_scanned ~session_id
          ~packages:(List.length workspace.packages)
          ~duration_ms:workspace_duration;
        Printf.eprintf "Server: Workspace scanned, found %d packages\n"
          (List.length workspace.packages);
        flush stderr;

        (* 2. recreate the build graph from the refreshed workspace *)
        Log.build_graph_creating ~session_id ();
        let graph_start = time_ms () in
        let fresh_build_graph = Build_graph.create workspace state.toolchain in
        let graph_duration = time_ms () - graph_start in
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

            (* Create stats for this failed build *)
            let build_stats = Tusk_protocol.BuildStats.make () in
            Tusk_protocol.BuildStats.mark_started build_stats;
            Tusk_protocol.BuildStats.mark_completed build_stats;

            (* Send build completed event to properly finish the build *)
            send client_pid
              (ServerResponse
                 (BuildCompleted
                    {
                      session_id;
                      completed_At = Datetime.now ();
                      stats = build_stats;
                    }));

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

            (* Create mutable build stats to track throughout the build *)
            let build_stats = Tusk_protocol.BuildStats.make () in
            Tusk_protocol.BuildStats.mark_started build_stats;

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
            Log.store_creating ~session_id ();
            let store_start = time_ms () in
            let store = Store.create ~workspace in
            let store_duration = time_ms () - store_start in
            Log.store_created ~session_id ~duration_ms:store_duration;

            Log.worker_pool_creating ~session_id ~workers:state.workers;
            let pool_start = time_ms () in
            let _ =
              Worker_pool.start ~workers:state.workers ~provider:(self ())
                ~build_graph:target_graph ~build_results ~workspace ~store
                ~worker_fn:Build_worker.main ()
            in
            let pool_duration = time_ms () - pool_start in
            Log.worker_pool_created ~session_id ~workers:state.workers
              ~duration_ms:pool_duration;

            (* Track build statistics *)
            let build_start_time = time_ms () in
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
                    Tusk_protocol.BuildStats.inc_packages_built build_stats;
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
                    let pkg_name = node.Build_node.package.name in
                    Build_queue.mark_as_failed build_queue node ~error;
                    failed := pkg_name :: !failed;
                    Tusk_protocol.BuildStats.inc_packages_failed build_stats;
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
              (fun () -> build_loop ())
              ~finally:(fun () ->
                (* Log build complete event *)
                let duration_ms = time_ms () - build_start_time in
                (* TODO: Get actual results from build_results *)
                Log.build_complete ~session_id ~duration_ms ~results:[];

                (* Mark build as completed to finalize stats *)
                Tusk_protocol.BuildStats.mark_completed build_stats;

                (* Send build completed response with stats *)
                send client_pid
                  (ServerResponse
                     (BuildCompleted
                        {
                          session_id;
                          completed_At = Datetime.now ();
                          stats = build_stats;
                        })));
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
        |> Result.expect ~msg:"tusk_server: network operation failed"
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
    |> Result.expect ~msg:"tusk_server: operation failed"
  in
  let toolchain = Toolchains.ready_toolchains workspace in
  let build_graph = Build_graph.create workspace toolchain in
  let build_results = Build_results.create () in
  let build_queue = Build_queue.create build_results in
  let _ = start_tcp_server ~server:server_pid ~port in

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
        Env.current_dir ()
        |> Result.expect ~msg:"tusk_server: could not get current dir"
      in
      let workers = available_parallelism () in
      let port = 9753 in
      init ~current_dir ~workers ~port)

(** Start with listener - makes current process become the server *)
let start_with_listener () =
  let current_dir =
    Env.current_dir ()
    |> Result.expect ~msg:"tusk_server: could not get current dir"
  in
  let workers = available_parallelism () in
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
              Env.current_dir ()
              |> Result.expect ~msg:"tusk_server: operation failed";
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
          (Env.current_dir ()
          |> Result.expect ~msg:"tusk_server: operation failed")
        |> Result.expect ~msg:"tusk_server: operation failed"
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
