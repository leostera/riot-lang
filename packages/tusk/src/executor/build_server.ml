open Std
open Miniriot
open Core
open Model
open Workspace
open Tusk_protocol

let do_build ~session_id ~client_pid ~build_results ~build_queue ~build_stats
    ~store =
  let selector msg =
    match msg with Worker_pool_types.Worker msg -> `select msg | _ -> `skip
  in

  (* Track build statistics *)
  let build_start_time = Time.Instant.now () in
  let succeeded = ref [] in
  let failed = ref [] in

  let rec build_loop () =
    if Build_results.all_done build_results then (
      Printf.eprintf "Server: All builds done\n%!";
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
          let ready, later, busy = Build_queue.get_stats build_queue in
          Printf.eprintf
            "[DEBUG] Queue state after %s: Ready=%d, Later=%d, Busy=%d\n%!"
            pkg_name ready later busy;
          build_loop ()
      | Worker_pool_types.TaskFailed { worker; node; error } ->
          let pkg_name = node.Build_node.package.name in
          Printf.printf "[BUILD_SERVER] Package %s failed: %s\n%!" pkg_name
            error;
          Build_queue.mark_as_failed build_queue node ~error;
          failed := pkg_name :: !failed;
          Tusk_protocol.BuildStats.inc_packages_failed build_stats;
          ()
      | Worker_pool_types.WorkerReady worker ->
          Printf.eprintf "Server: Worker ready\n%!";
          let () =
            match Build_queue.next build_queue with
            | None ->
                Printf.eprintf "Server: No work available for worker\n%!";
                ()
            | Some node ->
                Printf.eprintf "Server: Sending task %s to worker %s\n%!"
                  node.Build_node.package.name
                  Pid.(to_string worker);
                (* Create simple task with just node and session *)
                let task = Worker_pool_types.{ node; session_id } in
                Worker_pool.send_task worker task
          in
          build_loop ()
      | Worker_pool_types.RequeueWithDependencies { worker; node; deps } ->
          let pkg_name = node.Build_node.package.name in
          let dep_names = List.map (fun d -> d.Build_node.package.name) deps in
          Printf.eprintf "[DEBUG] Requeuing %s with deps: [%s]\n" pkg_name
            (String.concat ", " dep_names);

          (* requeue_with_deps will handle removing from busy tasks *)
          Build_queue.requeue_with_deps build_queue node ~deps;

          let ready, later, busy = Build_queue.get_stats build_queue in
          Printf.eprintf
            "[DEBUG] Queue state after requeue: Ready=%d, Later=%d, Busy=%d\n%!"
            ready later busy;
          build_loop ()
      | _ ->
          (* Ignore other messages *)
          build_loop ()
  in

  Fun.protect
    (fun () -> build_loop ())
    ~finally:(fun () ->
      (* Log build complete event *)
      let duration_ms =
        Time.Instant.duration_since ~earlier:build_start_time
          (Time.Instant.now ())
        |> Time.Duration.to_millis
      in
      (* Get results from Build_results module *)
      let results = Build_results.to_events build_results in
      Log.build_complete ~session_id ~duration_ms ~results;

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
  Ok ()

let start ~workspace ~toolchain ~workers ~session_id ~client_pid ~target =
  spawn @@ fun () ->
  Printf.eprintf "Server: Build process spawned\n%!";

  (* Send BuildStarted response with session_id *)
  send client_pid
    (ServerResponse (BuildStarted { session_id; started_at = Datetime.now () }));

  (* 1. on every build we refresh the workspace *)
  Log.workspace_scanning ~session_id ();
  let workspace_start = Time.Instant.now () in
  let workspace =
    Workspace_manager.scan workspace.root
    |> Result.expect ~msg:"tusk_server: operation failed"
  in
  let workspace_duration =
    Time.Instant.duration_since ~earlier:workspace_start (Time.Instant.now ())
    |> Time.Duration.to_millis
  in
  Log.workspace_scanned ~session_id
    ~packages:(List.length workspace.packages)
    ~duration_ms:workspace_duration;
  Printf.eprintf "Server: Workspace scanned, found %d packages\n%!"
    (List.length workspace.packages);

  (* 2. recreate the build graph from the refreshed workspace *)
  Log.build_graph_creating ~session_id ();
  let graph_start = Time.Instant.now () in
  let fresh_build_graph = Build_graph.create workspace toolchain in
  let graph_duration =
    Time.Instant.duration_since ~earlier:graph_start (Time.Instant.now ())
    |> Time.Duration.to_millis
  in
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
      Printf.eprintf "Server: Cycle detected involving packages: %s\n%!"
        (String.concat ", " cycle_nodes);

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
      Printf.eprintf "Server: Build starting with %d nodes\n%!"
        (List.length nodes);

      (* Log build started event *)
      let packages = List.map (fun n -> n.Build_node.package.name) nodes in
      let total_modules = 0 in
      (* TODO: Count actual modules when available *)
      Log.build_started ~session_id ~packages ~total_modules ~workers;

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
      Printf.eprintf "Server: Queued %d packages: %s\n%!" (List.length packages)
        (String.concat ", " packages);

      (* Debug: Check queue state *)
      let ready, waiting, busy = Build_queue.get_stats build_queue in
      Printf.eprintf
        "Server: Queue state - Ready: %d, Waiting: %d, Busy: %d\n%!" ready
        waiting busy;

      (* 4. create a worker pool to execute this build *)
      Log.store_creating ~session_id ();
      let store_start = Time.Instant.now () in
      let store = Store.create ~workspace in
      let store_duration =
        Time.Instant.duration_since ~earlier:store_start (Time.Instant.now ())
        |> Time.Duration.to_millis
      in
      Log.store_created ~session_id ~duration_ms:store_duration;

      Log.worker_pool_creating ~session_id ~workers;
      let pool_start = Time.Instant.now () in
      let _ =
        Worker_pool.start ~workers ~provider:(self ()) ~build_graph:target_graph
          ~build_results ~workspace ~store ~worker_fn:Build_worker.main ()
      in
      let pool_duration =
        Time.Instant.duration_since ~earlier:pool_start (Time.Instant.now ())
        |> Time.Duration.to_millis
      in
      Log.worker_pool_created ~session_id ~workers ~duration_ms:pool_duration;

      (* 5. enter the build loop *)
      do_build ~session_id ~client_pid ~build_results ~build_queue ~build_stats
        ~store
