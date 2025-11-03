open Std
open Std.Collections

open Tusk_model
open Tusk_planner
open Telemetry_events

type workspace_result = {
  results : Package_builder.build_result list;
  total_duration : Time.Duration.t;
  cached_count : int;
  built_count : int;
  failed_count : int;
}

type Message.t += TaskCompleted of Package_builder.build_result

type coordinator_state = {
  packages : Package.t list;
  queue : Build_queue.t;
  pool : Build_queue.package_node WorkerPool.DynamicWorkerPool.t;
  task_ref : Build_queue.package_node Ref.t;
}

let rec loop state =
  let selector msg =
    let open WorkerPool in
    match msg with
    | TaskCompleted result -> `select (`TaskCompleted result)
    | DynamicWorkerPool.WorkerReady worker -> (
        let worker_ref = DynamicWorkerPool.get_worker_task_ref worker in
        match Ref.type_equal state.task_ref worker_ref with
        | Some Type.Equal ->
            `select
              (`WorkerReady
                 (worker
                   : Build_queue.package_node
                     WorkerPool.DynamicWorkerPool.worker))
        | None -> `skip)
    | _ -> `skip
  in

  if
    Build_queue.is_complete state.queue
      ~total_packages:(List.length state.packages)
  then handle_build_completed state
  else
    match receive ~timeout:0.010 ~selector () with
    | exception Receive_timeout -> loop state
    | `WorkerReady worker -> handle_worker_ready state worker
    | `TaskCompleted result -> handle_task_completed state result

and handle_task_completed state result =
  Build_queue.mark_completed state.queue result;

  (* Don't emit BuildCompleted/BuildFailed here - package_builder already emits them *)
  Log.info "Package %s: %s (%dms)" result.package.Package.name
    (match result.status with
    | Cached _ -> "cached"
    | Built _ -> "built"
    | Failed _ -> "failed")
    (Time.Duration.to_millis result.duration);

  loop state

and handle_worker_ready state worker =
  match Build_queue.next state.queue with
  | None ->
      (* No work available yet - retry after a short delay *)
      let _timer_ref =
        Timer.send_after (self ())
          (WorkerPool.DynamicWorkerPool.WorkerReady worker) ~after:(Time.Duration.from_secs_float 0.1)
      in
      loop state
  | Some node ->
      WorkerPool.DynamicWorkerPool.send_task state.pool worker node;
      loop state

and handle_build_completed state =
  let _, _, _, completed, succeeded, failed = Build_queue.stats state.queue in
  Log.info
    "Workspace build: all done, completed=%d succeeded=%d failed=%d total=%d"
    completed succeeded failed
    (List.length state.packages);
  ()

let init ~workspace ~toolchain ~store ~package_graph ~packages ~concurrency =
  let queue = Build_queue.create () in

  (* Queue all package nodes BEFORE starting pool *)
  Package_graph.iter_nodes package_graph ~fn:(fun node ->
      Build_queue.queue queue node);

  let pool : Build_queue.package_node WorkerPool.DynamicWorkerPool.t =
    WorkerPool.DynamicWorkerPool.start ~concurrency ~owner:(self ())
      ~worker_fn:(fun ~owner ~task ->
        let (node : Build_queue.package_node) = task in
        let package = Package_graph.get_package node.value in
        let result =
          Package_builder.build ~workspace ~toolchain ~store ~package_graph
            ~package
        in
        send owner (TaskCompleted result))
      ()
  in

  let task_ref = pool.task_ref in
  let state = { packages; queue; pool; task_ref } in

  loop state;
  state

let build_workspace ~workspace ~toolchain ~store ~target ~concurrency =
  let start = Time.Instant.now () in

  match Tusk_planner.plan_workspace ~workspace ~target ~load_errors:[] with
  | Error err -> Error err
  | Ok { packages; package_graph; _ } -> (
      Telemetry.emit
        (WorkspaceStarted { target; package_count = List.length packages });

      Log.info "Building %d packages with %d workers" (List.length packages)
        concurrency;

      match Package_graph.topological_sort package_graph with
      | exception Package_graph.Cycle_detected cycle ->
          Error (Workspace_planner.CycleDetected { cycle })
      | nodes ->
          let packages = List.map Package_graph.get_package nodes in

          (* Run coordinator to completion of the build *)
          let state =
            init ~workspace ~toolchain ~store ~package_graph ~packages
              ~concurrency
          in

          (* Collect results *)
          let results =
            List.filter_map
              (fun (pkg : Package.t) ->
                Build_queue.get_result state.queue pkg.name)
              packages
          in

          let cached_count =
            List.fold_left
              (fun acc r ->
                match r.Package_builder.status with
                | Cached _ -> acc + 1
                | _ -> acc)
              0 results
          in
          let built_count =
            List.fold_left
              (fun acc r ->
                match r.Package_builder.status with
                | Built _ -> acc + 1
                | _ -> acc)
              0 results
          in
          let failed_count =
            List.fold_left
              (fun acc r ->
                match r.Package_builder.status with
                | Failed _ -> acc + 1
                | _ -> acc)
              0 results
          in

          let total_duration =
            Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
          in

          Ok
            { results; total_duration; cached_count; built_count; failed_count }
      )
