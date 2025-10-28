open Std
open Std.Collections
open Miniriot
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

let build_workspace ~workspace ~toolchain ~store ~target ~concurrency =
  let start = Time.Instant.now () in
  Log.debug "Starting workspace build";

  match Tusk_planner.plan_workspace ~workspace ~target with
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
                  Package_builder.build ~workspace ~toolchain ~store
                    ~package_graph ~package
                in
                send owner (TaskCompleted result))
              ()
          in

          let task_ref = pool.task_ref in

          let selector msg =
            match msg with
            | WorkerPool.DynamicWorkerPool.WorkerReady worker -> (
                let worker_ref =
                  WorkerPool.DynamicWorkerPool.get_worker_task_ref worker
                in
                match Ref.type_equal task_ref worker_ref with
                | Some Type.Equal ->
                    `select
                      (`WorkerReady
                         (worker
                           : Build_queue.package_node
                             WorkerPool.DynamicWorkerPool.worker))
                | None -> `skip)
            | TaskCompleted result -> `select (`TaskCompleted result)
            | _ -> `skip
          in

          let rec dispatch_loop () =
            if
              Build_queue.is_complete queue
                ~total_packages:(List.length packages)
            then (
              let _, _, _, completed, succeeded, failed =
                Build_queue.stats queue
              in
              Log.info
                "Workspace build: all done, completed=%d succeeded=%d \
                 failed=%d total=%d"
                completed succeeded failed (List.length packages);
              ())
            else
              match receive ~selector () with
              | `WorkerReady worker -> (
                  match Build_queue.next queue with
                  | None ->
                      let ready, waiting, busy, _, _, _ =
                        Build_queue.stats queue
                      in
                      Log.debug
                        "No work available for worker (ready=%d waiting=%d \
                         busy=%d)"
                        ready waiting busy;
                      dispatch_loop ()
                  | Some node ->
                      let package = Package_graph.get_package node.value in
                      Log.debug "Dispatching package %s to worker"
                        package.Package.name;
                      WorkerPool.DynamicWorkerPool.send_task pool worker node;
                      dispatch_loop ())
              | `TaskCompleted result ->
                  Build_queue.mark_completed queue result;

                  let status_str =
                    match result.status with
                    | Cached _ -> "cached"
                    | Built _ -> "built"
                    | Failed _ -> "failed"
                  in

                  (* Don't emit BuildCompleted/BuildFailed here - package_builder already emits them *)
                  Log.info "Package %s: %s (%dms)" result.package.Package.name
                    status_str
                    (Time.Duration.to_millis result.duration);
                  dispatch_loop ()
          in

          dispatch_loop ();

          let results =
            List.filter_map
              (fun (pkg : Package.t) -> Build_queue.get_result queue pkg.name)
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

          Telemetry.emit
            (WorkspaceCompleted
               {
                 target;
                 total_duration;
                 cached_count;
                 built_count;
                 failed_count;
               });

          Ok
            { results; total_duration; cached_count; built_count; failed_count }
      )
