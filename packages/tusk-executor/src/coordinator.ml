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

  match plan_workspace ~workspace ~target with
  | Error err -> Error err
  | Ok plan -> (
      let packages = Workspace_planner.packages_in_plan plan in
      let package_graph = plan.package_graph in

      Telemetry.emit
        (WorkspaceStarted { target; package_count = List.length packages });

      Log.info "Building %d packages with %d workers" (List.length packages)
        concurrency;

      match Package_graph.topological_sort package_graph with
      | exception Package_graph.Cycle_detected cycle ->
          Error (Workspace_planner.CycleDetected { cycle })
      | _ ->
          let queue = Build_queue.create () in

          List.iter
            (fun package ->
              let dependencies =
                Package_graph.get_dependencies package_graph package
              in
              Build_queue.enqueue queue { package; dependencies })
            packages;

          let pool : Build_queue.package_task WorkerPool.DynamicWorkerPool.t =
            WorkerPool.DynamicWorkerPool.start ~concurrency ~owner:(self ())
              ~worker_fn:(fun ~owner ~task ->
                let { Build_queue.package; _ } = task in
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
                           : Build_queue.package_task
                             WorkerPool.DynamicWorkerPool.worker))
                | None -> `skip)
            | TaskCompleted result -> `select (`TaskCompleted result)
            | _ -> `skip
          in

          let rec dispatch_loop remaining =
            if remaining = 0 then ()
            else
              match receive ~selector () with
              | `WorkerReady worker -> (
                  match Build_queue.next queue with
                  | None ->
                      Log.debug "No work available for worker";
                      dispatch_loop remaining
                  | Some task ->
                      Log.debug "Dispatching package %s to worker"
                        task.package.Package.name;
                      WorkerPool.DynamicWorkerPool.send_task pool worker task;
                      dispatch_loop remaining)
              | `TaskCompleted result ->
                  Build_queue.mark_completed queue result;

                  let cached =
                    match result.status with Cached _ -> true | _ -> false
                  in
                  let status_str =
                    match result.status with
                    | Cached _ -> "cached"
                    | Built _ -> "built"
                    | Failed _ -> "failed"
                  in

                  (match result.status with
                  | Cached _ ->
                      Telemetry.emit
                        (BuildCompleted
                           {
                             package = result.package;
                             target;
                             status = `Cached;
                             duration = result.duration;
                           })
                  | Built _ ->
                      Telemetry.emit
                        (BuildCompleted
                           {
                             package = result.package;
                             target;
                             status = `Fresh;
                             duration = result.duration;
                           })
                  | Failed err ->
                      Telemetry.emit
                        (BuildFailed
                           {
                             package = result.package;
                             target;
                             error = Package_builder.package_error_to_string err;
                           }));

                  Log.info "Package %s: %s (%dms)" result.package.Package.name
                    status_str
                    (Time.Duration.to_millis result.duration);
                  dispatch_loop (remaining - 1)
          in

          dispatch_loop (List.length packages);

          let results =
            List.filter_map
              (fun (pkg : Package.t) ->
                match Build_queue.(HashMap.get queue.completed pkg.name) with
                | Some result -> Some result
                | None -> None)
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
