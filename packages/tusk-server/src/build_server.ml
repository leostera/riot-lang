(** Build server - Handles build execution in a spawned process *)

open Std

open Tusk_model
open Tusk_executor

let init ~workspace ~toolchain ~store ~concurrency ~session_id ~client_pid
    ~server_pid ~target =
  Log.debug "Build worker started for session %s"
    (Session_id.to_string session_id);

  send client_pid
    (Protocol.ServerResponse
       (Protocol.BuildStarted { session_id; started_at = Datetime.now () }));

  let handler_name =
    format "build-worker-%s" (Session_id.to_string session_id)
  in
  Telemetry.attach handler_name (fun event ->
      send client_pid
        (Protocol.ServerResponse (Protocol.BuildEvent { session_id; event })));

  let planner_target =
    match target with
    | Protocol.All -> Tusk_planner.Workspace_planner.All
    | Protocol.Package name -> Tusk_planner.Workspace_planner.Package name
  in

  let stats = Protocol.BuildStats.make () in
  Protocol.BuildStats.mark_started stats;

  Log.debug "Build worker calling Coordinator.build_workspace";
  let result =
    Coordinator2.build_workspace ~workspace ~toolchain ~store
      ~target:planner_target ~concurrency
  in

  Log.debug "Build worker finished, sending result to client";
  (match result with
  | Ok workspace_result ->
      Protocol.BuildStats.set_total_modules stats
        (List.length workspace_result.results);

      List.iter
        (fun (result : Package_builder.build_result) ->
          match result.status with
          | Package_builder.Built _ ->
              Protocol.BuildStats.inc_packages_built stats;
              Protocol.BuildStats.inc_cache_misses stats
          | Package_builder.Cached _ -> Protocol.BuildStats.inc_cache_hits stats
          | Package_builder.Failed _ ->
              Protocol.BuildStats.inc_packages_failed stats)
        workspace_result.results;

      Protocol.BuildStats.mark_completed stats;

      if workspace_result.failed_count > 0 then (
        let errors =
          List.filter
            (fun (result : Package_builder.build_result) ->
              match result.status with
              | Package_builder.Failed _ -> true
              | _ -> false)
            workspace_result.results
        in
        let built =
          List.filter
            (fun (result : Package_builder.build_result) ->
              match result.status with
              | Package_builder.Failed _ -> false
              | _ -> true)
            workspace_result.results
        in
        send client_pid
          (Protocol.ServerResponse
             (Protocol.BuildFailed
                {
                  session_id;
                  failed_at = Datetime.now ();
                  stats;
                  built;
                  errors;
                }));
        (* Send updated package graph back to internal server even on failure *)
        send server_pid
          (Protocol.UpdatePackageGraph workspace_result.package_graph))
      else (
        send client_pid
          (Protocol.ServerResponse
             (Protocol.BuildCompleted
                {
                  session_id;
                  completed_at = Datetime.now ();
                  stats;
                  results = workspace_result.results;
                }));
        (* Send updated package graph back to internal server *)
        send server_pid
          (Protocol.UpdatePackageGraph workspace_result.package_graph))
  | Error err -> (
      match err with
      | Tusk_planner.Workspace_planner.PackageNotFound { name; available } ->
          send client_pid
            (Protocol.ServerResponse
               (Protocol.PackageNotFound
                  {
                    session_id;
                    package_name = name;
                    available_packages = available;
                  }))
      | Tusk_planner.Workspace_planner.CycleDetected { cycle } ->
          send client_pid
            (Protocol.ServerResponse
               (Protocol.CycleDetected
                  {
                    session_id;
                    cycle_nodes = cycle;
                    detected_at = Datetime.now ();
                  }))));

  Telemetry.detach handler_name;
  Log.debug "Build worker exiting";
  Ok ()

(** Start a build in a spawned worker process *)
let start ~workspace ~toolchain ~store ~concurrency ~session_id ~client_pid
    ~server_pid ~target =
  let _ =
    spawn (fun () ->
        init ~workspace ~toolchain ~store ~concurrency ~session_id ~client_pid
          ~server_pid ~target)
  in
  ()
