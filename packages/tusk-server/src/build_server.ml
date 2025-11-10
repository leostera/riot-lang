(** Build server - Handles build execution in a spawned process *)

open Std
open Std.Collections
open Std.Iter

open Tusk_model
open Tusk_executor

let init ~(workspace : Workspace.t) ~load_errors ~toolchain ~store ~concurrency ~session_id ~client_pid
    ~server_pid ~target =
  Log.debug
    ("Build worker started for session " ^ Session_id.to_string session_id);

  send client_pid
    (Protocol.ServerResponse
       (Protocol.BuildStarted { session_id; started_at = Datetime.now () }));

  let handler_name =
    "build-worker-" ^ Session_id.to_string session_id
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

  (* Check for package load errors first *)
  if List.length load_errors > 0 then (
    let error_msg = 
      load_errors
      |> List.map Workspace_manager.load_error_to_string
      |> String.concat "\n"
    in
    send client_pid
      (Protocol.ServerResponse
         (Protocol.PlanningFailed
            {
              session_id;
              failed_at = Datetime.now ();
              reason = "Could not load external packages:\n" ^ error_msg;
            }))
  ) else (
    Log.debug "Build worker calling Coordinator.build_workspace";
    
    (* Create build context: start with debug profile, apply workspace overrides *)
    let profile = Profile.(apply_overrides debug workspace.profile_overrides) in
    Log.debug ("Build started with profile " ^ (Data.Json.to_string (Profile.to_json profile)));
    let build_ctx = Build_ctx.make ~session_id ~profile () in
    
    let result =
      Coordinator2.build_workspace ~workspace ~toolchain ~store
        ~target:planner_target ~concurrency ~build_ctx
    in

    Log.debug "Build worker finished, sending result to client";
    match result with
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
                  }))
      | Tusk_planner.Workspace_planner.MissingDependencies { missing } ->
          Log.error "Planning failed: Missing dependencies";
          List.iter (fun { Tusk_planner.Package_graph.package; dependency } ->
            Log.error ("  " ^ package ^ " requires: " ^ dependency)
          ) missing;
          
          (* Group missing deps by package for cleaner error messages *)
          let grouped = HashMap.create () in
          List.iter (fun { Tusk_planner.Package_graph.package; dependency } ->
            match HashMap.get grouped package with
            | None -> let _ = HashMap.insert grouped package [dependency] in ()
            | Some deps -> let _ = HashMap.insert grouped package (dependency :: deps) in ()
          ) missing;
          
          let error_msg = 
            grouped
            |> HashMap.into_iter
            |> Iterator.map ~fn:(fun (pkg, deps) ->
                "  • " ^ pkg ^ " requires: " ^ String.concat ", " (List.rev deps))
            |> Iterator.to_list
            |> String.concat "\n"
          in
          
          send client_pid
            (Protocol.ServerResponse
               (Protocol.PlanningFailed
                  {
                    session_id;
                    failed_at = Datetime.now ();
                    reason = "Missing dependencies:\n" ^ error_msg;
                  }))
      | Tusk_planner.Workspace_planner.PackageLoadFailed { errors } ->
          Log.error "Planning failed: Could not load external packages";
          List.iter (fun err ->
            Log.error ("  " ^ Workspace_manager.load_error_to_string err)
          ) errors;
          
          let error_msg = 
            errors
            |> List.map Workspace_manager.load_error_to_string
            |> String.concat "\n  "
          in
          
          send client_pid
            (Protocol.ServerResponse
               (Protocol.PlanningFailed
                  {
                    session_id;
                    failed_at = Datetime.now ();
                    reason = "Could not load external packages:\n  " ^ error_msg;
                  }))
    )
  );

  Telemetry.detach handler_name;
  Log.debug "Build worker exiting";
  Ok ()

(** Start a build in a spawned worker process *)
let start ~workspace ~load_errors ~toolchain ~store ~concurrency ~session_id ~client_pid
    ~server_pid ~target =
  let _ =
    spawn (fun () ->
        init ~workspace ~load_errors ~toolchain ~store ~concurrency ~session_id ~client_pid
          ~server_pid ~target)
  in
  ()
