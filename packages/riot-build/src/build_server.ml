(** Build worker - Handles build execution in a spawned process. *)
open Std
open Riot_model
open Riot_executor

(* Build workers only report build progress and results back to the local
   session. *)

let resolve_profile = fun ~(workspace:Workspace.t) profile_name ->
  let base_profile =
    match profile_name with
    | "release" -> Profile.release
    | "debug" -> Profile.debug
    | name -> { Profile.debug with name }
  in
  Profile.apply_overrides base_profile workspace.profile_overrides

let init = fun ~(workspace:Workspace.t) ~load_errors ~toolchain ~concurrency ~session_id ~client_pid ~runtime_pid ~target ~scope ~profile_name ~target_arch ->
  Log.debug
    (
      "Build worker started for session " ^ Session_id.to_string session_id ^ (
        match target_arch with
        | Some arch -> ", target_arch: " ^ Riot_model.Target.to_string arch
        | None -> ""
      )
    );
  send
    client_pid
    (Protocol.ServerResponse (Protocol.BuildStarted { session_id; started_at = DateTime.now () }));
  let stats = Protocol.BuildStats.make () in
  Protocol.BuildStats.mark_started stats;
  let handler_name = "build-worker-" ^ Session_id.to_string session_id in
  Telemetry.attach handler_name
    (fun event ->
      (
        match event with
        | Riot_executor.Telemetry_events.CacheHit _ -> Protocol.BuildStats.inc_action_cache_hits stats
        | Riot_executor.Telemetry_events.CacheMiss _ -> Protocol.BuildStats.inc_action_cache_misses stats
        | _ -> ()
      );
      (* Filter events by session_id to prevent cross-contamination *)
      let event_session_id =
        match event with
        | Riot_executor.Telemetry_events.WorkspacePlanStarted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.WorkspacePlanCompleted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.WorkspaceManifestFilterCompleted { session_id; _ } ->
            Some session_id
        | Riot_executor.Telemetry_events.WorkspaceGraphCreated { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.WorkspaceTargetGraphFiltered { session_id; _ } ->
            Some session_id
        | Riot_executor.Telemetry_events.WorkspaceTopologicalSortCompleted { session_id; _ } ->
            Some session_id
        | Riot_executor.Telemetry_events.PlanningWorkspaceStarted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.BuildStarted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.CompilationStarted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.PackageOcamlcWarnings { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.BuildCompleted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.BuildFailed { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.BuildSkipped { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.PlanningWorkspaceCompleted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.PackagePlanningResult { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.PackagePlanningBreakdown { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.WorkspaceStarted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.WorkspaceCompleted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.ActionStarted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.ActionCommandStarted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.ActionCompleted { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.ActionFailed { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.CacheHit { session_id; _ } -> Some session_id
        | Riot_executor.Telemetry_events.CacheMiss { session_id; _ } -> Some session_id
        | _ -> None
      in
      match event_session_id with
      | Some event_sid when Session_id.to_string event_sid = Session_id.to_string session_id -> send
        client_pid
        (Protocol.ServerResponse (Protocol.BuildEvent { session_id; event }))
      | _ ->
          (* Skip events from other sessions or non-build events *)
          ());
  let planner_target =
    match target with
    | Protocol.All -> Riot_planner.Workspace_planner.All
    | Protocol.Package name -> Riot_planner.Workspace_planner.Package name
    | Protocol.Packages names -> Riot_planner.Workspace_planner.Packages names
  in
  (* Check for package load errors first *)
  if List.length load_errors > 0 then
    (
      let error_msg = load_errors
      |> List.map ~fn:Workspace_manager.load_error_to_string
      |> String.concat "\n" in
      send
        client_pid
        (Protocol.ServerResponse (Protocol.PlanningFailed {
          session_id;
          failed_at = DateTime.now ();
          reason = "Could not load external packages:\n" ^ error_msg
        }))
    )
  else (
    Log.debug "Build worker calling Coordinator.build_workspace";
    let profile = resolve_profile ~workspace profile_name in
    Log.debug ("Build started with profile " ^ (Data.Json.to_string (Profile.to_json profile)));
    let config = Riot_model.Toolchain_config.from_root ~root:workspace.Workspace.root in
    let toolchain, compilation_mode =
      match target_arch with
      | Some target_triplet ->
          Log.info
            ("✓ Using target architecture: "
            ^ Riot_model.Target.to_string target_triplet
            ^ " -> os="
            ^ target_triplet.os);
          let host_triplet = System.host_triple in
          if System.TargetTriple.equal target_triplet host_triplet then
            (
              Log.info "Target matches host - native compilation";
              (toolchain, Riot_model.Build_ctx.HostOnly)
            )
          else (
            Log.info
              ("Cross-compiling from "
              ^ System.TargetTriple.to_string host_triplet
              ^ " to "
              ^ System.TargetTriple.to_string target_triplet);
            let resolved_toolchain =
              match Riot_toolchain.init_for_target ~config ~target:target_triplet with
              | Ok tc ->
                  Log.debug
                    ("Using cross-compilation toolchain for "
                    ^ Riot_model.Target.to_string target_triplet);
                  tc
              | Error msg ->
                  Log.error
                    ("Failed to init toolchain for "
                    ^ Riot_model.Target.to_string target_triplet
                    ^ ": "
                    ^ msg
                    ^ ", using host toolchain");
                  toolchain
            in
            let toolchain_root =
              Path.(Riot_model.Riot_dirs.dot_riot
              / Path.v "toolchains"
              / Path.v config.version
              / Path.v (Riot_model.Target.to_string target_triplet)) in
            let detection =
              Riot_toolchain.CrossCompilingToolchain.detect ~toolchain_root () ~target_triplet
            in
            (
              resolved_toolchain,
              Riot_model.Build_ctx.Cross {
                target = target_triplet;
                sysroot = detection.sysroot;
                bin_dir = detection.bin_dir;
                bin_prefix = detection.bin_prefix;
              }
            )
          )
      | None -> (toolchain, Riot_model.Build_ctx.HostOnly)
    in
    let build_ctx =
      Build_ctx.make
        ~session_id
        ~profile
        ~compilation_mode
        ~available_parallelism:concurrency
        ()
    in
    let target_triplet = Build_ctx.target_triplet build_ctx in
    let store = Riot_store.Store.create_for_lane ~workspace ~profile:profile.name ~target:target_triplet in
    Log.info
      ("Build context created: target_platform="
      ^ (Build_ctx.target_platform_name build_ctx)
      ^ ", host_platform="
      ^ (Build_ctx.host_platform_name build_ctx));
    let result =
      Coordinator.build_workspace ~workspace ~toolchain ~store ~target:planner_target
        ~scope:(
          match scope with
          | Protocol.Runtime -> Riot_planner.Package_graph.Runtime
          | Protocol.Dev -> Riot_planner.Package_graph.Dev
        )
        ~concurrency
        ~build_ctx
        ~session_id
    in
    Log.debug "Build worker finished, sending result to client";
    match result with
    | Ok workspace_result ->
        Protocol.BuildStats.set_total_modules stats (List.length workspace_result.results);
        List.for_each
          workspace_result.results
          ~fn:(fun (result: Package_builder.build_result) ->
            match result.status with
            | Package_builder.Built _ ->
                Protocol.BuildStats.inc_packages_built stats;
                Protocol.BuildStats.inc_cache_misses stats
            | Package_builder.Cached _ ->
                Protocol.BuildStats.inc_cache_hits stats
            | Package_builder.Skipped _ ->
                ()
            | Package_builder.Failed _ ->
                Protocol.BuildStats.inc_packages_failed stats);
        Protocol.BuildStats.mark_completed stats;
        if workspace_result.failed_count > 0 then
          (
            let errors =
              List.filter
                workspace_result.results
                ~fn:(fun (result: Package_builder.build_result) ->
                  match result.status with
                  | Package_builder.Failed _ -> true
                  | Package_builder.Skipped _
                  | Package_builder.Built _
                  | Package_builder.Cached _ -> false)
            in
            let built =
              List.filter
                workspace_result.results
                ~fn:(fun (result: Package_builder.build_result) ->
                  match result.status with
                  | Package_builder.Failed _ -> false
                  | Package_builder.Skipped _
                  | Package_builder.Built _
                  | Package_builder.Cached _ -> true)
            in
            send client_pid
              (
                Protocol.ServerResponse (
                  Protocol.BuildFailed {
                    session_id;
                    failed_at = DateTime.now ();
                    stats;
                    built;
                    errors;
                  }
                )
              );
            (* Keep the server's canonical graph runtime-shaped. *)
            (
              match scope with
              | Protocol.Runtime -> send
                runtime_pid
                (Protocol.UpdatePackageGraph workspace_result.package_graph)
              | Protocol.Dev -> ()
            )
          )
        else (
          send
            client_pid
            (Protocol.ServerResponse (Protocol.BuildCompleted {
              session_id;
              completed_at = DateTime.now ();
              stats;
              results = workspace_result.results
            }));
          (
            match scope with
            | Protocol.Runtime -> send
              runtime_pid
              (Protocol.UpdatePackageGraph workspace_result.package_graph)
            | Protocol.Dev -> ()
          )
        )
    | Error err -> (
        match err with
        | Riot_planner.Workspace_planner.PackageNotFound { name; available } ->
            send
              client_pid
              (Protocol.ServerResponse (Protocol.PackageNotFound {
                session_id;
                package_name = name;
                available_packages = available
              }))
        | Riot_planner.Workspace_planner.PackagesNotFound { names; available } ->
            send
              client_pid
              (Protocol.ServerResponse (Protocol.PackagesNotFound {
                session_id;
                package_names = names;
                available_packages = available
              }))
        | Riot_planner.Workspace_planner.CycleDetected { cycle } ->
            send
              client_pid
              (Protocol.ServerResponse (Protocol.CycleDetected {
                session_id;
                cycle_nodes = cycle;
                detected_at = DateTime.now ()
              }))
        | Riot_planner.Workspace_planner.MissingDependencies { missing } ->
            Log.error "Planning failed: Missing dependencies";
            List.for_each
              missing
              ~fn:(fun { Riot_planner.Package_graph.package; dependency } ->
                Log.error ("  " ^ package ^ " requires: " ^ dependency));
            (* Group missing deps by package for cleaner error messages *)
            let grouped = Collections.HashMap.create () in
            List.for_each
              missing
              ~fn:(fun { Riot_planner.Package_graph.package; dependency } ->
                match Collections.HashMap.get grouped ~key:package with
                | None ->
                    let _ = Collections.HashMap.insert grouped ~key:package ~value:[ dependency ] in
                    ()
                | Some deps ->
                    let _ = Collections.HashMap.insert grouped ~key:package ~value:(dependency :: deps) in
                    ());
            let error_msg = grouped
            |> Collections.HashMap.to_list
              |> List.map
              ~fn:(fun ((pkg, deps)) ->
                "  • " ^ pkg ^ " requires: " ^ String.concat ", " (List.reverse deps))
            |> String.concat "\n" in
            send
              client_pid
              (Protocol.ServerResponse (Protocol.PlanningFailed {
                session_id;
                failed_at = DateTime.now ();
                reason = "Missing dependencies:\n" ^ error_msg
              }))
        | Riot_planner.Workspace_planner.PackageLoadFailed { errors } ->
            Log.error "Planning failed: Could not load external packages";
            List.for_each errors ~fn:(fun err ->
              Log.error ("  " ^ Workspace_manager.load_error_to_string err));
            let error_msg = errors
            |> List.map ~fn:Workspace_manager.load_error_to_string
            |> String.concat "\n  " in
            send
              client_pid
              (Protocol.ServerResponse (Protocol.PlanningFailed {
                session_id;
                failed_at = DateTime.now ();
                reason = "Could not load external packages:\n  " ^ error_msg
              }))
      )
  );
  Telemetry.detach handler_name;
  Log.debug "Build worker exiting";
  Ok ()

(** Start a build in a spawned worker process *)
let start = fun ~workspace ~load_errors ~toolchain ~concurrency ~session_id ~client_pid ~runtime_pid ~target ~scope ~profile ~target_arch ->
  let _ =
    spawn
      (fun () ->
        init
          ~workspace
          ~load_errors
          ~toolchain
          ~concurrency
          ~session_id
          ~client_pid
          ~runtime_pid
          ~target
          ~scope
          ~profile_name:profile
          ~target_arch)
  in
  ()
