open Std

module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let clone_workspace_with_target = fun (workspace: Riot_model.Workspace.t) ~target_dir ->
  Riot_model.Workspace.make
    ?name:workspace.name
    ~root:workspace.root
    ~packages:workspace.packages
    ~dependencies:workspace.dependencies
    ~dev_dependencies:workspace.dev_dependencies
    ~build_dependencies:workspace.build_dependencies
    ~profile_overrides:workspace.profile_overrides
    ~target_dir:(Path.to_string target_dir)
    ()

let load_repo_workspace = fun () ->
  let manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.scan manager (Path.v ".") with
  | Error err ->
      Error ("workspace scan failed: " ^ Riot_model.Workspace_manager.scan_error_message err)
  | Ok (workspace, errors) ->
      if List.is_empty errors then
        let open Std.Result.Syntax in
        let* registry =
          Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:"pkgs.ml" ()
          |> Result.map_err
            ~fn:(fun err ->
              "registry init failed: " ^ Pkgs_ml.Registry_cache.create_error_message err)
        in
        Riot_deps.ensure_workspace
          ~workspace_manager:manager
          ~mode:Riot_deps.Dep_solver.Refresh
          ~registry
          ~workspace
          ()
        |> Result.map_err ~fn:Riot_model.Pm_error.message
      else
        Error ("workspace scan produced load errors: "
        ^ String.concat "; " (List.map errors ~fn:Riot_model.Workspace_manager.load_error_to_string))

let render_build_event = fun (event: Riot_build.Event.t) ->
  match event with
  | Riot_build.Event.Pm event -> "Pm(" ^ Riot_model.Event.name event.kind ^ ")"
  | Riot_build.Event.BuildingTarget { target; host } ->
      "BuildingTarget(" ^ Riot_model.Target.to_string target ^ "," ^ Bool.to_string host ^ ")"
  | Riot_build.Event.CacheGc _ -> "CacheGc"
  | Riot_build.Event.Telemetry _ -> "Telemetry"
  | Riot_build.Event.Phase _ -> "Phase"

let phase_name = fun __tmp1 ->
  match __tmp1 with
  | Riot_build.Event.TargetsResolved _ -> "targets_resolved"
  | Riot_build.Event.ToolchainsEnsured _ -> "toolchains_ensured"
  | Riot_build.Event.ToolchainsValidated _ -> "toolchains_validated"
  | Riot_build.Event.RuntimeStarting -> "runtime_starting"
  | Riot_build.Event.RuntimeStarted -> "runtime_started"
  | Riot_build.Event.BuildLockWaiting _ -> "build_lock_waiting"
  | Riot_build.Event.BuildLanesPreparationStarted _ -> "build_lanes_preparation_started"
  | Riot_build.Event.BuildLanesPreparationFinished _ -> "build_lanes_preparation_finished"
  | Riot_build.Event.BuildUnitPlanCreated _ -> "build_unit_plan_created"
  | Riot_build.Event.BuildLanePreparationStarted _ -> "build_lane_preparation_started"
  | Riot_build.Event.BuildLaneLockAcquired _ -> "build_lane_lock_acquired"
  | Riot_build.Event.BuildLaneToolchainInitialized _ -> "build_lane_toolchain_initialized"
  | Riot_build.Event.BuildLaneStoreCreated _ -> "build_lane_store_created"
  | Riot_build.Event.BuildLanePreparationFinished _ -> "build_lane_preparation_finished"
  | Riot_build.Event.PackagePlanningStarted _ -> "package_planning_started"
  | Riot_build.Event.PackagePlanningFinished _ -> "package_planning_finished"
  | Riot_build.Event.PackageActionGraphPlanned _ -> "package_action_graph_planned"
  | Riot_build.Event.PackageExecutionStarted _ -> "package_execution_started"
  | Riot_build.Event.PackageExecutionFinished _ -> "package_execution_finished"
  | Riot_build.Event.TargetBuildStarted _ -> "target_build_started"
  | Riot_build.Event.TargetBuildFinished _ -> "target_build_finished"
  | Riot_build.Event.CacheGenerationRecordingStarted _ -> "cache_generation_recording_started"
  | Riot_build.Event.CacheGenerationRecorded _ -> "cache_generation_recorded"
  | Riot_build.Event.ReturningResults _ -> "returning_results"

let public_phase_names = fun events ->
  List.reverse events
  |> List.filter_map
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Riot_build.Event.Phase phase -> Some (phase_name phase)
      | Riot_build.Event.Pm _
      | Riot_build.Event.BuildingTarget _
      | Riot_build.Event.Telemetry _
      | Riot_build.Event.CacheGc _ -> None)

let expect_public_phase_subsequence = fun events ->
  let haystack = public_phase_names events in
  let needle = [
    "targets_resolved";
    "toolchains_ensured";
    "toolchains_validated";
    "runtime_starting";
    "runtime_started";
    "target_build_started";
    "package_planning_started";
    "package_planning_finished";
    "package_execution_started";
    "package_execution_finished";
    "target_build_finished";
    "returning_results";
  ]
  in
  let rec loop haystack needle =
    match (haystack, needle) with
    | (_, []) -> Ok ()
    | ([], _) ->
        Error ("expected public phase subsequence "
        ^ String.concat " -> " needle
        ^ "\nactual phases: "
        ^ String.concat " -> " haystack)
    | (actual :: haystack_rest, expected :: needle_rest) ->
        if String.equal actual expected then
          loop haystack_rest needle_rest
        else
          loop haystack_rest needle
  in
  loop haystack needle

let summarize_build_failure = fun (err: Riot_build.error) events ->
  let recent_events =
    List.reverse events
    |> List.take ~len:12
    |> List.reverse
    |> List.map ~fn:render_build_event
    |> String.concat " -> "
  in
  Riot_build.error_message err ^ "\nrecent events: " ^ recent_events

let summarize_recent_events = fun events ->
  List.reverse events
  |> List.take ~len:20
  |> List.reverse
  |> List.map ~fn:render_build_event
  |> String.concat " -> "

let test_build_runtime_builds_repo_kernel = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_kernel_runtime"
    (fun tempdir ->
      match load_repo_workspace () with
      | Error _ as err -> err
      | Ok workspace ->
          let workspace =
            clone_workspace_with_target workspace ~target_dir:Path.(tempdir / Path.v "target")
          in
          let events = ref [] in
          match Riot_build.build
            ~on_event:(fun event -> events := event :: !events)
            (Riot_build.Request.make
              ~workspace
              ~packages:[ package_name "kernel" ]
              ~targets:Riot_model.Target.Host
              ~scope:Riot_build.Request.Runtime
              ~profile:Riot_model.Profile.debug
              ()) with
          | Error err -> Error (summarize_build_failure err !events)
          | Ok output -> (
              match Riot_build.Build_result.find_package output (package_name "kernel") with
              | None -> Error "expected kernel build output"
              | Some result -> (
                  match Riot_build.Build_result.package_status result with
                  | Riot_build.Build_result.Built _
                  | Riot_build.Build_result.Cached _ ->
                      Result.map_err
                        (expect_public_phase_subsequence !events)
                        ~fn:(fun err -> err ^ "\nrecent events: " ^ summarize_recent_events !events)
                  | Riot_build.Build_result.Skipped reason ->
                      Error ("expected kernel build to run, got skipped: " ^ reason)
                  | Riot_build.Build_result.Failed message ->
                      Error ("kernel build failed: " ^ message)
                )
            )) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let tests = let open Test in
[
  case
    ~size:Large
    "build runtime: repo kernel builds successfully"
    test_build_runtime_builds_repo_kernel;
]

let name = "Riot Build Runtime Kernel Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
