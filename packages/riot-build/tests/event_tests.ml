open Std
module Test = Std.Test

let test_building_target_event_to_json = fun _ctx ->
  let actual = Riot_build.Event.to_json
    (Riot_build.BuildingTarget { target = "aarch64-linux"; host = false }) in
  Test.assert_equal
    ~expected:(Some (Data.Json.Object [
      ("type", Data.Json.String "BuildingTarget");
      ("target", Data.Json.String "aarch64-linux");
      ("host", Data.Json.Bool false);
    ]))
    ~actual;
  Ok ()

let test_pm_event_to_json_reuses_riot_model_event_shape = fun _ctx ->
  let session_id = Riot_model.Session_id.make () in
  let event = Riot_model.Event.create
    ~session_id
    ~level:Riot_model.Event.Info (Riot_model.Event.PackageDownloadStarted {
      package = "std";
      version = "0.1.0";
      path = "/tmp/std"
    }) in
  match Riot_build.Event.to_json (Riot_build.Pm event) with
  | Some (Data.Json.Object fields) -> (
      match
        List.find fields ~fn:(fun (name, _) -> String.equal name "event")
        |> Option.map ~fn:(fun (_, value) -> value)
      with
      | Some (Data.Json.String "riot.pm.package_download.started") -> Ok ()
      | Some json -> Error ("expected PM event name in JSON, got " ^ Data.Json.to_string json)
      | None -> Error "expected PM event name in JSON"
    )
  | Some json ->
      Error ("expected JSON object, got " ^ Data.Json.to_string json)
  | None ->
      Error "expected JSON output for PM event"

let test_build_phase_event_to_json = fun _ctx ->
  let actual = Riot_build.Event.to_json
    (Riot_build.Phase
      (Riot_build.Event.RuntimePhase
        (Riot_build.Event.TargetsResolved { target_count = 3 }))) in
  Test.assert_equal
    ~expected:(Some (Data.Json.Object [
      ("type", Data.Json.String "BuildPhase");
      ("subsystem", Data.Json.String "runtime");
      ("phase", Data.Json.String "targets_resolved");
      ("target_count", Data.Json.Int 3);
    ]))
    ~actual;
  Ok ()

let test_workspace_plan_completed_telemetry_event_to_json = fun _ctx ->
  let session_id = Riot_model.Session_id.make () in
  let duration = Time.Duration.from_millis 52 in
  let actual =
    Riot_build.Event.to_json
      (Riot_build.Streaming
        (Riot_build.Client.BuildEvent
          (Riot_executor.Telemetry_events.WorkspacePlanCompleted {
            session_id;
            target = Riot_planner.Workspace_planner.Package "riot-cli";
            workspace_package_count = 64;
            planned_package_count = 31;
            duration;
          })))
  in
  Test.assert_equal
    ~expected:(Some (Data.Json.Object [
      ("type", Data.Json.String "WorkspacePlanCompleted");
      ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
      ("target", Data.Json.String "riot-cli");
      ("workspace_package_count", Data.Json.Int 64);
      ("planned_package_count", Data.Json.Int 31);
      ("duration_ms", Data.Json.Int 52);
    ]))
    ~actual;
  Ok ()

let test_workspace_graph_created_telemetry_event_to_json = fun _ctx ->
  let session_id = Riot_model.Session_id.make () in
  let duration = Time.Duration.from_millis 47 in
  let actual =
    Riot_build.Event.to_json
      (Riot_build.Streaming
        (Riot_build.Client.BuildEvent
          (Riot_executor.Telemetry_events.WorkspaceGraphCreated {
            session_id;
            target = Riot_planner.Workspace_planner.Package "riot-cli";
            node_count = 31;
            breakdown = {
              Riot_executor.Telemetry_events.build_node_realization_count = 2;
              build_node_realization_duration = Time.Duration.from_millis 3;
              runtime_node_realization_count = 31;
              runtime_node_realization_duration = Time.Duration.from_millis 41;
              dev_node_realization_count = 0;
              dev_node_realization_duration = Time.Duration.from_millis 0;
              edge_wiring_duration = Time.Duration.from_millis 3;
            };
            duration;
          })))
  in
  Test.assert_equal
    ~expected:(Some (Data.Json.Object [
      ("type", Data.Json.String "WorkspaceGraphCreated");
      ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
      ("target", Data.Json.String "riot-cli");
      ("node_count", Data.Json.Int 31);
      ("breakdown", Data.Json.Object [
        ("build_node_realization_count", Data.Json.Int 2);
        ("build_node_realization_duration_ms", Data.Json.Int 3);
        ("runtime_node_realization_count", Data.Json.Int 31);
        ("runtime_node_realization_duration_ms", Data.Json.Int 41);
        ("dev_node_realization_count", Data.Json.Int 0);
        ("dev_node_realization_duration_ms", Data.Json.Int 0);
        ("edge_wiring_duration_ms", Data.Json.Int 3);
      ]);
      ("duration_ms", Data.Json.Int 47);
    ]))
    ~actual;
  Ok ()

let test_package_planning_breakdown_telemetry_event_to_json = fun _ctx ->
  let session_id = Riot_model.Session_id.make () in
  let actual =
    Riot_build.Event.to_json
      (Riot_build.Streaming
        (Riot_build.Client.BuildEvent
          (Riot_executor.Telemetry_events.PackagePlanningBreakdown {
            session_id;
            package = Riot_model.Package.synthetic
              ~name:"std"
              ~path:(Path.v "packages/std")
              ~relative_path:(Path.v "packages/std");
            target = Riot_planner.Workspace_planner.Package "riot-cli";
            breakdown = {
              Riot_executor.Telemetry_events.dependency_count = 1;
              dependency_check_duration = Time.Duration.from_millis 2;
              input_hash_duration = Time.Duration.from_millis 19;
              artifact_lookup_duration = Time.Duration.from_millis 3;
              artifact_cache_hit = true;
              plan_bundle_lookup_duration = Time.Duration.from_millis 0;
              plan_bundle_decode_duration = Time.Duration.from_millis 0;
              plan_bundle_cache_hit = false;
              module_plan_duration = Time.Duration.from_millis 0;
            };
          })))
  in
  Test.assert_equal
    ~expected:(Some (Data.Json.Object [
      ("type", Data.Json.String "PackagePlanningBreakdown");
      ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
      ("package", Riot_model.Package.to_json (Riot_model.Package.synthetic
        ~name:"std"
        ~path:(Path.v "packages/std")
        ~relative_path:(Path.v "packages/std")));
      ("target", Data.Json.String "riot-cli");
      ("breakdown", Data.Json.Object [
        ("dependency_count", Data.Json.Int 1);
        ("dependency_check_duration_ms", Data.Json.Int 2);
        ("input_hash_duration_ms", Data.Json.Int 19);
        ("artifact_lookup_duration_ms", Data.Json.Int 3);
        ("artifact_cache_hit", Data.Json.Bool true);
        ("plan_bundle_lookup_duration_ms", Data.Json.Int 0);
        ("plan_bundle_decode_duration_ms", Data.Json.Int 0);
        ("plan_bundle_cache_hit", Data.Json.Bool false);
        ("module_plan_duration_ms", Data.Json.Int 0);
      ]);
    ]))
    ~actual;
  Ok ()

let tests =
  let open Test in [
    case "event: building target json" test_building_target_event_to_json;
    case "event: pm events reuse riot-model json" test_pm_event_to_json_reuses_riot_model_event_shape;
    case "event: build phase json" test_build_phase_event_to_json;
    case "event: workspace plan telemetry json" test_workspace_plan_completed_telemetry_event_to_json;
    case "event: workspace graph telemetry json" test_workspace_graph_created_telemetry_event_to_json;
    case "event: package planning breakdown telemetry json" test_package_planning_breakdown_telemetry_event_to_json;
  ]

let name = "Riot Build Event Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
