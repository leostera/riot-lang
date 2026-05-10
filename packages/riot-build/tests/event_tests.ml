open Std

module Test = Std.Test
module Telemetry_events = Riot_build.Internal.Telemetry_events

let package_name = fun name ->
  Result.expect
    (Riot_model.Package_name.from_string name)
    ~msg:("package name " ^ name)

let make_demo_package = fun () ->
  Riot_model.Package.make
    ~name:(package_name "demo")
    ~path:(Path.v "/tmp/demo")
    ~relative_path:(Path.v "packages/demo")
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let expect_timestamp_field = fun ~expected event ->
  match Riot_build.Event.timestamp (Riot_build.Event.Telemetry event) with
  | Some (actual, _) ->
      Test.assert_equal ~expected ~actual;
      Ok ()
  | None -> Error ("expected timestamp field " ^ expected)

let test_building_target_event_to_json = fun _ctx ->
  let target =
    Result.expect (Riot_model.Target.from_string "aarch64-unknown-linux-gnu") ~msg:"target"
  in
  let actual =
    Riot_build.Event.to_json (Riot_build.Event.BuildingTarget { target; host = false })
  in
  Test.assert_equal
    ~expected:(Some (Data.Json.Object [
      ("type", Data.Json.String "BuildingTarget");
      ("target", Data.Json.String "aarch64-unknown-linux-gnu");
      ("host", Data.Json.Bool false);
    ]))
    ~actual;
  Ok ()

let test_pm_event_to_json_reuses_riot_model_event_shape = fun _ctx ->
  let session_id = Riot_model.Session_id.make () in
  let event =
    Riot_model.Event.create
      ~session_id
      ~level:Riot_model.Event.Info
      (Riot_model.Event.PackageDownloadStarted {
        package = package_name "std";
        version = "0.1.0";
        path = "/tmp/std";
      })
  in
  match Riot_build.Event.to_json (Riot_build.Event.Pm event) with
  | Some (Data.Json.Object fields) ->
      (match List.find fields ~fn:(fun (name, _) -> String.equal name "event")
      |> Option.map ~fn:(fun (_, value) -> value) with
      | Some (Data.Json.String "riot.pm.package_download.started") -> Ok ()
      | Some json -> Error ("expected PM event name in JSON, got " ^ Data.Json.to_string json)
      | None -> Error "expected PM event name in JSON")
  | Some json -> Error ("expected JSON object, got " ^ Data.Json.to_string json)
  | None -> Error "expected JSON output for PM event"

let test_build_phase_event_to_json = fun _ctx ->
  let actual =
    Riot_build.Event.to_json
      (Riot_build.Event.Phase (Riot_build.Event.TargetsResolved { target_count = 3 }))
  in
  Test.assert_equal
    ~expected:(Some (Data.Json.Object [
      ("type", Data.Json.String "BuildPhase");
      ("phase", Data.Json.String "targets_resolved");
      ("target_count", Data.Json.Int 3);
    ]))
    ~actual;
  Ok ()

let test_package_planning_phase_event_to_json = fun _ctx ->
  let actual =
    Riot_build.Event.to_json
      (
        Riot_build.Event.Phase (
          Riot_build.Event.PackagePlanningFinished {
            lane_count = 2;
            package_count = 5;
            deferred_count = 1;
            execution_required_count = 2;
            finalized_count = 2;
            cached_count = 1;
            skipped_count = 1;
            failed_count = 0;
            error_count = 0;
          }
        )
      )
  in
  Test.assert_equal
    ~expected:(Some (Data.Json.Object [
      ("type", Data.Json.String "BuildPhase");
      ("phase", Data.Json.String "package_planning_finished");
      ("lane_count", Data.Json.Int 2);
      ("package_count", Data.Json.Int 5);
      ("deferred_count", Data.Json.Int 1);
      ("execution_required_count", Data.Json.Int 2);
      ("finalized_count", Data.Json.Int 2);
      ("cached_count", Data.Json.Int 1);
      ("skipped_count", Data.Json.Int 1);
      ("failed_count", Data.Json.Int 0);
      ("error_count", Data.Json.Int 0);
    ]))
    ~actual;
  Ok ()

let test_package_action_graph_planned_event_to_json = fun _ctx ->
  let target =
    Result.expect (Riot_model.Target.from_string "aarch64-unknown-linux-gnu") ~msg:"target"
  in
  let actual =
    Riot_build.Event.to_json
      (
        Riot_build.Event.Phase (
          Riot_build.Event.PackageActionGraphPlanned {
            package = make_demo_package ();
            build_target = target;
            action_count = 42;
            planned_at = Time.Instant.now ();
          }
        )
      )
  in
  Test.assert_equal
    ~expected:(Some (Data.Json.Object [
      ("type", Data.Json.String "BuildPhase");
      ("phase", Data.Json.String "package_action_graph_planned");
      ("package", Data.Json.String "demo");
      ("target", Data.Json.String "aarch64-unknown-linux-gnu");
      ("action_count", Data.Json.Int 42);
    ]))
    ~actual;
  Ok ()

let test_telemetry_event_to_json = fun _ctx ->
  let session_id = Riot_model.Session_id.make () in
  let package = make_demo_package () in
  let telemetry_event = Telemetry_events.PackageStarted {
    session_id;
    package;
    target = Telemetry_events.Package package.name;
    started_at = Time.Instant.now ();
  }
  in
  let actual = Riot_build.Event.to_json (Riot_build.Event.Telemetry telemetry_event) in
  let expected = Telemetry_events.to_json telemetry_event in
  Test.assert_equal ~expected ~actual;
  let actual_type =
    match actual with
    | Some json -> Data.Json.get_field "type" json
    | None -> None
  in
  Test.assert_equal ~expected:(Some (Data.Json.String "PackageStarted")) ~actual:actual_type;
  Ok ()

let test_telemetry_timestamp_fields_describe_event_instants = fun _ctx ->
  let session_id = Riot_model.Session_id.make () in
  let package = make_demo_package () in
  let target = Telemetry_events.Package package.name in
  let build_target =
    Result.expect (Riot_model.Target.from_string "aarch64-apple-darwin") ~msg:"target"
  in
  let now = Time.Instant.now () in
  let events = [
    (
      "started_at_us",
      Telemetry_events.PackageStarted {
        session_id;
        package;
        target;
        started_at = now;
      }
    );
    (
      "started_at_us",
      Telemetry_events.CompilationStarted {
        session_id;
        package;
        target;
        build_target;
        action_count = 4;
        started_at = now;
      }
    );
    (
      "created_at_us",
      Telemetry_events.SandboxCreated {
        session_id;
        package;
        target;
        build_target;
        path = Path.v "/tmp/demo-sandbox";
        created_at = now;
        duration = Time.Duration.zero;
      }
    );
    (
      "copied_at_us",
      Telemetry_events.SandboxInputsCopied {
        session_id;
        package;
        target;
        build_target;
        input_count = 2;
        copied_at = now;
        duration = Time.Duration.zero;
      }
    );
    (
      "copied_at_us",
      Telemetry_events.SandboxDependenciesCopied {
        session_id;
        package;
        target;
        build_target;
        dependency_count = 1;
        object_count = 3;
        copied_at = now;
        duration = Time.Duration.zero;
      }
    );
    (
      "prepared_at_us",
      Telemetry_events.PackageExecutionPrepared {
        session_id;
        package;
        target;
        build_target;
        input_count = 2;
        dependency_count = 1;
        dependency_object_count = 3;
        prepared_at = now;
        duration = Time.Duration.zero;
      }
    );
  ]
  in
  let rec check = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | (expected, event) :: rest ->
        match expect_timestamp_field ~expected event with
        | Ok () -> check rest
        | Error err -> Error err
  in
  check events

let test_package_execution_prepared_event_round_trips = fun _ctx ->
  let session_id = Riot_model.Session_id.make () in
  let package = make_demo_package () in
  let build_target =
    Result.expect (Riot_model.Target.from_string "aarch64-apple-darwin") ~msg:"target"
  in
  let event = Telemetry_events.PackageExecutionPrepared {
    session_id;
    package;
    target = Telemetry_events.Package package.name;
    build_target;
    input_count = 12;
    dependency_count = 4;
    dependency_object_count = 3;
    prepared_at = Time.Instant.now ();
    duration = Time.Duration.from_millis 37;
  }
  in
  match Telemetry_events.to_json event with
  | Some (Data.Json.Object fields as json) ->
      Test.assert_equal
        ~expected:(Some (Data.Json.String "PackageExecutionPrepared"))
        ~actual:(Data.Json.get_field "type" (Data.Json.Object fields));
      Test.assert_equal
        ~expected:(Some (Data.Json.Int 12))
        ~actual:(Data.Json.get_field "input_count" (Data.Json.Object fields));
      Test.assert_equal
        ~expected:(Some (Data.Json.Int 4))
        ~actual:(Data.Json.get_field "dependency_count" (Data.Json.Object fields));
      Test.assert_equal
        ~expected:(Some (Data.Json.Int 3))
        ~actual:(Data.Json.get_field "dependency_object_count" (Data.Json.Object fields));
      Test.assert_equal
        ~expected:(Some (Data.Json.Int 37))
        ~actual:(Data.Json.get_field "duration_ms" (Data.Json.Object fields));
      (match Telemetry_events.from_json json with
      | Ok (Telemetry_events.PackageExecutionPrepared parsed) ->
          Test.assert_equal ~expected:12 ~actual:parsed.input_count;
          Test.assert_equal ~expected:4 ~actual:parsed.dependency_count;
          Test.assert_equal ~expected:3 ~actual:parsed.dependency_object_count;
          Test.assert_equal ~expected:37 ~actual:(Time.Duration.to_millis parsed.duration);
          Ok ()
      | Ok _ -> Error "expected PackageExecutionPrepared event"
      | Error err ->
          Error ("expected PackageExecutionPrepared event to decode: " ^ Data.Json.to_string err))
  | Some _ -> Error "expected package preparation event JSON object"
  | None -> Error "expected package preparation event to render JSON"

let tests = let open Test in
[
  case "event: building target json" test_building_target_event_to_json;
  case "event: pm events reuse riot-model json" test_pm_event_to_json_reuses_riot_model_event_shape;
  case "event: build phase json" test_build_phase_event_to_json;
  case "event: package planning phase json" test_package_planning_phase_event_to_json;
  case "event: package action graph planned json" test_package_action_graph_planned_event_to_json;
  case "event: telemetry json" test_telemetry_event_to_json;
  case
    "event: telemetry timestamps describe event instants"
    test_telemetry_timestamp_fields_describe_event_instants;
  case
    "event: package execution prepared round trips"
    test_package_execution_prepared_event_round_trips;
]

let name = "Riot Build Event Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
