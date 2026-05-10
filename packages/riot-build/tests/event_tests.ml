open Std

module Test = Std.Test

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

let event = fun kind ->
  Riot_model.Event.create
    ~session_id:(Riot_model.Session_id.make ())
    ~level:Riot_model.Event.Info
    kind

let json_field = fun name json -> Data.Json.get_field name json

let data_field = fun name json ->
  match json_field "data" json with
  | Some data -> Data.Json.get_field name data
  | None -> None

let expect_json_field = fun ~label ~expected ~actual ->
  if expected = actual then
    Ok ()
  else
    Error (label
    ^ ": expected "
    ^ Data.Json.to_string expected
    ^ ", got "
    ^ Data.Json.to_string actual)

let expect_some_json_field = fun ~label ~expected actual ->
  match actual with
  | Some actual -> expect_json_field ~label ~expected ~actual
  | None -> Error (label ^ ": expected field")

let test_building_target_event_to_json = fun _ctx ->
  let target =
    Result.expect (Riot_model.Target.from_string "aarch64-unknown-linux-gnu") ~msg:"target"
  in
  match Riot_build.Event.to_json
    (event (Riot_model.Event.Build (Riot_model.Event.BuildTargetBuilding { target; host = false }))) with
  | Some json ->
      let open Std.Result.Syntax in
      let* () =
        expect_some_json_field
          ~label:"event"
          ~expected:(Data.Json.String "riot.build.target.building")
          (json_field "event" json)
      in
      let* () =
        expect_some_json_field
          ~label:"data.target"
          ~expected:(Data.Json.String "aarch64-unknown-linux-gnu")
          (data_field "target" json)
      in
      expect_some_json_field
        ~label:"data.host"
        ~expected:(Data.Json.Bool false)
        (data_field "host" json)
  | None -> Error "expected JSON output"

let test_deps_event_to_json_uses_model_envelope = fun _ctx ->
  let deps_event = Riot_model.Event.Deps (Riot_model.Event.DepsPackageDownloadStarted {
    package = package_name "std";
    version = "0.1.0";
    path = "/tmp/std";
  })
  in
  match Riot_build.Event.to_json (event deps_event) with
  | Some json ->
      expect_some_json_field
        ~label:"event"
        ~expected:(Data.Json.String "riot.deps.package.download.started")
        (json_field "event" json)
  | None -> Error "expected JSON output for deps event"

let test_build_phase_event_to_json = fun _ctx ->
  let session_id = Riot_model.Session_id.make () in
  match Riot_build.Event.to_json
    (Riot_build.Event.phase ~session_id (Riot_build.Event.TargetsResolved { target_count = 3 })) with
  | Some json ->
      let open Std.Result.Syntax in
      let* () =
        expect_some_json_field
          ~label:"event"
          ~expected:(Data.Json.String "riot.build.phase.targets.resolved")
          (json_field "event" json)
      in
      let* () =
        expect_some_json_field
          ~label:"data.phase"
          ~expected:(Data.Json.String "targets_resolved")
          (data_field "phase" json)
      in
      expect_some_json_field
        ~label:"data.target_count"
        ~expected:(Data.Json.Int 3)
        (data_field "target_count" json)
  | None -> Error "expected JSON output"

let test_package_planning_phase_event_to_json = fun _ctx ->
  let session_id = Riot_model.Session_id.make () in
  match Riot_build.Event.to_json
    (
      Riot_build.Event.phase
        ~session_id
        (
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
    ) with
  | Some json ->
      let open Std.Result.Syntax in
      let* () =
        expect_some_json_field
          ~label:"event"
          ~expected:(Data.Json.String "riot.build.phase.package.planning.finished")
          (json_field "event" json)
      in
      let* () =
        expect_some_json_field
          ~label:"data.package_count"
          ~expected:(Data.Json.Int 5)
          (data_field "package_count" json)
      in
      expect_some_json_field
        ~label:"data.error_count"
        ~expected:(Data.Json.Int 0)
        (data_field "error_count" json)
  | None -> Error "expected JSON output"

let test_package_action_graph_planned_event_to_json = fun _ctx ->
  let target =
    Result.expect (Riot_model.Target.from_string "aarch64-unknown-linux-gnu") ~msg:"target"
  in
  match Riot_build.Event.to_json
    (
      Riot_build.Event.phase
        ~session_id:(Riot_model.Session_id.make ())
        (
          Riot_build.Event.PackageActionGraphPlanned {
            package = make_demo_package ();
            build_target = target;
            action_count = 42;
            planned_at = Time.Instant.now ();
          }
        )
    ) with
  | Some json ->
      let open Std.Result.Syntax in
      let* () =
        expect_some_json_field
          ~label:"event"
          ~expected:(Data.Json.String "riot.build.phase.package.action_graph.planned")
          (json_field "event" json)
      in
      let* () =
        expect_some_json_field
          ~label:"data.target"
          ~expected:(Data.Json.String "aarch64-unknown-linux-gnu")
          (data_field "target" json)
      in
      expect_some_json_field
        ~label:"data.action_count"
        ~expected:(Data.Json.Int 42)
        (data_field "action_count" json)
  | None -> Error "expected JSON output"

let test_package_execution_prepared_event_to_json = fun _ctx ->
  let package = make_demo_package () in
  let build_target =
    Result.expect (Riot_model.Target.from_string "aarch64-apple-darwin") ~msg:"target"
  in
  let prepared = Riot_model.Event.BuildPackageExecutionPrepared {
    package;
    build_target;
    input_count = 12;
    dependency_count = 4;
    dependency_object_count = 3;
    prepared_at = Time.Instant.now ();
    duration = Time.Duration.from_millis 37;
  }
  in
  match Riot_build.Event.to_json (event (Riot_model.Event.Build prepared)) with
  | Some json ->
      let open Std.Result.Syntax in
      let* () =
        expect_some_json_field
          ~label:"event"
          ~expected:(Data.Json.String "riot.build.package.execution.prepared")
          (json_field "event" json)
      in
      let* () =
        expect_some_json_field
          ~label:"data.input_count"
          ~expected:(Data.Json.Int 12)
          (data_field "input_count" json)
      in
      let* () =
        expect_some_json_field
          ~label:"data.dependency_count"
          ~expected:(Data.Json.Int 4)
          (data_field "dependency_count" json)
      in
      let* () =
        expect_some_json_field
          ~label:"data.dependency_object_count"
          ~expected:(Data.Json.Int 3)
          (data_field "dependency_object_count" json)
      in
      expect_some_json_field
        ~label:"data.duration_ms"
        ~expected:(Data.Json.Int 37)
        (data_field "duration_ms" json)
  | None -> Error "expected package preparation event JSON object"

let tests = let open Test in
[
  case "event: building target json" test_building_target_event_to_json;
  case "event: deps events reuse riot-model json" test_deps_event_to_json_uses_model_envelope;
  case "event: build phase json" test_build_phase_event_to_json;
  case "event: package planning phase json" test_package_planning_phase_event_to_json;
  case "event: package action graph planned json" test_package_action_graph_planned_event_to_json;
  case "event: package execution prepared json" test_package_execution_prepared_event_to_json;
]

let name = "Riot Build Event Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
