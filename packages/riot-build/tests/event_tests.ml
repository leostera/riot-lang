open Std
module Test = Std.Test

let package_name = fun name ->
  Result.expect (Riot_model.Package_name.from_string name) ~msg:("package name " ^ name)

let test_building_target_event_to_json = fun _ctx ->
  let target = Result.expect (Riot_model.Target.from_string "aarch64-unknown-linux-gnu") ~msg:"target" in
  let actual = Riot_build.Event.to_json (Riot_build.Event.BuildingTarget { target; host = false }) in
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
  let event = Riot_model.Event.create
    ~session_id
    ~level:Riot_model.Event.Info (Riot_model.Event.PackageDownloadStarted {
      package = package_name "std";
      version = "0.1.0";
      path = "/tmp/std"
    }) in
  match Riot_build.Event.to_json (Riot_build.Event.Pm event) with
  | Some (Data.Json.Object fields) -> (
      match
        List.find fields
          ~fn:(fun (name, _) ->
            String.equal name "event") |> Option.map ~fn:(fun (_, value) -> value)
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
    (Riot_build.Event.Phase (Riot_build.Event.TargetsResolved { target_count = 3 })) in
  Test.assert_equal
    ~expected:(Some (Data.Json.Object [
      ("type", Data.Json.String "BuildPhase");
      ("phase", Data.Json.String "targets_resolved");
      ("target_count", Data.Json.Int 3);
    ]))
    ~actual;
  Ok ()

let test_package_planning_phase_event_to_json = fun _ctx ->
  let actual = Riot_build.Event.to_json
    (Riot_build.Event.Phase (Riot_build.Event.PackagePlanningFinished {
      lane_count = 2;
      package_count = 5;
      deferred_count = 1;
      execution_required_count = 2;
      finalized_count = 2;
      cached_count = 1;
      skipped_count = 1;
      failed_count = 0;
      error_count = 0;
    })) in
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

let tests =
  let open Test in [
    case "event: building target json" test_building_target_event_to_json;
    case "event: pm events reuse riot-model json" test_pm_event_to_json_reuses_riot_model_event_shape;
    case "event: build phase json" test_build_phase_event_to_json;
    case "event: package planning phase json" test_package_planning_phase_event_to_json;
  ]

let name = "Riot Build Event Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
