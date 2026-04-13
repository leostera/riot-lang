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

let tests =
  let open Test in [
    case "event: building target json" test_building_target_event_to_json;
    case "event: pm events reuse riot-model json" test_pm_event_to_json_reuses_riot_model_event_shape;
  ]

let name = "Riot Build Event Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
