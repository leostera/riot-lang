open Std

type t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Streaming of Client.streaming_event

let telemetry_event_to_json = fun event ->
  match Riot_executor.Telemetry_events.to_json event with
  | Some json -> Some json
  | None -> None

let build_stats_to_json = fun (stats: Client.build_stats) ->
  Data.Json.Object [
    ("duration_ms", Data.Json.Int stats.duration_ms);
    ("packages_built", Data.Json.Int stats.packages_built);
    ("packages_failed", Data.Json.Int stats.packages_failed);
    ("total_modules", Data.Json.Int stats.total_modules);
    ("cache_hits", Data.Json.Int stats.cache_hits);
    ("cache_misses", Data.Json.Int stats.cache_misses);
  ]

let package_results_to_json = fun (results: Riot_executor.Package_builder.build_result list) ->
  Data.Json.Array (List.map Riot_executor.Package_builder.build_result_to_json results)

let build_failed_event_to_json = fun session_id failed_at errors ->
  Data.Json.Object [
    ("type", Data.Json.String "BuildFailed");
    ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
    ("failed_at", Data.Json.String (Datetime.to_iso8601 failed_at));
    ("errors", package_results_to_json errors);
  ]

let build_completed_event_to_json = fun session_id completed_at stats results ->
  Data.Json.Object [
    ("type", Data.Json.String "BuildCompleted");
    ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
    ("completed_at", Data.Json.String (Datetime.to_iso8601 completed_at));
    ("stats", build_stats_to_json stats);
    ("results", package_results_to_json results);
  ]

let planning_failed_event_to_json = fun session_id failed_at reason ->
  Data.Json.Object [
    ("type", Data.Json.String "PlanningFailed");
    ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
    ("failed_at", Data.Json.String (Datetime.to_iso8601 failed_at));
    ("reason", Data.Json.String reason);
  ]

let cycle_detected_event_to_json = fun session_id detected_at cycle_nodes ->
  Data.Json.Object [
    ("type", Data.Json.String "CycleDetected");
    ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
    ("detected_at", Data.Json.String (Datetime.to_iso8601 detected_at));
    ("cycle_nodes", Data.Json.Array (List.map Data.Json.string cycle_nodes));
  ]

let to_json = function
  | Pm event ->
      Some (Riot_model.Event.to_json event)
  | BuildingTarget { target; host } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "BuildingTarget");
        ("target", Data.Json.String target);
        ("host", Data.Json.Bool host);
      ])
  | CacheGc event ->
      Some (Riot_store.Cache_gc.event_to_json event)
  | Streaming event -> (
      match event with
      | Client.BuildStarted _ -> None
      | Client.BuildEvent event -> telemetry_event_to_json event
      | Client.BuildCompleted { session_id; completed_at; stats; results } -> Some (build_completed_event_to_json
        session_id
        completed_at
        stats
        results)
      | Client.BuildFailed {
        session_id;
        failed_at;
        stats=_;
        built=_;
        errors
      } -> Some (build_failed_event_to_json session_id failed_at errors)
      | Client.PlanningFailed { session_id; failed_at; reason } -> Some (planning_failed_event_to_json
        session_id
        failed_at
        reason)
      | Client.CycleDetected { session_id; detected_at; cycle_nodes } -> Some (cycle_detected_event_to_json
        session_id
        detected_at
        cycle_nodes)
    )
