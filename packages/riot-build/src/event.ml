open Std

type t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Phase of phase
  | Streaming of Client.streaming_event

and runtime_phase =
  | TargetsResolved of { target_count: int }
  | ToolchainsEnsured of { target_count: int }
  | ToolchainsValidated of { target_count: int }
  | ClientConnecting
  | ClientConnected
  | TargetBuildStarted of { target: string; host: bool }
  | TargetBuildFinished of { target: string; result_count: int; had_partial_failure: bool }
  | CacheGenerationRecordingStarted of { lane_count: int; new_entry_count: int }
  | CacheGenerationRecorded of { lane_count: int; new_entry_count: int }
  | ReturningResults of { result_count: int; had_partial_failure: bool }

and cli_phase =
  | JsonTerminalEventEncodingStarted of { event: string; result_count: int option }
  | JsonTerminalEventEncoded of { event: string; result_count: int option }

and phase =
  | RuntimePhase of runtime_phase
  | CliPhase of cli_phase

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
  Data.Json.Array (List.map results ~fn:Riot_executor.Package_builder.build_result_to_json)

let build_failed_event_to_json = fun session_id failed_at errors ->
  Data.Json.Object [
    ("type", Data.Json.String "BuildFailed");
    ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
    ("failed_at", Data.Json.String (DateTime.to_iso8601 failed_at));
    ("errors", package_results_to_json errors);
  ]

let build_completed_event_to_json = fun session_id completed_at stats results ->
  Data.Json.Object [
    ("type", Data.Json.String "BuildCompleted");
    ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
    ("completed_at", Data.Json.String (DateTime.to_iso8601 completed_at));
    ("stats", build_stats_to_json stats);
    ("results", package_results_to_json results);
  ]

let planning_failed_event_to_json = fun session_id failed_at reason ->
  Data.Json.Object [
    ("type", Data.Json.String "PlanningFailed");
    ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
    ("failed_at", Data.Json.String (DateTime.to_iso8601 failed_at));
    ("reason", Data.Json.String reason);
  ]

let cycle_detected_event_to_json = fun session_id detected_at cycle_nodes ->
  Data.Json.Object [
    ("type", Data.Json.String "CycleDetected");
    ("session_id", Data.Json.String (Riot_model.Session_id.to_string session_id));
    ("detected_at", Data.Json.String (DateTime.to_iso8601 detected_at));
    ("cycle_nodes", Data.Json.Array (List.map cycle_nodes ~fn:Data.Json.string));
  ]

let phase_name_of_runtime_phase = function
  | TargetsResolved _ -> "targets_resolved"
  | ToolchainsEnsured _ -> "toolchains_ensured"
  | ToolchainsValidated _ -> "toolchains_validated"
  | ClientConnecting -> "client_connecting"
  | ClientConnected -> "client_connected"
  | TargetBuildStarted _ -> "target_build_started"
  | TargetBuildFinished _ -> "target_build_finished"
  | CacheGenerationRecordingStarted _ -> "cache_generation_recording_started"
  | CacheGenerationRecorded _ -> "cache_generation_recorded"
  | ReturningResults _ -> "returning_results"

let runtime_phase_fields = function
  | TargetsResolved { target_count }
  | ToolchainsEnsured { target_count }
  | ToolchainsValidated { target_count } ->
      [ ("target_count", Data.Json.Int target_count) ]
  | ClientConnecting
  | ClientConnected -> []
  | TargetBuildStarted { target; host } ->
      [ ("target", Data.Json.String target); ("host", Data.Json.Bool host) ]
  | TargetBuildFinished { target; result_count; had_partial_failure } ->
      [
        ("target", Data.Json.String target);
        ("result_count", Data.Json.Int result_count);
        ("had_partial_failure", Data.Json.Bool had_partial_failure);
      ]
  | CacheGenerationRecordingStarted { lane_count; new_entry_count }
  | CacheGenerationRecorded { lane_count; new_entry_count } ->
      [ ("lane_count", Data.Json.Int lane_count); ("new_entry_count", Data.Json.Int new_entry_count) ]
  | ReturningResults { result_count; had_partial_failure } ->
      [
        ("result_count", Data.Json.Int result_count);
        ("had_partial_failure", Data.Json.Bool had_partial_failure);
      ]

let phase_name_of_cli_phase = function
  | JsonTerminalEventEncodingStarted _ -> "json_terminal_event_encoding_started"
  | JsonTerminalEventEncoded _ -> "json_terminal_event_encoded"

let cli_phase_fields = function
  | JsonTerminalEventEncodingStarted { event; result_count }
  | JsonTerminalEventEncoded { event; result_count } ->
      let result_count_json =
        match result_count with
        | Some count -> Data.Json.Int count
        | None -> Data.Json.Null
      in
      [
        ("event", Data.Json.String event);
        ("result_count", result_count_json);
      ]

let phase_event_to_json = function
  | RuntimePhase phase ->
      Data.Json.Object
        ([
           ("type", Data.Json.String "BuildPhase");
           ("subsystem", Data.Json.String "runtime");
           ("phase", Data.Json.String (phase_name_of_runtime_phase phase));
         ]
        @ runtime_phase_fields phase)
  | CliPhase phase ->
      Data.Json.Object
        ([
           ("type", Data.Json.String "BuildPhase");
           ("subsystem", Data.Json.String "cli");
           ("phase", Data.Json.String (phase_name_of_cli_phase phase));
         ]
        @ cli_phase_fields phase)

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
  | Phase phase ->
      Some (phase_event_to_json phase)
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
