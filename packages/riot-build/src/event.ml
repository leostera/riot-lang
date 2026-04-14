open Std

type t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: Riot_model.Target.t; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Phase of runtime_phase

and runtime_phase =
  | TargetsResolved of { target_count: int }
  | ToolchainsEnsured of { target_count: int }
  | ToolchainsValidated of { target_count: int }
  | ClientConnecting
  | ClientConnected
  | TargetBuildStarted of { target: Riot_model.Target.t; host: bool }
  | TargetBuildFinished of { target: Riot_model.Target.t; result_count: int; had_partial_failure: bool }
  | CacheGenerationRecordingStarted of { lane_count: int; new_entry_count: int }
  | CacheGenerationRecorded of { lane_count: int; new_entry_count: int }
  | ReturningResults of { result_count: int; had_partial_failure: bool }

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
      [
        ("target", Data.Json.String (Riot_model.Target.to_string target));
        ("host", Data.Json.Bool host);
      ]
  | TargetBuildFinished { target; result_count; had_partial_failure } ->
      [
        ("target", Data.Json.String (Riot_model.Target.to_string target));
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

let to_json = function
  | Pm event ->
      Some (Riot_model.Event.to_json event)
  | BuildingTarget { target; host } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "BuildingTarget");
        ("target", Data.Json.String (Riot_model.Target.to_string target));
        ("host", Data.Json.Bool host);
      ])
  | CacheGc event ->
      Some (Riot_store.Cache_gc.event_to_json event)
  | Phase phase ->
      Some (Data.Json.Object
        ([
           ("type", Data.Json.String "BuildPhase");
           ("phase", Data.Json.String (phase_name_of_runtime_phase phase));
         ]
        @ runtime_phase_fields phase))
