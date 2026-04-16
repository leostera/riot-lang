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
  | RuntimeStarting
  | RuntimeStarted
  | PackagePlanningStarted of { lane_count: int; package_count: int }
  | PackagePlanningFinished of {
      lane_count: int;
      package_count: int;
      deferred_count: int;
      execution_required_count: int;
      finalized_count: int;
      cached_count: int;
      skipped_count: int;
      failed_count: int;
      error_count: int;
    }
  | PackageExecutionStarted of { lane_count: int; package_count: int }
  | PackageExecutionFinished of {
      lane_count: int;
      package_count: int;
      finalized_count: int;
      built_count: int;
      failed_count: int;
      error_count: int;
    }
  | TargetBuildStarted of { target: Riot_model.Target.t; host: bool }
  | TargetBuildFinished of {
      target: Riot_model.Target.t;
      result_count: int;
      had_partial_failure: bool
    }
  | CacheGenerationRecordingStarted of { lane_count: int; new_entry_count: int }
  | CacheGenerationRecorded of { lane_count: int; new_entry_count: int }
  | ReturningResults of { result_count: int; had_partial_failure: bool }

let phase_name_of_runtime_phase = function
  | TargetsResolved _ -> "targets_resolved"
  | ToolchainsEnsured _ -> "toolchains_ensured"
  | ToolchainsValidated _ -> "toolchains_validated"
  | RuntimeStarting -> "runtime_starting"
  | RuntimeStarted -> "runtime_started"
  | PackagePlanningStarted _ -> "package_planning_started"
  | PackagePlanningFinished _ -> "package_planning_finished"
  | PackageExecutionStarted _ -> "package_execution_started"
  | PackageExecutionFinished _ -> "package_execution_finished"
  | TargetBuildStarted _ -> "target_build_started"
  | TargetBuildFinished _ -> "target_build_finished"
  | CacheGenerationRecordingStarted _ -> "cache_generation_recording_started"
  | CacheGenerationRecorded _ -> "cache_generation_recorded"
  | ReturningResults _ -> "returning_results"

let runtime_phase_fields = function
  | TargetsResolved { target_count }
  | ToolchainsEnsured { target_count }
  | ToolchainsValidated { target_count } -> [ ("target_count", Data.Json.Int target_count) ]
  | PackagePlanningStarted { lane_count; package_count }
  | PackageExecutionStarted { lane_count; package_count } -> [
    ("lane_count", Data.Json.Int lane_count);
    ("package_count", Data.Json.Int package_count);
  ]
  | PackagePlanningFinished {
    lane_count;
    package_count;
    deferred_count;
    execution_required_count;
    finalized_count;
    cached_count;
    skipped_count;
    failed_count;
    error_count;
  } -> [
    ("lane_count", Data.Json.Int lane_count);
    ("package_count", Data.Json.Int package_count);
    ("deferred_count", Data.Json.Int deferred_count);
    ("execution_required_count", Data.Json.Int execution_required_count);
    ("finalized_count", Data.Json.Int finalized_count);
    ("cached_count", Data.Json.Int cached_count);
    ("skipped_count", Data.Json.Int skipped_count);
    ("failed_count", Data.Json.Int failed_count);
    ("error_count", Data.Json.Int error_count);
  ]
  | PackageExecutionFinished {
    lane_count;
    package_count;
    finalized_count;
    built_count;
    failed_count;
    error_count;
  } -> [
    ("lane_count", Data.Json.Int lane_count);
    ("package_count", Data.Json.Int package_count);
    ("finalized_count", Data.Json.Int finalized_count);
    ("built_count", Data.Json.Int built_count);
    ("failed_count", Data.Json.Int failed_count);
    ("error_count", Data.Json.Int error_count);
  ]
  | RuntimeStarting
  | RuntimeStarted -> []
  | TargetBuildStarted { target; host } -> [
    ("target", Data.Json.String (Riot_model.Target.to_string target));
    ("host", Data.Json.Bool host);
  ]
  | TargetBuildFinished { target; result_count; had_partial_failure } -> [
    ("target", Data.Json.String (Riot_model.Target.to_string target));
    ("result_count", Data.Json.Int result_count);
    ("had_partial_failure", Data.Json.Bool had_partial_failure);
  ]
  | CacheGenerationRecordingStarted { lane_count; new_entry_count }
  | CacheGenerationRecorded { lane_count; new_entry_count } -> [
    ("lane_count", Data.Json.Int lane_count);
    ("new_entry_count", Data.Json.Int new_entry_count)
  ]
  | ReturningResults { result_count; had_partial_failure } -> [
    ("result_count", Data.Json.Int result_count);
    ("had_partial_failure", Data.Json.Bool had_partial_failure);
  ]

let to_json = function
  | Pm event -> Some (Riot_model.Event.to_json event)
  | BuildingTarget { target; host } -> Some (Data.Json.Object [
    ("type", Data.Json.String "BuildingTarget");
    ("target", Data.Json.String (Riot_model.Target.to_string target));
    ("host", Data.Json.Bool host);
  ])
  | CacheGc event -> Some (Riot_store.Cache_gc.event_to_json event)
  | Phase phase -> Some (Data.Json.Object ([
    ("type", Data.Json.String "BuildPhase");
    ("phase", Data.Json.String (phase_name_of_runtime_phase phase));
  ]
  @ runtime_phase_fields phase))
