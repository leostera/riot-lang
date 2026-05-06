open Std

type t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of {
      target: Riot_model.Target.t;
      host: bool;
    }
  | CacheGc of Riot_store.Cache_gc.event
  | Telemetry of Std.Telemetry.event
  | Phase of runtime_phase

and runtime_phase =
  | TargetsResolved of { target_count: int }
  | ToolchainsEnsured of { target_count: int }
  | ToolchainsValidated of { target_count: int }
  | RuntimeStarting
  | RuntimeStarted
  | BuildLockWaiting of {
      lock_path: Path.t;
    }
  | BuildLanesPreparationStarted of {
      target_count: int;
      started_at: Time.Instant.t;
    }
  | BuildLanesPreparationFinished of {
      lane_count: int;
      completed_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildUnitPlanCreated of {
      unit_count: int;
      planned_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildLanePreparationStarted of {
      target: Riot_model.Target.t;
      started_at: Time.Instant.t;
    }
  | BuildLaneLockAcquired of {
      target: Riot_model.Target.t;
      acquired_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildLaneToolchainInitialized of {
      target: Riot_model.Target.t;
      initialized_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildLaneStoreCreated of {
      target: Riot_model.Target.t;
      created_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildLanePreparationFinished of {
      target: Riot_model.Target.t;
      completed_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | PackagePlanningStarted of { lane_count: int; package_count: int }
  | PackagePlanStarted of {
      package: Riot_model.Package.t;
      build_target: Riot_model.Target.t;
      source_count: int;
      started_at: Time.Instant.t;
    }
  | PackagePlanSourceStarted of {
      package: Riot_model.Package.t;
      build_target: Riot_model.Target.t;
      source: Path.t;
      source_index: int;
      source_count: int;
      started_at: Time.Instant.t;
    }
  | PackagePlanFinished of {
      package: Riot_model.Package.t;
      build_target: Riot_model.Target.t;
      source_count: int;
      completed_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
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
  | PackageActionGraphPlanned of {
      package: Riot_model.Package.t;
      build_target: Riot_model.Target.t;
      action_count: int;
      planned_at: Time.Instant.t;
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
  | TargetBuildStarted of {
      target: Riot_model.Target.t;
      host: bool;
    }
  | TargetBuildFinished of {
      target: Riot_model.Target.t;
      result_count: int;
      had_partial_failure: bool;
    }
  | CacheGenerationRecordingStarted of { lane_count: int; new_entry_count: int }
  | CacheGenerationRecorded of { lane_count: int; new_entry_count: int }
  | ReturningResults of { result_count: int; had_partial_failure: bool }

let phase_name_of_runtime_phase = fun __tmp1 ->
  match __tmp1 with
  | TargetsResolved _ -> "targets_resolved"
  | ToolchainsEnsured _ -> "toolchains_ensured"
  | ToolchainsValidated _ -> "toolchains_validated"
  | RuntimeStarting -> "runtime_starting"
  | RuntimeStarted -> "runtime_started"
  | BuildLockWaiting _ -> "build_lock_waiting"
  | BuildLanesPreparationStarted _ -> "build_lanes_preparation_started"
  | BuildLanesPreparationFinished _ -> "build_lanes_preparation_finished"
  | BuildUnitPlanCreated _ -> "build_unit_plan_created"
  | BuildLanePreparationStarted _ -> "build_lane_preparation_started"
  | BuildLaneLockAcquired _ -> "build_lane_lock_acquired"
  | BuildLaneToolchainInitialized _ -> "build_lane_toolchain_initialized"
  | BuildLaneStoreCreated _ -> "build_lane_store_created"
  | BuildLanePreparationFinished _ -> "build_lane_preparation_finished"
  | PackagePlanningStarted _ -> "package_planning_started"
  | PackagePlanStarted _ -> "package_plan_started"
  | PackagePlanSourceStarted _ -> "package_plan_source_started"
  | PackagePlanFinished _ -> "package_plan_finished"
  | PackagePlanningFinished _ -> "package_planning_finished"
  | PackageActionGraphPlanned _ -> "package_action_graph_planned"
  | PackageExecutionStarted _ -> "package_execution_started"
  | PackageExecutionFinished _ -> "package_execution_finished"
  | TargetBuildStarted _ -> "target_build_started"
  | TargetBuildFinished _ -> "target_build_finished"
  | CacheGenerationRecordingStarted _ -> "cache_generation_recording_started"
  | CacheGenerationRecorded _ -> "cache_generation_recorded"
  | ReturningResults _ -> "returning_results"

let runtime_phase_fields = fun __tmp1 ->
  match __tmp1 with
  | TargetsResolved { target_count }
  | ToolchainsEnsured { target_count }
  | ToolchainsValidated { target_count } -> [ ("target_count", Data.Json.Int target_count); ]
  | PackagePlanningStarted { lane_count; package_count }
  | PackageExecutionStarted { lane_count; package_count } ->
      [ ("lane_count", Data.Json.Int lane_count); ("package_count", Data.Json.Int package_count); ]
  | PackagePlanStarted {
      package;
      build_target;
      source_count;
      started_at = _;
    } ->
      [
        ("package", Data.Json.String (Riot_model.Package_name.to_string package.name));
        ("target", Data.Json.String (Riot_model.Target.to_string build_target));
        ("source_count", Data.Json.Int source_count);
      ]
  | PackagePlanSourceStarted {
      package;
      build_target;
      source;
      source_index;
      source_count;
      started_at = _;
    } ->
      [
        ("package", Data.Json.String (Riot_model.Package_name.to_string package.name));
        ("target", Data.Json.String (Riot_model.Target.to_string build_target));
        ("source", Data.Json.String (Path.to_string source));
        ("source_index", Data.Json.Int source_index);
        ("source_count", Data.Json.Int source_count);
      ]
  | PackagePlanFinished {
      package;
      build_target;
      source_count;
      completed_at = _;
      duration;
    } ->
      [
        ("package", Data.Json.String (Riot_model.Package_name.to_string package.name));
        ("target", Data.Json.String (Riot_model.Target.to_string build_target));
        ("source_count", Data.Json.Int source_count);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
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
    } ->
      [
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
  | PackageActionGraphPlanned {
      package;
      build_target;
      action_count;
      planned_at = _;
    } ->
      [
        ("package", Data.Json.String (Riot_model.Package_name.to_string package.name));
        ("target", Data.Json.String (Riot_model.Target.to_string build_target));
        ("action_count", Data.Json.Int action_count);
      ]
  | PackageExecutionFinished {
      lane_count;
      package_count;
      finalized_count;
      built_count;
      failed_count;
      error_count;
    } ->
      [
        ("lane_count", Data.Json.Int lane_count);
        ("package_count", Data.Json.Int package_count);
        ("finalized_count", Data.Json.Int finalized_count);
        ("built_count", Data.Json.Int built_count);
        ("failed_count", Data.Json.Int failed_count);
        ("error_count", Data.Json.Int error_count);
      ]
  | RuntimeStarting
  | RuntimeStarted -> []
  | BuildLockWaiting { lock_path } ->
      [ ("lock_path", Data.Json.String (Path.to_string lock_path)); ]
  | BuildLanesPreparationStarted { target_count; started_at = _ } ->
      [ ("target_count", Data.Json.Int target_count); ]
  | BuildLanesPreparationFinished {
      lane_count;
      completed_at = _;
      duration;
    } ->
      [
        ("lane_count", Data.Json.Int lane_count);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildUnitPlanCreated {
      unit_count;
      planned_at = _;
      duration;
    } ->
      [
        ("unit_count", Data.Json.Int unit_count);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildLanePreparationStarted { target; started_at = _ } ->
      [ ("target", Data.Json.String (Riot_model.Target.to_string target)); ]
  | BuildLaneLockAcquired {
      target;
      acquired_at = _;
      duration;
    } ->
      [
        ("target", Data.Json.String (Riot_model.Target.to_string target));
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildLaneToolchainInitialized {
      target;
      initialized_at = _;
      duration;
    } ->
      [
        ("target", Data.Json.String (Riot_model.Target.to_string target));
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildLaneStoreCreated {
      target;
      created_at = _;
      duration;
    } ->
      [
        ("target", Data.Json.String (Riot_model.Target.to_string target));
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildLanePreparationFinished {
      target;
      completed_at = _;
      duration;
    } ->
      [
        ("target", Data.Json.String (Riot_model.Target.to_string target));
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ]
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
      [
        ("lane_count", Data.Json.Int lane_count);
        ("new_entry_count", Data.Json.Int new_entry_count);
      ]
  | ReturningResults { result_count; had_partial_failure } ->
      [
        ("result_count", Data.Json.Int result_count);
        ("had_partial_failure", Data.Json.Bool had_partial_failure);
      ]

let to_json = fun __tmp1 ->
  match __tmp1 with
  | Pm event -> Some (Riot_model.Event.to_json event)
  | BuildingTarget { target; host } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "BuildingTarget");
        ("target", Data.Json.String (Riot_model.Target.to_string target));
        ("host", Data.Json.Bool host);
      ])
  | CacheGc _ -> None
  | Telemetry event -> Telemetry_events.to_json event
  | Phase phase ->
      Some (Data.Json.Object ([
        ("type", Data.Json.String "BuildPhase");
        ("phase", Data.Json.String (phase_name_of_runtime_phase phase));
      ]
      @ runtime_phase_fields phase))

let timestamp = fun __tmp1 ->
  match __tmp1 with
  | Telemetry event -> Telemetry_events.event_timestamp event
  | Phase (BuildLanesPreparationStarted { started_at; _ })
  | Phase (BuildLanePreparationStarted { started_at; _ })
  | Phase (PackagePlanStarted { started_at; _ })
  | Phase (PackagePlanSourceStarted { started_at; _ }) ->
      Some ("started_at_us", started_at)
  | Phase (BuildLanesPreparationFinished { completed_at; _ })
  | Phase (BuildLanePreparationFinished { completed_at; _ })
  | Phase (PackagePlanFinished { completed_at; _ }) ->
      Some ("completed_at_us", completed_at)
  | Phase (BuildUnitPlanCreated { planned_at; _ }) ->
      Some ("planned_at_us", planned_at)
  | Phase (BuildLaneLockAcquired { acquired_at; _ }) ->
      Some ("acquired_at_us", acquired_at)
  | Phase (BuildLaneToolchainInitialized { initialized_at; _ }) ->
      Some ("initialized_at_us", initialized_at)
  | Phase (PackageActionGraphPlanned { planned_at; _ }) ->
      Some ("planned_at_us", planned_at)
  | Phase (BuildLaneStoreCreated { created_at; _ }) ->
      Some ("created_at_us", created_at)
  | Pm _
  | BuildingTarget _
  | CacheGc _
  | Phase _ -> None
