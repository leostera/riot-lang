open Std

(** Structured events emitted by [riot-build].

    Use these events to drive human output, JSON output, or higher-level UI
    integrations without scraping terminal text.
*)
type t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: Riot_model.Target.t; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Telemetry of Std.Telemetry.event
  | Phase of runtime_phase

and runtime_phase =
  | TargetsResolved of { target_count: int }
  | ToolchainsEnsured of { target_count: int }
  | ToolchainsValidated of { target_count: int }
  | RuntimeStarting
  | RuntimeStarted
  | BuildLockWaiting of { lock_path: Path.t }
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
      error_count: int
    }
  | PackageExecutionStarted of { lane_count: int; package_count: int }
  | PackageExecutionFinished of {
      lane_count: int;
      package_count: int;
      finalized_count: int;
      built_count: int;
      failed_count: int;
      error_count: int
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

(** Convert an event into a JSON payload when it has a machine-readable form. *)
val to_json: t -> Data.Json.t option
