open Std

type error = {
  lane: Build_lane.locked Build_lane.t;
  reason: string;
}
type completion = {
  target: Riot_model.Target.t;
  result_count: int;
  had_partial_failure: bool;
}
type summary = {
  completions: completion list;
  lane_results: Lane_result.t list;
  errors: error list;
  had_failure: bool;
}
type event =
  | PlanningStarted of { lane_count: int; package_count: int }
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
  | PlanningFinished of {
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
  | ExecutionStarted of { lane_count: int; package_count: int }
  | ExecutionFinished of {
      lane_count: int;
      package_count: int;
      finalized_count: int;
      built_count: int;
      failed_count: int;
      error_count: int;
    }

val run:
  parallelism:int ->
  ?on_event:(event -> unit) ->
  Build_lane.locked Build_lane.t list ->
  summary
