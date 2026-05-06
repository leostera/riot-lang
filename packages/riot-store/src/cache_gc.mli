open Std
open Riot_model

type generation_lane = {
  profile: string;
  target: Riot_model.Target.t;
  hashes: string list;
}
type new_cache_entry = {
  profile: string;
  target: Riot_model.Target.t;
  hash: string;
  size_bytes: int64;
}
type summary = {
  ran_gc: bool;
  kept_generations: int;
  deleted_generations: int;
  deleted_entries: int;
  size_before_bytes: int64;
  size_after_bytes: int64;
}
type error = string
type trigger =
  | Manual
  | Post_build
type event =
  | GcStarted of {
      trigger: trigger;
    }
  | GcCacheScanStarted of {
      trigger: trigger;
      build_root: Path.t;
    }
  | GcCacheEntryScanStarted of {
      trigger: trigger;
      hash: string;
      path: Path.t;
    }
  | GcCacheEntryScanned of {
      trigger: trigger;
      hash: string;
      path: Path.t;
      size_bytes: int64;
    }
  | GcCacheScanCompleted of {
      trigger: trigger;
      entry_count: int;
      total_size_bytes: int64;
    }
  | GcPlanComputed of {
      trigger: trigger;
      deleted_entries: int;
      deleted_generations: int;
      reclaimable_bytes: int64;
    }
  | GcCacheEntryDeleteStarted of {
      trigger: trigger;
      hash: string;
      path: Path.t;
      size_bytes: int64;
    }
  | GcGenerationDeleteStarted of {
      trigger: trigger;
      path: Path.t;
    }
  | GcSkipped of {
      trigger: trigger;
      summary: summary;
    }
  | GcCompleted of {
      trigger: trigger;
      summary: summary;
    }
  | GcFailed of {
      trigger: trigger;
      error: string;
    }
  | ForceCleanStarted of {
      build_root: Path.t;
    }
  | ForceCleanCompleted of {
      build_root: Path.t;
    }
  | ForceCleanFailed of {
      build_root: Path.t;
      error: string;
    }

val clean: workspace:Workspace.t -> (summary, error) result

val clean_with_events: workspace:Workspace.t -> on_event:(event -> unit) -> (summary, error) result

val force_clean: workspace:Workspace.t -> (unit, error) result

val force_clean_with_events:
  workspace:Workspace.t ->
  on_event:(event -> unit) ->
  (unit, error) result

val record_successful_build:
  workspace:Workspace.t ->
  lanes:generation_lane list ->
  new_entries:new_cache_entry list ->
  (summary, error) result

val record_successful_build_with_events:
  workspace:Workspace.t ->
  on_event:(event -> unit) ->
  lanes:generation_lane list ->
  new_entries:new_cache_entry list ->
  (summary, error) result

val summary_message: summary -> string

val summary_serializer: summary Serde.Ser.t

val generation_lane_serializer: generation_lane Serde.Ser.t

val generation_lane_deserializer: generation_lane Serde.De.t

val event_message: event -> string

val event_serializer: event Serde.Ser.t
