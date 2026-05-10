open Std

type t = Riot_model.Event.t

type runtime_phase = Riot_model.Event.build_runtime_phase =
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

let create = fun ~session_id ?(level = Riot_model.Event.Info) kind ->
  Riot_model.Event.create
    ~session_id
    ~level
    kind

let build = fun ~session_id ?level event -> create ~session_id ?level (Riot_model.Event.Build event)

let cache = fun ~session_id ?level event -> create ~session_id ?level (Riot_model.Event.Cache event)

let phase = fun ~session_id phase -> build ~session_id (Riot_model.Event.BuildPhase phase)

let cache_gc_trigger = fun __tmp1 ->
  match __tmp1 with
  | Riot_store.Cache_gc.Manual -> Riot_model.Event.Manual
  | Riot_store.Cache_gc.Post_build -> Riot_model.Event.Post_build

let cache_gc_summary = fun (summary: Riot_store.Cache_gc.summary) ->
  Riot_model.Event.{
    ran_gc = summary.ran_gc;
    kept_generations = summary.kept_generations;
    deleted_generations = summary.deleted_generations;
    deleted_entries = summary.deleted_entries;
    size_before_bytes = summary.size_before_bytes;
    size_after_bytes = summary.size_after_bytes;
  }

let cache_gc_event_kind = fun __tmp1 ->
  let open Riot_model.Event in
  match __tmp1 with
  | Riot_store.Cache_gc.GcStarted { trigger } ->
      CacheGcStarted { trigger = cache_gc_trigger trigger }
  | Riot_store.Cache_gc.GcCacheScanStarted { trigger; build_root } ->
      CacheGcCacheScanStarted { trigger = cache_gc_trigger trigger; build_root }
  | Riot_store.Cache_gc.GcCacheEntryScanStarted { trigger; hash; path } ->
      CacheGcCacheEntryScanStarted { trigger = cache_gc_trigger trigger; hash; path }
  | Riot_store.Cache_gc.GcCacheEntryScanned {
      trigger;
      hash;
      path;
      size_bytes;
    } ->
      CacheGcCacheEntryScanned {
        trigger = cache_gc_trigger trigger;
        hash;
        path;
        size_bytes;
      }
  | Riot_store.Cache_gc.GcCacheScanCompleted { trigger; entry_count; total_size_bytes } ->
      CacheGcCacheScanCompleted {
        trigger = cache_gc_trigger trigger;
        entry_count;
        total_size_bytes;
      }
  | Riot_store.Cache_gc.GcPlanComputed {
      trigger;
      deleted_entries;
      deleted_generations;
      reclaimable_bytes;
    } ->
      CacheGcPlanComputed {
        trigger = cache_gc_trigger trigger;
        deleted_entries;
        deleted_generations;
        reclaimable_bytes;
      }
  | Riot_store.Cache_gc.GcCacheEntryDeleteStarted {
      trigger;
      hash;
      path;
      size_bytes;
    } ->
      CacheGcCacheEntryDeleteStarted {
        trigger = cache_gc_trigger trigger;
        hash;
        path;
        size_bytes;
      }
  | Riot_store.Cache_gc.GcGenerationDeleteStarted { trigger; path } ->
      CacheGcGenerationDeleteStarted { trigger = cache_gc_trigger trigger; path }
  | Riot_store.Cache_gc.GcSkipped { trigger; summary } ->
      CacheGcSkipped { trigger = cache_gc_trigger trigger; summary = cache_gc_summary summary }
  | Riot_store.Cache_gc.GcCompleted { trigger; summary } ->
      CacheGcCompleted {
        trigger = cache_gc_trigger trigger;
        summary = cache_gc_summary summary;
      }
  | Riot_store.Cache_gc.GcFailed { trigger; error } ->
      CacheGcFailed { trigger = cache_gc_trigger trigger; error }
  | Riot_store.Cache_gc.ForceCleanStarted { build_root } -> CacheForceCleanStarted { build_root }
  | Riot_store.Cache_gc.ForceCleanCompleted { build_root } ->
      CacheForceCleanCompleted { build_root }
  | Riot_store.Cache_gc.ForceCleanFailed { build_root; error } ->
      CacheForceCleanFailed { build_root; error }

let cache_gc = fun ~session_id event -> cache ~session_id (cache_gc_event_kind event)

let to_json = fun event -> Some (Riot_model.Event.to_json event)

let timestamp = fun (_: t) -> None
