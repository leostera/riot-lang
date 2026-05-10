(**
   Shared structured events for Riot.

   [t] is the stable envelope. The payload is namespaced by domain through
   [kind], and serialized event names use dotted strings such as
   ["riot.build.cache.hit"], ["riot.deps.resolution.started"], and
   ["riot.test.suite.completed"].
*)
open Std
open Std.Data

module Pm_error = Pm_error

val strip_ansi_codes: string -> string

type level =
  | Error
  | Warn
  | Info
  | Debug
  | Trace

val level_to_string: level -> string

type skip_reason =
  | DependenciesFailed of Package_name.t list
type compile_error_kind =
  | SyntaxError
  | TypeError of { description: string }
  | UnboundValue of { name: string }
  | UnboundModule of { name: string }
  | FileNotFound of { filename: string }
  | OtherError of { message: string }
type compile_error = {
  file: string;
  line: int;
  span: int * int;
  hint: string;
  kind: compile_error_kind;
  raw: string;
}
type build_result = {
  package: Package_name.t;
  success: bool;
  duration_ms: int;
  modules_compiled: int;
  cache_hits: int;
  cache_misses: int;
  errors: compile_error list;
}
type build_runtime_phase =
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
      target: Target.t;
      started_at: Time.Instant.t;
    }
  | BuildLaneLockAcquired of {
      target: Target.t;
      acquired_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildLaneToolchainInitialized of {
      target: Target.t;
      initialized_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildLaneStoreCreated of {
      target: Target.t;
      created_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildLanePreparationFinished of {
      target: Target.t;
      completed_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | PackagePlanningStarted of { lane_count: int; package_count: int }
  | PackagePlanStarted of {
      package: Package.t;
      build_target: Target.t;
      source_count: int;
      started_at: Time.Instant.t;
    }
  | PackagePlanSourceStarted of {
      package: Package.t;
      build_target: Target.t;
      source: Path.t;
      source_index: int;
      source_count: int;
      started_at: Time.Instant.t;
    }
  | PackagePlanFinished of {
      package: Package.t;
      build_target: Target.t;
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
      package: Package.t;
      build_target: Target.t;
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
      target: Target.t;
      host: bool;
    }
  | TargetBuildFinished of {
      target: Target.t;
      result_count: int;
      had_partial_failure: bool;
    }
  | CacheGenerationRecordingStarted of { lane_count: int; new_entry_count: int }
  | CacheGenerationRecorded of { lane_count: int; new_entry_count: int }
  | ReturningResults of { result_count: int; had_partial_failure: bool }
type build_summary = {
  duration: Time.Duration.t;
  built_count: int;
  cached_count: int;
  failed_count: int;
  skipped_count: int;
}
type build_artifact_status =
  | Fresh
  | Cached
type build_warning_source =
  | FreshWarning
  | CachedWarning
type build_package_error =
  | BuildPlanningFailed of { message: string }
  | BuildExecutionFailed of { message: string }
  | BuildActionExecutionFailed of { message: string }
  | BuildActionOutputsNotCreated of {
      missing: Path.t list;
    }
  | BuildActionDependenciesFailed of {
      failed: string list;
    }
type build_event =
  | BuildStarted of {
      packages: Package_name.t list;
      total_modules: int;
      workers: int;
    }
  | BuildCompleted of {
      duration_ms: int;
      results: build_result list;
      succeeded: Package_name.t list;
      failed: Package_name.t list;
    }
  | BuildGraphCreating
  | BuildGraphCreated of { nodes: int; duration_ms: int }
  | BuildPackageStarted of {
      package: Package_name.t;
    }
  | BuildPackageCompleted of build_result
  | BuildPackageSkipped of {
      package: Package_name.t;
      reason: skip_reason;
    }
  | BuildCompileError of {
      package: Package_name.t;
      error: compile_error;
    }
  | BuildCompilingInterface of {
      package: Package_name.t;
      file: string;
    }
  | BuildCompilingImplementation of {
      package: Package_name.t;
      file: string;
    }
  | BuildLinkingLibrary of {
      package: Package_name.t;
      output: string;
    }
  | BuildLinkingExecutable of {
      package: Package_name.t;
      output: string;
    }
  | BuildComputingHash of {
      package: Package_name.t;
    }
  | BuildHashComputed of {
      package: Package_name.t;
      hash: string;
    }
  | BuildCopyingFile of { source: string; dest: string }
  | BuildWritingFile of { path: string }
  | BuildCreatingDirectory of { path: string }
  | BuildDependencyMissing of {
      package: Package_name.t;
      missing: Package_name.t list;
    }
  | BuildDependencySatisfied of {
      package: Package_name.t;
    }
  | BuildCycleDetected of {
      packages: Package_name.t list;
    }
  | BuildQueuePackage of {
      package: Package_name.t;
      queue: [`Ready | `Waiting];
    }
  | BuildQueueStats of { ready: int; waiting: int; busy: int }
  | BuildWorkerStarted of {
      worker_id: Worker_id.t;
    }
  | BuildWorkerAssigned of {
      worker_id: Worker_id.t;
      package: Package_name.t;
    }
  | BuildWorkerIdle of {
      worker_id: Worker_id.t;
    }
  | BuildWorkerPoolCreating of { workers: int }
  | BuildWorkerPoolCreated of { workers: int; duration_ms: int }
  | BuildWorkerPoolStarted of { workers: int }
  | BuildTargetBuilding of {
      target: Target.t;
      host: bool;
    }
  | BuildPackageCompilationStarted of {
      package: Package.t;
      build_target: Target.t;
      action_count: int;
      started_at: Time.Instant.t;
    }
  | BuildSandboxCreated of {
      package: Package.t;
      build_target: Target.t;
      path: Path.t;
      created_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildSandboxInputsCopied of {
      package: Package.t;
      build_target: Target.t;
      input_count: int;
      copied_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildSandboxDependenciesCopied of {
      package: Package.t;
      build_target: Target.t;
      dependency_count: int;
      object_count: int;
      copied_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildPackageExecutionPrepared of {
      package: Package.t;
      build_target: Target.t;
      input_count: int;
      dependency_count: int;
      dependency_object_count: int;
      prepared_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildPackageWarnings of {
      package: Package.t;
      build_target: Target.t;
      source: build_warning_source;
      messages: string list;
    }
  | BuildPackageFinished of {
      package: Package.t;
      build_target: Target.t;
      status: build_artifact_status;
      duration: Time.Duration.t;
    }
  | BuildPackageFailed of {
      package: Package.t;
      build_target: Target.t;
      error: build_package_error;
    }
  | BuildPackageSkippedDetailed of {
      package: Package.t;
      build_target: Target.t;
      reason: string;
    }
  | BuildActionStarted of {
      package: Package.t;
      build_target: Target.t;
      action_id: string;
      action_label: string;
      started_at: Time.Instant.t;
    }
  | BuildActionCommandStarted of {
      package: Package.t;
      build_target: Target.t;
      action_id: string;
      action_label: string;
      command: string;
      started_at: Time.Instant.t;
    }
  | BuildActionCompleted of {
      package: Package.t;
      build_target: Target.t;
      action_id: string;
      action_label: string;
      status: build_artifact_status;
      completed_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | BuildActionFailed of {
      package: Package.t;
      build_target: Target.t;
      action_id: string;
      action_label: string;
      failed_at: Time.Instant.t;
      error: string;
    }
  | BuildActionCacheHit of {
      package: Package.t;
      action_id: string;
      action_label: string;
      hash: string;
    }
  | BuildActionCacheMiss of {
      package: Package.t;
      action_id: string;
      action_label: string;
      hash: string;
    }
  | BuildPhase of build_runtime_phase
  | BuildCommandFinished of build_summary
type cache_gc_summary = {
  ran_gc: bool;
  kept_generations: int;
  deleted_generations: int;
  deleted_entries: int;
  size_before_bytes: int64;
  size_after_bytes: int64;
}
type cache_gc_trigger =
  | Manual
  | Post_build
type cache_event =
  | CacheBuildHit of {
      package: Package_name.t;
      hash: string;
    }
  | CacheBuildMiss of {
      package: Package_name.t;
      hash: string;
    }
  | CacheBuildStored of {
      package: Package_name.t;
      hash: string;
      artifacts: string list;
    }
  | CacheStoreCreating
  | CacheStoreCreated of { duration_ms: int }
  | CacheGcStarted of {
      trigger: cache_gc_trigger;
    }
  | CacheGcCacheScanStarted of {
      trigger: cache_gc_trigger;
      build_root: Path.t;
    }
  | CacheGcCacheEntryScanStarted of {
      trigger: cache_gc_trigger;
      hash: string;
      path: Path.t;
    }
  | CacheGcCacheEntryScanned of {
      trigger: cache_gc_trigger;
      hash: string;
      path: Path.t;
      size_bytes: int64;
    }
  | CacheGcCacheScanCompleted of {
      trigger: cache_gc_trigger;
      entry_count: int;
      total_size_bytes: int64;
    }
  | CacheGcPlanComputed of {
      trigger: cache_gc_trigger;
      deleted_entries: int;
      deleted_generations: int;
      reclaimable_bytes: int64;
    }
  | CacheGcCacheEntryDeleteStarted of {
      trigger: cache_gc_trigger;
      hash: string;
      path: Path.t;
      size_bytes: int64;
    }
  | CacheGcGenerationDeleteStarted of {
      trigger: cache_gc_trigger;
      path: Path.t;
    }
  | CacheGcSkipped of {
      trigger: cache_gc_trigger;
      summary: cache_gc_summary;
    }
  | CacheGcCompleted of {
      trigger: cache_gc_trigger;
      summary: cache_gc_summary;
    }
  | CacheGcFailed of {
      trigger: cache_gc_trigger;
      error: string;
    }
  | CacheForceCleanStarted of {
      build_root: Path.t;
    }
  | CacheForceCleanCompleted of {
      build_root: Path.t;
    }
  | CacheForceCleanFailed of {
      build_root: Path.t;
      error: string;
    }
type deps_event =
  | DepsLockfileReadStarted of { path: string }
  | DepsLockfileReadFinished of { path: string; duration_ms: int }
  | DepsLockfileReadFailed of {
      path: string;
      error: Pm_error.t;
    }
  | DepsLockfileWriteStarted of { path: string }
  | DepsLockfileWriteFinished of { path: string; duration_ms: int }
  | DepsLockfileWriteFailed of {
      path: string;
      error: Pm_error.t;
    }
  | DepsResolutionStarted of {
      packages: Package_name.t list;
      mode: [`Refresh | `Unlock];
    }
  | DepsResolutionUsingExistingLock of { path: string }
  | DepsResolutionRefreshingLock of { path: string }
  | DepsResolutionUnlocking of {
      path: string option;
    }
  | DepsResolutionFinished of { duration_ms: int; resolved_packages: int; resolved_edges: int }
  | DepsResolutionFailed of {
      error: Pm_error.t;
    }
  | DepsRegistryIndexUpdating of { registry: string }
  | DepsUniverseBuilding of {
      packages: Package_name.t list;
    }
  | DepsUniverseBuilt of {
      runtime_packages: int;
      build_packages: int;
      dev_packages: int;
      duration_ms: int;
    }
  | DepsPackageMetadataFetchStarted of {
      registry: string;
      package: Package_name.t;
    }
  | DepsPackageMetadataFetchFinished of {
      registry: string;
      package: Package_name.t;
      version: string option;
      duration_ms: int;
    }
  | DepsPackageMetadataFetchFailed of {
      registry: string;
      package: Package_name.t;
      error: Pm_error.t;
    }
  | DepsSourceMaterializationStarted of {
      source_locator: string;
      ref_: string option;
    }
  | DepsSourceMaterializationFinished of {
      source_locator: string;
      ref_: string option;
      package: Package_name.t;
      version: string option;
    }
  | DepsManifestUpdated of {
      path: string;
      section: string;
      operation: [`Add | `Remove];
      dependency: string;
    }
  | DepsPackageVersionLocked of {
      package: Package_name.t;
      version: string;
    }
  | DepsPackageVersionsUnchanged of { packages: int }
  | DepsPackageVersionUpdated of {
      package: Package_name.t;
      from_version: string;
      to_version: string;
    }
  | DepsPackageManifestFetchStarted of {
      package: Package_name.t;
      version: string;
    }
  | DepsPackageManifestFetchFinished of {
      package: Package_name.t;
      version: string;
      duration_ms: int;
    }
  | DepsPackageManifestFetchFailed of {
      package: Package_name.t;
      version: string option;
      error: Pm_error.t;
    }
  | DepsPackageDownloadQueued of {
      package: Package_name.t;
      version: string;
      path: string;
    }
  | DepsPackageDownloadStarted of {
      package: Package_name.t;
      version: string;
      path: string;
    }
  | DepsPackageDownloadFinished of {
      package: Package_name.t;
      version: string;
      path: string;
      duration_ms: int;
    }
  | DepsPackageDownloadFailed of {
      package: Package_name.t;
      version: string;
      path: string;
      error: Pm_error.t;
    }
  | DepsPackageDownloadSkipped of {
      package: Package_name.t;
      version: string;
      path: string;
      reason: string;
    }
  | DepsPackageCacheHit of {
      package: Package_name.t;
      version: string;
      path: string;
    }
  | DepsPackageMaterializationStarted of {
      package: Package_name.t;
      version: string;
      path: string;
    }
  | DepsPackageMaterializationFinished of {
      package: Package_name.t;
      version: string;
      path: string;
      duration_ms: int;
    }
  | DepsPackageMaterializationFailed of {
      package: Package_name.t;
      version: string;
      path: string;
      error: Pm_error.t;
    }
  | DepsPackageResolvedForBuild of {
      package: Package_name.t;
      version: string option;
      path: string;
      workspace: bool;
    }
type test_suite = {
  package_name: Package_name.t;
  suite_name: string;
}
type test_case_type =
  | Unit
  | Property of { examples: int }
  | Fuzz of { seeds: int }
type test_case_size =
  | Small
  | Large
type test_case_reliability =
  | Stable
  | Flaky of { retry_attempts: int }
type test_case_status =
  | Passed
  | Failed of string
  | TimedOut of { timeout_ms: int }
  | Skipped
type test_case_result = {
  index: int;
  name: string;
  test_type: test_case_type;
  size: test_case_size;
  reliability: test_case_reliability;
  attempts: int;
  result: test_case_status;
  duration_us: int;
}
type failed_test = {
  suite: test_suite;
  name: string;
  message: string;
  duration_us: int;
}
type test_suite_summary = {
  total: int;
  passed: int;
  failed: int;
  skipped: int;
  duration_us: int;
  results: test_case_result list;
}
type test_event =
  | TestNoSuitesFound of {
      package_name: Package_name.t option;
      suite_name: string option;
    }
  | TestSuitesCollected of {
      package_name: Package_name.t option;
      suite_name: string option;
      suite_count: int;
    }
  | TestResolvingSuiteBinary of test_suite
  | TestSuiteBinaryResolved of {
      suite: test_suite;
      binary_path: Path.t;
    }
  | TestRunningSuite of test_suite
  | TestExecutingSuiteBinary of {
      suite: test_suite;
      binary_path: Path.t;
      args: string list;
    }
  | TestSuiteHeartbeat of {
      suite: test_suite;
      binary_path: Path.t;
      elapsed_us: int;
    }
  | TestSuiteBinaryFinished of {
      suite: test_suite;
      binary_path: Path.t;
      status: int;
      stdout_bytes: int;
      stderr_bytes: int;
    }
  | TestSuiteProgress of {
      suite: test_suite;
      event: Json.t;
    }
  | TestParsingSuiteOutput of {
      suite: test_suite;
      binary_path: Path.t;
    }
  | TestSuiteCompleted of {
      suite: test_suite;
      status: int;
      stdout: string;
      stderr: string;
      started_at_us: int option;
      completed_at_us: int option;
      duration_us: int option;
      summary: test_suite_summary;
    }
  | TestSummary of {
      total: int;
      passed: int;
      failed: int;
      skipped: int;
      failed_tests: failed_test list;
    }
type workspace_event =
  | WorkspaceEmpty
  | WorkspaceScanning
  | WorkspaceScanned of { packages: int; duration_ms: int }
type server_event =
  | ServerStarted of { pid: string }
  | ServerScanning of { root: string }
  | ServerRestarted of { packages: int; toolchain: string }
  | ServerShutdown
type rpc_event =
  | RpcRequestReceived of {
      request_type: string;
      args: Json.t;
    }
  | RpcResponseSent of {
      result: (unit, string) result;
    }
type mcp_event =
  | McpToolCall of {
      tool: string;
      args: Json.t;
    }
type command_install_mode =
  | CommandInstallLocal
  | CommandInstallGlobal
type command_error = {
  kind: string;
  details: (string * Json.t) list;
  message: string;
}
type command_event =
  | CommandBinaryRunning of {
      package: Package_name.t;
      binary: string;
      args: string list;
    }
  | CommandBinaryInstalling of {
      package: Package_name.t;
      binary: string;
    }
  | CommandBinaryPromoted of {
      binary: string;
      destination: Path.t;
      mode: command_install_mode;
    }
  | CommandBinaryInstalled of {
      binary: string;
      duration_ms: int;
      destination: Path.t;
      mode: command_install_mode;
    }
  | CommandError of command_error
type unknown_event = {
  event: string;
  message: string option;
  data: Json.t;
}
type kind =
  | Build of build_event
  | Cache of cache_event
  | Deps of deps_event
  | Test of test_event
  | Workspace of workspace_event
  | Server of server_event
  | Rpc of rpc_event
  | Mcp of mcp_event
  | Command of command_event
  | Unknown of unknown_event
type t = {
  timestamp: Std.DateTime.t;
  session_id: Session_id.t;
  level: level;
  kind: kind;
}

val create: session_id:Session_id.t -> level:level -> kind -> t

val name: kind -> string

val display: kind -> string

val to_string: t -> string

val serializer: t Serde.Ser.t

val deserializer: t Serde.De.t

val to_json: t -> Json.t

val from_json: Json.t -> (t, string) result
