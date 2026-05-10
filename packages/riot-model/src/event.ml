open Std
open Std.Data
open Std.Collections
open Std.Result.Syntax

module Pm_error = Pm_error
module De = Serde.De
module Ser = Serde.Ser

(**
   Shared event envelope and payload vocabulary for Riot.

   The JSON shape is intentionally stable and ordinary:

   {[
     {
       "timestamp": "...",
       "session_id": "...",
       "level": "info",
       "event": "riot.deps.lockfile.read.finished",
       "message": "Read lockfile riot.lock in 12ms",
       "data": { ... }
     }
   ]}

   Payloads stay namespaced in OCaml so producers and renderers do not need to
   coordinate through ad hoc strings.
*)
let strip_ansi_codes = fun str ->
  let len = String.length str in
  let out = IO.Bytes.create ~size:len in
  let rec skip_until_m index =
    match String.get str ~at:index with
    | None -> len
    | Some char ->
        if Char.equal char 'm' then
          index + 1
        else
          skip_until_m (index + 1)
  in
  let rec strip read_index write_index =
    if read_index >= len then
      IO.Bytes.sub_unchecked out ~offset:0 ~len:write_index
      |> IO.Bytes.to_string
    else
      match String.get str ~at:read_index with
      | None ->
          IO.Bytes.sub_unchecked out ~offset:0 ~len:write_index
          |> IO.Bytes.to_string
      | Some '\027' -> (
          match String.get str ~at:(read_index + 1) with
          | Some '[' -> strip (skip_until_m (read_index + 2)) write_index
          | _ ->
              IO.Bytes.set_unchecked out ~at:write_index ~char:'\027';
              strip (read_index + 1) (write_index + 1)
        )
      | Some char ->
          IO.Bytes.set_unchecked out ~at:write_index ~char;
          strip (read_index + 1) (write_index + 1)
  in
  strip 0 0

type level =
  | Error
  | Warn
  | Info
  | Debug
  | Trace

let level_to_string = fun __tmp1 ->
  match __tmp1 with
  | Error -> "error"
  | Warn -> "warn"
  | Info -> "info"
  | Debug -> "debug"
  | Trace -> "trace"

let level_of_string = fun __tmp1 ->
  match __tmp1 with
  | "error" -> Error
  | "warn" -> Warn
  | "debug" -> Debug
  | "trace" -> Trace
  | _ -> Info

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
  timestamp: DateTime.t;
  session_id: Session_id.t;
  level: level;
  kind: kind;
}

let create = fun ~session_id ~level kind ->
  {
    timestamp = DateTime.now ();
    session_id;
    level;
    kind;
  }

let package_name_json = fun package -> Json.String (Package_name.to_string package)

let package_names_json = fun packages -> Json.Array (List.map packages ~fn:package_name_json)

let path_json = fun path -> Json.String (Path.to_string path)

let target_json = fun target -> Json.String (Target.to_string target)

let json_of_string_option = fun __tmp1 ->
  match __tmp1 with
  | Some value -> Json.String value
  | None -> Json.Null

let json_of_int_option = fun __tmp1 ->
  match __tmp1 with
  | Some value -> Json.Int value
  | None -> Json.Null

let json_of_resolution_mode = fun __tmp1 ->
  match __tmp1 with
  | `Refresh -> Json.String "refresh"
  | `Unlock -> Json.String "unlock"

let json_of_manifest_operation = fun __tmp1 ->
  match __tmp1 with
  | `Add -> Json.String "add"
  | `Remove -> Json.String "remove"

let json_of_trigger = fun __tmp1 ->
  match __tmp1 with
  | Manual -> Json.String "manual"
  | Post_build -> Json.String "post_build"

let json_of_command_install_mode = fun __tmp1 ->
  match __tmp1 with
  | CommandInstallLocal -> Json.String "local"
  | CommandInstallGlobal -> Json.String "global"

let strings_json = fun values -> Json.Array (List.map values ~fn:(fun value -> Json.String value))

let package_name_of_json = fun __tmp1 ->
  match __tmp1 with
  | Json.String package ->
      Package_name.from_string package
      |> Result.map_err ~fn:Package_name.error_message
  | _ -> Error "invalid package name"

let package_names_of_json = fun __tmp1 ->
  match __tmp1 with
  | Json.Array packages ->
      let rec loop acc = fun __tmp1 ->
        match __tmp1 with
        | [] -> Ok (List.reverse acc)
        | json :: rest ->
            let* package = package_name_of_json json in
            loop (package :: acc) rest
      in
      loop [] packages
  | _ -> Error "invalid package names"

let string_option_of_json = fun __tmp1 ->
  match __tmp1 with
  | Json.String value -> Some value
  | Json.Null -> None
  | _ -> None

let resolution_mode_of_json = fun __tmp1 ->
  match __tmp1 with
  | Json.String "refresh" -> Some `Refresh
  | Json.String "unlock" -> Some `Unlock
  | _ -> None

let manifest_operation_of_json = fun __tmp1 ->
  match __tmp1 with
  | Json.String "add" -> Some `Add
  | Json.String "remove" -> Some `Remove
  | _ -> None

let get = fun name fields -> Fields.get name fields

let get_string = fun name fields ->
  match get name fields with
  | Some (Json.String value) -> Some value
  | _ -> None

let get_int = fun name fields ->
  match get name fields with
  | Some (Json.Int value) -> Some value
  | _ -> None

let get_bool = fun name fields ->
  match get name fields with
  | Some (Json.Bool value) -> Some value
  | _ -> None

let build_runtime_phase_name = fun __tmp1 ->
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

let build_runtime_phase_event_suffix = fun __tmp1 ->
  match __tmp1 with
  | TargetsResolved _ -> "targets.resolved"
  | ToolchainsEnsured _ -> "toolchains.ensured"
  | ToolchainsValidated _ -> "toolchains.validated"
  | RuntimeStarting -> "runtime.starting"
  | RuntimeStarted -> "runtime.started"
  | BuildLockWaiting _ -> "lock.waiting"
  | BuildLanesPreparationStarted _ -> "lanes.preparation.started"
  | BuildLanesPreparationFinished _ -> "lanes.preparation.finished"
  | BuildUnitPlanCreated _ -> "unit.plan.created"
  | BuildLanePreparationStarted _ -> "lane.preparation.started"
  | BuildLaneLockAcquired _ -> "lane.lock.acquired"
  | BuildLaneToolchainInitialized _ -> "lane.toolchain.initialized"
  | BuildLaneStoreCreated _ -> "lane.store.created"
  | BuildLanePreparationFinished _ -> "lane.preparation.finished"
  | PackagePlanningStarted _ -> "package.planning.started"
  | PackagePlanStarted _ -> "package.plan.started"
  | PackagePlanSourceStarted _ -> "package.plan.source.started"
  | PackagePlanFinished _ -> "package.plan.finished"
  | PackagePlanningFinished _ -> "package.planning.finished"
  | PackageActionGraphPlanned _ -> "package.action_graph.planned"
  | PackageExecutionStarted _ -> "package.execution.started"
  | PackageExecutionFinished _ -> "package.execution.finished"
  | TargetBuildStarted _ -> "target.started"
  | TargetBuildFinished _ -> "target.finished"
  | CacheGenerationRecordingStarted _ -> "cache.generation.recording.started"
  | CacheGenerationRecorded _ -> "cache.generation.recorded"
  | ReturningResults _ -> "results.returning"

let deps_event_name = fun __tmp1 ->
  match __tmp1 with
  | DepsLockfileReadStarted _ -> "riot.deps.lockfile.read.started"
  | DepsLockfileReadFinished _ -> "riot.deps.lockfile.read.finished"
  | DepsLockfileReadFailed _ -> "riot.deps.lockfile.read.failed"
  | DepsLockfileWriteStarted _ -> "riot.deps.lockfile.write.started"
  | DepsLockfileWriteFinished _ -> "riot.deps.lockfile.write.finished"
  | DepsLockfileWriteFailed _ -> "riot.deps.lockfile.write.failed"
  | DepsResolutionStarted _ -> "riot.deps.resolution.started"
  | DepsResolutionUsingExistingLock _ -> "riot.deps.resolution.using_existing_lock"
  | DepsResolutionRefreshingLock _ -> "riot.deps.resolution.refreshing_lock"
  | DepsResolutionUnlocking _ -> "riot.deps.resolution.unlocking"
  | DepsResolutionFinished _ -> "riot.deps.resolution.finished"
  | DepsResolutionFailed _ -> "riot.deps.resolution.failed"
  | DepsRegistryIndexUpdating _ -> "riot.deps.registry.index.updating"
  | DepsUniverseBuilding _ -> "riot.deps.universe.building"
  | DepsUniverseBuilt _ -> "riot.deps.universe.built"
  | DepsPackageMetadataFetchStarted _ -> "riot.deps.package.metadata.fetch.started"
  | DepsPackageMetadataFetchFinished _ -> "riot.deps.package.metadata.fetch.finished"
  | DepsPackageMetadataFetchFailed _ -> "riot.deps.package.metadata.fetch.failed"
  | DepsSourceMaterializationStarted _ -> "riot.deps.source.materialization.started"
  | DepsSourceMaterializationFinished _ -> "riot.deps.source.materialization.finished"
  | DepsManifestUpdated _ -> "riot.deps.manifest.updated"
  | DepsPackageVersionLocked _ -> "riot.deps.package.version.locked"
  | DepsPackageVersionsUnchanged _ -> "riot.deps.package.versions.unchanged"
  | DepsPackageVersionUpdated _ -> "riot.deps.package.version.updated"
  | DepsPackageManifestFetchStarted _ -> "riot.deps.package.manifest.fetch.started"
  | DepsPackageManifestFetchFinished _ -> "riot.deps.package.manifest.fetch.finished"
  | DepsPackageManifestFetchFailed _ -> "riot.deps.package.manifest.fetch.failed"
  | DepsPackageDownloadQueued _ -> "riot.deps.package.download.queued"
  | DepsPackageDownloadStarted _ -> "riot.deps.package.download.started"
  | DepsPackageDownloadFinished _ -> "riot.deps.package.download.finished"
  | DepsPackageDownloadFailed _ -> "riot.deps.package.download.failed"
  | DepsPackageDownloadSkipped _ -> "riot.deps.package.download.skipped"
  | DepsPackageCacheHit _ -> "riot.deps.package.cache.hit"
  | DepsPackageMaterializationStarted _ -> "riot.deps.package.materialization.started"
  | DepsPackageMaterializationFinished _ -> "riot.deps.package.materialization.finished"
  | DepsPackageMaterializationFailed _ -> "riot.deps.package.materialization.failed"
  | DepsPackageResolvedForBuild _ -> "riot.deps.package.resolved_for_build"

let cache_event_name = fun __tmp1 ->
  match __tmp1 with
  | CacheBuildHit _ -> "riot.build.cache.hit"
  | CacheBuildMiss _ -> "riot.build.cache.miss"
  | CacheBuildStored _ -> "riot.build.cache.stored"
  | CacheStoreCreating -> "riot.cache.store.creating"
  | CacheStoreCreated _ -> "riot.cache.store.created"
  | CacheGcStarted _ -> "riot.cache.gc.started"
  | CacheGcCacheScanStarted _ -> "riot.cache.gc.scan.started"
  | CacheGcCacheEntryScanStarted _ -> "riot.cache.gc.entry.scan.started"
  | CacheGcCacheEntryScanned _ -> "riot.cache.gc.entry.scanned"
  | CacheGcCacheScanCompleted _ -> "riot.cache.gc.scan.completed"
  | CacheGcPlanComputed _ -> "riot.cache.gc.plan.computed"
  | CacheGcCacheEntryDeleteStarted _ -> "riot.cache.gc.entry.delete.started"
  | CacheGcGenerationDeleteStarted _ -> "riot.cache.gc.generation.delete.started"
  | CacheGcSkipped _ -> "riot.cache.gc.skipped"
  | CacheGcCompleted _ -> "riot.cache.gc.completed"
  | CacheGcFailed _ -> "riot.cache.gc.failed"
  | CacheForceCleanStarted _ -> "riot.cache.force_clean.started"
  | CacheForceCleanCompleted _ -> "riot.cache.force_clean.completed"
  | CacheForceCleanFailed _ -> "riot.cache.force_clean.failed"

let build_event_name = fun __tmp1 ->
  match __tmp1 with
  | BuildStarted _ -> "riot.build.started"
  | BuildCompleted _ -> "riot.build.completed"
  | BuildGraphCreating -> "riot.build.graph.creating"
  | BuildGraphCreated _ -> "riot.build.graph.created"
  | BuildPackageStarted _ -> "riot.build.package.started"
  | BuildPackageCompleted _ -> "riot.build.package.completed"
  | BuildPackageSkipped _ -> "riot.build.package.skipped"
  | BuildCompileError _ -> "riot.build.compile.error"
  | BuildCompilingInterface _ -> "riot.build.compile.interface"
  | BuildCompilingImplementation _ -> "riot.build.compile.implementation"
  | BuildLinkingLibrary _ -> "riot.build.link.library"
  | BuildLinkingExecutable _ -> "riot.build.link.executable"
  | BuildComputingHash _ -> "riot.build.hash.computing"
  | BuildHashComputed _ -> "riot.build.hash.computed"
  | BuildCopyingFile _ -> "riot.build.file.copy"
  | BuildWritingFile _ -> "riot.build.file.write"
  | BuildCreatingDirectory _ -> "riot.build.directory.create"
  | BuildDependencyMissing _ -> "riot.build.dependency.missing"
  | BuildDependencySatisfied _ -> "riot.build.dependency.satisfied"
  | BuildCycleDetected _ -> "riot.build.cycle.detected"
  | BuildQueuePackage _ -> "riot.build.queue.package"
  | BuildQueueStats _ -> "riot.build.queue.stats"
  | BuildWorkerStarted _ -> "riot.build.worker.started"
  | BuildWorkerAssigned _ -> "riot.build.worker.assigned"
  | BuildWorkerIdle _ -> "riot.build.worker.idle"
  | BuildWorkerPoolCreating _ -> "riot.build.worker_pool.creating"
  | BuildWorkerPoolCreated _ -> "riot.build.worker_pool.created"
  | BuildWorkerPoolStarted _ -> "riot.build.worker_pool.started"
  | BuildTargetBuilding _ -> "riot.build.target.building"
  | BuildPackageCompilationStarted _ -> "riot.build.package.compilation.started"
  | BuildSandboxCreated _ -> "riot.build.sandbox.created"
  | BuildSandboxInputsCopied _ -> "riot.build.sandbox.inputs.copied"
  | BuildSandboxDependenciesCopied _ -> "riot.build.sandbox.dependencies.copied"
  | BuildPackageExecutionPrepared _ -> "riot.build.package.execution.prepared"
  | BuildPackageWarnings _ -> "riot.build.package.warnings"
  | BuildPackageFinished _ -> "riot.build.package.finished"
  | BuildPackageFailed _ -> "riot.build.package.failed"
  | BuildPackageSkippedDetailed _ -> "riot.build.package.skipped"
  | BuildActionStarted _ -> "riot.build.action.started"
  | BuildActionCommandStarted _ -> "riot.build.action.command.started"
  | BuildActionCompleted _ -> "riot.build.action.completed"
  | BuildActionFailed _ -> "riot.build.action.failed"
  | BuildActionCacheHit _ -> "riot.build.action.cache.hit"
  | BuildActionCacheMiss _ -> "riot.build.action.cache.miss"
  | BuildPhase phase -> "riot.build.phase." ^ build_runtime_phase_event_suffix phase
  | BuildCommandFinished _ -> "riot.build.command.finished"

let test_event_name = fun __tmp1 ->
  match __tmp1 with
  | TestNoSuitesFound _ -> "riot.test.suites.none_found"
  | TestSuitesCollected _ -> "riot.test.suites.collected"
  | TestResolvingSuiteBinary _ -> "riot.test.suite.binary.resolving"
  | TestSuiteBinaryResolved _ -> "riot.test.suite.binary.resolved"
  | TestRunningSuite _ -> "riot.test.suite.running"
  | TestExecutingSuiteBinary _ -> "riot.test.suite.binary.executing"
  | TestSuiteHeartbeat _ -> "riot.test.suite.heartbeat"
  | TestSuiteBinaryFinished _ -> "riot.test.suite.binary.finished"
  | TestSuiteProgress _ -> "riot.test.suite.progress"
  | TestParsingSuiteOutput _ -> "riot.test.suite.output.parsing"
  | TestSuiteCompleted _ -> "riot.test.suite.completed"
  | TestSummary _ -> "riot.test.summary"

let workspace_event_name = fun __tmp1 ->
  match __tmp1 with
  | WorkspaceEmpty -> "riot.workspace.empty"
  | WorkspaceScanning -> "riot.workspace.scanning"
  | WorkspaceScanned _ -> "riot.workspace.scanned"

let server_event_name = fun __tmp1 ->
  match __tmp1 with
  | ServerStarted _ -> "riot.server.started"
  | ServerScanning _ -> "riot.server.scanning"
  | ServerRestarted _ -> "riot.server.restarted"
  | ServerShutdown -> "riot.server.shutdown"

let rpc_event_name = fun __tmp1 ->
  match __tmp1 with
  | RpcRequestReceived _ -> "riot.rpc.request.received"
  | RpcResponseSent _ -> "riot.rpc.response.sent"

let mcp_event_name = fun __tmp1 ->
  match __tmp1 with
  | McpToolCall _ -> "riot.mcp.tool_call"

let command_event_name = fun __tmp1 ->
  match __tmp1 with
  | CommandBinaryRunning _ -> "riot.command.binary.running"
  | CommandBinaryInstalling _ -> "riot.command.binary.installing"
  | CommandBinaryPromoted _ -> "riot.command.binary.promoted"
  | CommandBinaryInstalled _ -> "riot.command.binary.installed"
  | CommandError error -> "riot.command.error." ^ error.kind

let name = fun __tmp1 ->
  match __tmp1 with
  | Build event -> build_event_name event
  | Cache event -> cache_event_name event
  | Deps event -> deps_event_name event
  | Test event -> test_event_name event
  | Workspace event -> workspace_event_name event
  | Server event -> server_event_name event
  | Rpc event -> rpc_event_name event
  | Mcp event -> mcp_event_name event
  | Command event -> command_event_name event
  | Unknown event -> event.event

let package_name = Package_name.to_string

let package_list = fun packages -> String.concat ", " (List.map packages ~fn:package_name)

let cache_gc_trigger_to_string = fun __tmp1 ->
  match __tmp1 with
  | Manual -> "manual"
  | Post_build -> "post_build"

let scaled_size_string = fun bytes divisor suffix ->
  let whole = Int64.div bytes divisor in
  let remainder = Int64.rem bytes divisor in
  let fraction = Int64.div (Int64.mul remainder 10L) divisor in
  Int64.to_string whole ^ "." ^ Int64.to_string fraction ^ " " ^ suffix

let size_to_string = fun size_bytes ->
  let kib = 1_024L in
  let mib = Int64.mul kib 1_024L in
  let gib = Int64.mul mib 1_024L in
  let tib = Int64.mul gib 1_024L in
  if Int64.compare size_bytes tib != Order.LT then
    scaled_size_string size_bytes tib "TiB"
  else if Int64.compare size_bytes gib != Order.LT then
    scaled_size_string size_bytes gib "GiB"
  else if Int64.compare size_bytes mib != Order.LT then
    scaled_size_string size_bytes mib "MiB"
  else if Int64.compare size_bytes kib != Order.LT then
    scaled_size_string size_bytes kib "KiB"
  else
    Int64.to_string size_bytes ^ " B"

let cache_gc_summary_message = fun summary ->
  if not summary.ran_gc then
    "tracked cache already within policy (" ^ size_to_string summary.size_after_bytes ^ ")"
  else
    "removed "
    ^ Int.to_string summary.deleted_entries
    ^ " cache entries and "
    ^ Int.to_string summary.deleted_generations
    ^ " generations ("
    ^ size_to_string summary.size_before_bytes
    ^ " -> "
    ^ size_to_string summary.size_after_bytes
    ^ ")"

let build_artifact_status_to_string = fun __tmp1 ->
  match __tmp1 with
  | Fresh -> "fresh"
  | Cached -> "cached"

let build_warning_source_to_string = fun __tmp1 ->
  match __tmp1 with
  | FreshWarning -> "fresh"
  | CachedWarning -> "cached"

let build_package_error_message = fun __tmp1 ->
  match __tmp1 with
  | BuildPlanningFailed { message } -> message
  | BuildExecutionFailed { message } -> message
  | BuildActionExecutionFailed { message } -> message
  | BuildActionOutputsNotCreated { missing } ->
      "missing outputs: " ^ String.concat ", " (List.map missing ~fn:Path.to_string)
  | BuildActionDependenciesFailed { failed } -> "failed dependencies: " ^ String.concat ", " failed

let display_build_event = fun __tmp1 ->
  match __tmp1 with
  | BuildStarted { packages; _ } ->
      "Build started for " ^ Int.to_string (List.length packages) ^ " packages"
  | BuildCompleted { duration_ms; succeeded; failed; _ } ->
      "Build completed in "
      ^ Int.to_string duration_ms
      ^ "ms ("
      ^ Int.to_string (List.length succeeded)
      ^ " succeeded, "
      ^ Int.to_string (List.length failed)
      ^ " failed)"
  | BuildGraphCreating -> "Creating build graph"
  | BuildGraphCreated { nodes; duration_ms } ->
      "Created build graph with "
      ^ Int.to_string nodes
      ^ " nodes in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | BuildPackageStarted { package } -> "Building " ^ package_name package
  | BuildPackageCompleted { package; success; duration_ms; _ } ->
      if success then
        "Built " ^ package_name package ^ " in " ^ Int.to_string duration_ms ^ "ms"
      else
        "Failed to build " ^ package_name package
  | BuildPackageSkipped { package; reason } ->
      let reason =
        match reason with
        | DependenciesFailed deps -> "dependencies failed: " ^ package_list deps
      in
      "Skipped " ^ package_name package ^ " (" ^ reason ^ ")"
  | BuildCompileError { package; error } ->
      let (col_start, _) = error.span in
      let kind =
        match error.kind with
        | SyntaxError -> "Syntax error"
        | TypeError { description } -> description
        | UnboundValue { name } -> "Unbound value " ^ name
        | UnboundModule { name } -> "Unbound module " ^ name
        | FileNotFound { filename } -> "Cannot find file " ^ filename
        | OtherError { message } -> message
      in
      package_name package
      ^ " "
      ^ error.file
      ^ ":"
      ^ Int.to_string error.line
      ^ ":"
      ^ Int.to_string col_start
      ^ ": "
      ^ kind
  | BuildCompilingInterface { package; file } ->
      "[" ^ package_name package ^ "] Compiling interface " ^ file
  | BuildCompilingImplementation { package; file } ->
      "[" ^ package_name package ^ "] Compiling " ^ file
  | BuildLinkingLibrary { package; output } ->
      "[" ^ package_name package ^ "] Linking library " ^ output
  | BuildLinkingExecutable { package; output } ->
      "[" ^ package_name package ^ "] Linking executable " ^ output
  | BuildComputingHash { package } -> "Computing hash for " ^ package_name package
  | BuildHashComputed { package; hash } -> "Hash for " ^ package_name package ^ ": " ^ hash
  | BuildCopyingFile { source; dest } -> "Copying " ^ source ^ " -> " ^ dest
  | BuildWritingFile { path } -> "Writing " ^ path
  | BuildCreatingDirectory { path } -> "Creating directory " ^ path
  | BuildDependencyMissing { package; missing } ->
      package_name package ^ " waiting for " ^ package_list missing
  | BuildDependencySatisfied { package } -> package_name package ^ " dependencies satisfied"
  | BuildCycleDetected { packages } -> "Circular dependency detected: " ^ package_list packages
  | BuildQueuePackage { package; queue } ->
      let queue =
        match queue with
        | `Ready -> "ready"
        | `Waiting -> "waiting"
      in
      "Queued " ^ package_name package ^ " (" ^ queue ^ ")"
  | BuildQueueStats { ready; waiting; busy } ->
      "Queue: "
      ^ Int.to_string ready
      ^ " ready, "
      ^ Int.to_string waiting
      ^ " waiting, "
      ^ Int.to_string busy
      ^ " busy"
  | BuildWorkerStarted { worker_id } -> "Worker " ^ Worker_id.to_string worker_id ^ " started"
  | BuildWorkerAssigned { worker_id; package } ->
      "Worker " ^ Worker_id.to_string worker_id ^ " assigned to " ^ package_name package
  | BuildWorkerIdle { worker_id } -> "Worker " ^ Worker_id.to_string worker_id ^ " idle"
  | BuildWorkerPoolCreating { workers } ->
      "Creating worker pool with " ^ Int.to_string workers ^ " workers"
  | BuildWorkerPoolCreated { workers; duration_ms } ->
      "Worker pool created with "
      ^ Int.to_string workers
      ^ " workers in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | BuildWorkerPoolStarted { workers } ->
      "Started worker pool with " ^ Int.to_string workers ^ " workers"
  | BuildTargetBuilding { target; host } ->
      if host then
        "Building host target " ^ Target.to_string target
      else
        "Building target " ^ Target.to_string target
  | BuildPackageCompilationStarted { package; action_count; _ } ->
      "Building " ^ package_name package.name ^ " (" ^ Int.to_string action_count ^ " actions)"
  | BuildSandboxCreated { package; path; _ } ->
      "Created build sandbox for " ^ package_name package.name ^ " at " ^ Path.to_string path
  | BuildSandboxInputsCopied { package; input_count; _ } ->
      "Copied " ^ Int.to_string input_count ^ " sandbox inputs for " ^ package_name package.name
  | BuildSandboxDependenciesCopied { package; dependency_count; object_count; _ } ->
      "Copied "
      ^ Int.to_string object_count
      ^ " dependency objects from "
      ^ Int.to_string dependency_count
      ^ " dependencies for "
      ^ package_name package.name
  | BuildPackageExecutionPrepared { package; _ } ->
      "Prepared build execution for " ^ package_name package.name
  | BuildPackageWarnings { package; messages; _ } ->
      "Compiler warnings for "
      ^ package_name package.name
      ^ " ("
      ^ Int.to_string (List.length messages)
      ^ ")"
  | BuildPackageFinished { package; status; duration; _ } ->
      "Built "
      ^ package_name package.name
      ^ " ("
      ^ build_artifact_status_to_string status
      ^ ") in "
      ^ Time.Duration.to_secs_string ~precision:2 duration
      ^ "s"
  | BuildPackageFailed { package; error; _ } ->
      "Failed to build " ^ package_name package.name ^ ": " ^ build_package_error_message error
  | BuildPackageSkippedDetailed { package; reason; _ } ->
      "Skipped " ^ package_name package.name ^ ": " ^ reason
  | BuildActionStarted { package; action_label; _ } ->
      "[" ^ package_name package.name ^ "] " ^ action_label
  | BuildActionCommandStarted { package; action_label; command; _ } ->
      "[" ^ package_name package.name ^ "] " ^ action_label ^ ": " ^ command
  | BuildActionCompleted {
      package;
      action_label;
      status;
      duration;
      _;
    } ->
      "["
      ^ package_name package.name
      ^ "] "
      ^ action_label
      ^ " "
      ^ build_artifact_status_to_string status
      ^ " in "
      ^ Time.Duration.to_secs_string ~precision:2 duration
      ^ "s"
  | BuildActionFailed { package; action_label; error; _ } ->
      "[" ^ package_name package.name ^ "] " ^ action_label ^ " failed: " ^ error
  | BuildActionCacheHit { package; action_label; _ } ->
      "[" ^ package_name package.name ^ "] cache hit for " ^ action_label
  | BuildActionCacheMiss { package; action_label; _ } ->
      "[" ^ package_name package.name ^ "] cache miss for " ^ action_label
  | BuildPhase phase -> "Build phase: " ^ build_runtime_phase_name phase
  | BuildCommandFinished {
      duration;
      built_count;
      cached_count;
      failed_count;
      skipped_count;
    } ->
      "Build finished in "
      ^ Time.Duration.to_secs_string ~precision:2 duration
      ^ "s (built="
      ^ Int.to_string built_count
      ^ ", cached="
      ^ Int.to_string cached_count
      ^ ", skipped="
      ^ Int.to_string skipped_count
      ^ ", failed="
      ^ Int.to_string failed_count
      ^ ")"

let display_cache_event = fun __tmp1 ->
  match __tmp1 with
  | CacheBuildHit { package; _ } -> "Build cache hit for " ^ package_name package
  | CacheBuildMiss { package; _ } -> "Build cache miss for " ^ package_name package
  | CacheBuildStored { package; artifacts; _ } ->
      "Stored build cache for "
      ^ package_name package
      ^ " ("
      ^ Int.to_string (List.length artifacts)
      ^ " artifacts)"
  | CacheStoreCreating -> "Creating cache store"
  | CacheStoreCreated { duration_ms } ->
      "Created cache store in " ^ Int.to_string duration_ms ^ "ms"
  | CacheGcStarted { trigger } -> "Cache GC started (" ^ cache_gc_trigger_to_string trigger ^ ")"
  | CacheGcCacheScanStarted { build_root; _ } ->
      "Scanning cache entries under " ^ Path.to_string build_root
  | CacheGcCacheEntryScanStarted { hash; _ } -> "Scanning cache entry " ^ hash
  | CacheGcCacheEntryScanned { hash; size_bytes; _ } ->
      "Scanned cache entry " ^ hash ^ " (" ^ size_to_string size_bytes ^ ")"
  | CacheGcCacheScanCompleted { entry_count; total_size_bytes; _ } ->
      "Scanned "
      ^ Int.to_string entry_count
      ^ " cache entries ("
      ^ size_to_string total_size_bytes
      ^ ")"
  | CacheGcPlanComputed { deleted_entries; deleted_generations; reclaimable_bytes; _ } ->
      "Cache GC will remove "
      ^ Int.to_string deleted_entries
      ^ " entries and "
      ^ Int.to_string deleted_generations
      ^ " generations ("
      ^ size_to_string reclaimable_bytes
      ^ ")"
  | CacheGcCacheEntryDeleteStarted { hash; _ } -> "Removing cache entry " ^ hash
  | CacheGcGenerationDeleteStarted { path; _ } -> "Removing cache generation " ^ Path.to_string path
  | CacheGcSkipped { summary; _ } -> "Cache GC skipped: " ^ cache_gc_summary_message summary
  | CacheGcCompleted { summary; _ } -> "Cache GC completed: " ^ cache_gc_summary_message summary
  | CacheGcFailed { error; _ } -> "Cache GC failed: " ^ error
  | CacheForceCleanStarted { build_root } -> "Removing build root " ^ Path.to_string build_root
  | CacheForceCleanCompleted { build_root } -> "Removed build root " ^ Path.to_string build_root
  | CacheForceCleanFailed { build_root; error } ->
      "Failed to remove build root " ^ Path.to_string build_root ^ ": " ^ error

let display_deps_event = fun __tmp1 ->
  match __tmp1 with
  | DepsLockfileReadStarted { path } -> "Reading lockfile " ^ path
  | DepsLockfileReadFinished { path; duration_ms } ->
      "Read lockfile " ^ path ^ " in " ^ Int.to_string duration_ms ^ "ms"
  | DepsLockfileReadFailed { path; error } ->
      "Failed to read lockfile " ^ path ^ ": " ^ Pm_error.message error
  | DepsLockfileWriteStarted { path } -> "Writing lockfile " ^ path
  | DepsLockfileWriteFinished { path; duration_ms } ->
      "Wrote lockfile " ^ path ^ " in " ^ Int.to_string duration_ms ^ "ms"
  | DepsLockfileWriteFailed { path; error } ->
      "Failed to write lockfile " ^ path ^ ": " ^ Pm_error.message error
  | DepsResolutionStarted { packages; mode } ->
      let mode =
        match mode with
        | `Refresh -> "refresh"
        | `Unlock -> "unlock"
      in
      "Resolving dependencies ("
      ^ mode
      ^ ") for "
      ^ Int.to_string (List.length packages)
      ^ " packages"
  | DepsResolutionUsingExistingLock { path } -> "Using existing lockfile " ^ path
  | DepsResolutionRefreshingLock { path } -> "Refreshing lockfile " ^ path
  | DepsResolutionUnlocking { path } -> (
      match path with
      | Some path -> "Unlocking dependency graph from " ^ path
      | None -> "Unlocking dependency graph"
    )
  | DepsResolutionFinished { duration_ms; resolved_packages; resolved_edges } ->
      "Resolved "
      ^ Int.to_string resolved_packages
      ^ " packages and "
      ^ Int.to_string resolved_edges
      ^ " edges in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | DepsResolutionFailed { error } -> "Dependency resolution failed: " ^ Pm_error.message error
  | DepsRegistryIndexUpdating { registry } -> "Updating " ^ registry ^ " index"
  | DepsUniverseBuilding { packages } ->
      "Building dependency universe for " ^ Int.to_string (List.length packages) ^ " packages"
  | DepsUniverseBuilt {
      runtime_packages;
      build_packages;
      dev_packages;
      duration_ms;
    } ->
      "Built dependency universe in "
      ^ Int.to_string duration_ms
      ^ "ms (runtime="
      ^ Int.to_string runtime_packages
      ^ ", build="
      ^ Int.to_string build_packages
      ^ ", dev="
      ^ Int.to_string dev_packages
      ^ ")"
  | DepsPackageMetadataFetchStarted { registry; package } ->
      "Fetching metadata for " ^ package_name package ^ " from " ^ registry
  | DepsPackageMetadataFetchFinished {
      registry;
      package;
      version;
      duration_ms;
    } ->
      let package =
        match version with
        | Some version -> package_name package ^ "@" ^ version
        | None -> package_name package
      in
      "Fetched metadata for "
      ^ package
      ^ " from "
      ^ registry
      ^ " in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | DepsPackageMetadataFetchFailed { registry; package; error } ->
      "Failed to fetch metadata for "
      ^ package_name package
      ^ " from "
      ^ registry
      ^ ": "
      ^ Pm_error.message error
  | DepsSourceMaterializationStarted { source_locator; ref_ } -> (
      match ref_ with
      | Some ref_ -> "Materializing source dependency " ^ source_locator ^ "#" ^ ref_
      | None -> "Materializing source dependency " ^ source_locator
    )
  | DepsSourceMaterializationFinished { package; version; _ } -> (
      match version with
      | Some version -> "Discovered source dependency " ^ package_name package ^ "@" ^ version
      | None -> "Discovered source dependency " ^ package_name package
    )
  | DepsManifestUpdated {
      path;
      section;
      operation;
      dependency;
    } ->
      let verb =
        match operation with
        | `Add -> "Added"
        | `Remove -> "Removed"
      in
      verb ^ " " ^ dependency ^ " (" ^ section ^ ") in " ^ path
  | DepsPackageVersionLocked { package; version } ->
      "Locked " ^ package_name package ^ " (" ^ version ^ ")"
  | DepsPackageVersionsUnchanged { packages } ->
      "Dependencies are already up to date ("
      ^ Int.to_string packages
      ^ " locked packages unchanged)"
  | DepsPackageVersionUpdated { package; from_version; to_version } ->
      "Updated " ^ package_name package ^ " (" ^ from_version ^ " -> " ^ to_version ^ ")"
  | DepsPackageManifestFetchStarted { package; version } ->
      "Fetching manifest for " ^ package_name package ^ "@" ^ version
  | DepsPackageManifestFetchFinished { package; version; duration_ms } ->
      "Fetched manifest for "
      ^ package_name package
      ^ "@"
      ^ version
      ^ " in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | DepsPackageManifestFetchFailed { package; version; error } ->
      let package =
        match version with
        | Some version -> package_name package ^ "@" ^ version
        | None -> package_name package
      in
      "Failed to fetch manifest for " ^ package ^ ": " ^ Pm_error.message error
  | DepsPackageDownloadQueued { package; version; _ } ->
      "Queued download for " ^ package_name package ^ "@" ^ version
  | DepsPackageDownloadStarted { package; version; _ } ->
      "Downloading " ^ package_name package ^ "@" ^ version
  | DepsPackageDownloadFinished {
      package;
      version;
      path;
      duration_ms;
    } ->
      "Downloaded "
      ^ package_name package
      ^ "@"
      ^ version
      ^ " to "
      ^ path
      ^ " in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | DepsPackageDownloadFailed { package; version; error; _ } ->
      "Failed to download " ^ package_name package ^ "@" ^ version ^ ": " ^ Pm_error.message error
  | DepsPackageDownloadSkipped { package; version; reason; _ } ->
      "Skipped download for " ^ package_name package ^ "@" ^ version ^ " (" ^ reason ^ ")"
  | DepsPackageCacheHit { package; version; path } ->
      "Package cache hit for " ^ package_name package ^ "@" ^ version ^ " at " ^ path
  | DepsPackageMaterializationStarted { package; version; _ } ->
      "Materializing " ^ package_name package ^ "@" ^ version
  | DepsPackageMaterializationFinished {
      package;
      version;
      path;
      duration_ms;
    } ->
      "Materialized "
      ^ package_name package
      ^ "@"
      ^ version
      ^ " at "
      ^ path
      ^ " in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | DepsPackageMaterializationFailed { package; version; error; _ } ->
      "Failed to materialize "
      ^ package_name package
      ^ "@"
      ^ version
      ^ ": "
      ^ Pm_error.message error
  | DepsPackageResolvedForBuild {
      package;
      version;
      path;
      workspace;
    } ->
      let package =
        match version with
        | Some version -> package_name package ^ "@" ^ version
        | None -> package_name package
      in
      "Resolved " ^ package ^ " for build at " ^ path ^ if workspace then
        " (workspace)"
      else
        ""

let display_test_suite = fun suite -> package_name suite.package_name ^ "/" ^ suite.suite_name

let display_test_event = fun __tmp1 ->
  match __tmp1 with
  | TestNoSuitesFound _ -> "No test suites found"
  | TestSuitesCollected { suite_count; _ } ->
      "Collected " ^ Int.to_string suite_count ^ " test suites"
  | TestResolvingSuiteBinary suite -> "Resolving test suite " ^ display_test_suite suite
  | TestSuiteBinaryResolved { suite; binary_path } ->
      "Resolved test suite " ^ display_test_suite suite ^ " at " ^ Path.to_string binary_path
  | TestRunningSuite suite -> "Running test suite " ^ display_test_suite suite
  | TestExecutingSuiteBinary { suite; binary_path; _ } ->
      "Executing test suite " ^ display_test_suite suite ^ " at " ^ Path.to_string binary_path
  | TestSuiteHeartbeat { suite; elapsed_us; _ } ->
      "Test suite "
      ^ display_test_suite suite
      ^ " still running after "
      ^ Int.to_string elapsed_us
      ^ "us"
  | TestSuiteBinaryFinished { suite; status; _ } ->
      "Test suite " ^ display_test_suite suite ^ " exited with status " ^ Int.to_string status
  | TestSuiteProgress { suite; _ } -> "Test suite progress from " ^ display_test_suite suite
  | TestParsingSuiteOutput { suite; _ } ->
      "Parsing test suite output from " ^ display_test_suite suite
  | TestSuiteCompleted { suite; summary; _ } ->
      "Completed test suite "
      ^ display_test_suite suite
      ^ " ("
      ^ Int.to_string summary.passed
      ^ " passed, "
      ^ Int.to_string summary.failed
      ^ " failed, "
      ^ Int.to_string summary.skipped
      ^ " skipped)"
  | TestSummary {
      total;
      passed;
      failed;
      skipped;
      _;
    } ->
      "Test summary: "
      ^ Int.to_string passed
      ^ "/"
      ^ Int.to_string total
      ^ " passed, "
      ^ Int.to_string failed
      ^ " failed, "
      ^ Int.to_string skipped
      ^ " skipped"

let display_workspace_event = fun __tmp1 ->
  match __tmp1 with
  | WorkspaceEmpty -> "No packages found in workspace"
  | WorkspaceScanning -> "Scanning workspace"
  | WorkspaceScanned { packages; duration_ms } ->
      "Scanned workspace: "
      ^ Int.to_string packages
      ^ " packages in "
      ^ Int.to_string duration_ms
      ^ "ms"

let display_server_event = fun __tmp1 ->
  match __tmp1 with
  | ServerStarted { pid } -> "Server started (pid: " ^ pid ^ ")"
  | ServerScanning { root } -> "Scanning workspace: " ^ root
  | ServerRestarted { packages; toolchain } ->
      "Server restarted with " ^ Int.to_string packages ^ " packages (toolchain: " ^ toolchain ^ ")"
  | ServerShutdown -> "Server shutting down"

let display_rpc_event = fun __tmp1 ->
  match __tmp1 with
  | RpcRequestReceived { request_type; _ } -> "RPC request: " ^ request_type
  | RpcResponseSent { result } ->
      "RPC response sent (success: " ^ Bool.to_string
        (
          match result with
          | Ok _ -> true
          | Error _ -> false
        ) ^ ")"

let display_mcp_event = fun __tmp1 ->
  match __tmp1 with
  | McpToolCall { tool; _ } -> "MCP tool call: " ^ tool

let display_command_event = fun __tmp1 ->
  match __tmp1 with
  | CommandBinaryRunning { package; binary; _ } -> "Running " ^ package_name package ^ ":" ^ binary
  | CommandBinaryInstalling { package; binary } ->
      "Installing " ^ package_name package ^ ":" ^ binary
  | CommandBinaryPromoted { binary; destination; _ } ->
      "Promoted " ^ binary ^ " to " ^ Path.to_string destination
  | CommandBinaryInstalled { binary; duration_ms; _ } ->
      "Installed " ^ binary ^ " in " ^ Int.to_string duration_ms ^ "ms"
  | CommandError error -> error.message

let display = fun __tmp1 ->
  match __tmp1 with
  | Build event -> display_build_event event
  | Cache event -> display_cache_event event
  | Deps event -> display_deps_event event
  | Test event -> display_test_event event
  | Workspace event -> display_workspace_event event
  | Server event -> display_server_event event
  | Rpc event -> display_rpc_event event
  | Mcp event -> display_mcp_event event
  | Command event -> display_command_event event
  | Unknown event -> (
      match event.message with
      | Some message -> message
      | None -> event.event
    )

let to_string = fun event ->
  let timestamp = DateTime.to_iso8601 event.timestamp in
  let level =
    match event.level with
    | Error -> "[ERROR] "
    | Warn -> "[WARN] "
    | Info -> ""
    | Debug -> "[DEBUG] "
    | Trace -> "[TRACE] "
  in
  "[" ^ timestamp ^ "] " ^ level ^ display event.kind

let compile_error_kind_json = fun __tmp1 ->
  match __tmp1 with
  | SyntaxError -> Json.Object [ ("type", Json.String "syntax") ]
  | TypeError { description } ->
      Json.Object [ ("type", Json.String "type"); ("description", Json.String description) ]
  | UnboundValue { name } ->
      Json.Object [ ("type", Json.String "unbound_value"); ("name", Json.String name) ]
  | UnboundModule { name } ->
      Json.Object [ ("type", Json.String "unbound_module"); ("name", Json.String name) ]
  | FileNotFound { filename } ->
      Json.Object [ ("type", Json.String "file_not_found"); ("filename", Json.String filename) ]
  | OtherError { message } ->
      Json.Object [ ("type", Json.String "other"); ("message", Json.String message) ]

let compile_error_json = fun error ->
  let (start_, end_) = error.span in
  Json.Object [
    ("file", Json.String error.file);
    ("line", Json.Int error.line);
    ("span", Json.Array [ Json.Int start_; Json.Int end_ ]);
    ("kind", compile_error_kind_json error.kind);
    ("hint", Json.String (strip_ansi_codes error.hint));
    ("raw", Json.String (strip_ansi_codes error.raw));
  ]

let build_result_json = fun result ->
  Json.Object [
    ("package", package_name_json result.package);
    ("success", Json.Bool result.success);
    ("duration_ms", Json.Int result.duration_ms);
    ("modules_compiled", Json.Int result.modules_compiled);
    ("cache_hits", Json.Int result.cache_hits);
    ("cache_misses", Json.Int result.cache_misses);
    ("errors", Json.Array (List.map result.errors ~fn:compile_error_json));
  ]

let skip_reason_json = fun __tmp1 ->
  match __tmp1 with
  | DependenciesFailed deps ->
      Json.Object [
        ("type", Json.String "dependencies_failed");
        ("dependencies", package_names_json deps);
      ]

let package_json = fun package -> Json.Object [ ("name", package_name_json package.Package.name) ]

let build_artifact_status_json = fun status -> Json.String (build_artifact_status_to_string status)

let build_warning_source_json = fun source -> Json.String (build_warning_source_to_string source)

let build_package_error_json = fun __tmp1 ->
  match __tmp1 with
  | BuildPlanningFailed { message } ->
      Json.Object [ ("type", Json.String "planning_failed"); ("message", Json.String message) ]
  | BuildExecutionFailed { message } ->
      Json.Object [ ("type", Json.String "execution_failed"); ("message", Json.String message) ]
  | BuildActionExecutionFailed { message } ->
      Json.Object [
        ("type", Json.String "action_execution_failed");
        ("message", Json.String message);
      ]
  | BuildActionOutputsNotCreated { missing } ->
      Json.Object [
        ("type", Json.String "action_outputs_not_created");
        ("missing", Json.Array (List.map missing ~fn:path_json));
      ]
  | BuildActionDependenciesFailed { failed } ->
      Json.Object [
        ("type", Json.String "action_dependencies_failed");
        ("failed", strings_json failed);
      ]

let build_runtime_phase_fields = fun __tmp1 ->
  match __tmp1 with
  | TargetsResolved { target_count }
  | ToolchainsEnsured { target_count }
  | ToolchainsValidated { target_count } -> [ ("target_count", Json.Int target_count) ]
  | RuntimeStarting
  | RuntimeStarted -> []
  | BuildLockWaiting { lock_path } -> [ ("lock_path", path_json lock_path) ]
  | BuildLanesPreparationStarted { target_count; _ } -> [ ("target_count", Json.Int target_count) ]
  | BuildLanesPreparationFinished { lane_count; duration; _ } ->
      [
        ("lane_count", Json.Int lane_count);
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildUnitPlanCreated { unit_count; duration; _ } ->
      [
        ("unit_count", Json.Int unit_count);
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildLanePreparationStarted { target; _ } -> [ ("target", target_json target) ]
  | BuildLaneLockAcquired { target; duration; _ }
  | BuildLaneToolchainInitialized { target; duration; _ }
  | BuildLaneStoreCreated { target; duration; _ }
  | BuildLanePreparationFinished { target; duration; _ } ->
      [
        ("target", target_json target);
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
      ]
  | PackagePlanningStarted { lane_count; package_count }
  | PackageExecutionStarted { lane_count; package_count } ->
      [ ("lane_count", Json.Int lane_count); ("package_count", Json.Int package_count) ]
  | PackagePlanStarted { package; build_target; source_count; _ } ->
      [
        ("package", package_json package);
        ("target", target_json build_target);
        ("source_count", Json.Int source_count);
      ]
  | PackagePlanSourceStarted {
      package;
      build_target;
      source;
      source_index;
      source_count;
      _;
    } ->
      [
        ("package", package_json package);
        ("target", target_json build_target);
        ("source", path_json source);
        ("source_index", Json.Int source_index);
        ("source_count", Json.Int source_count);
      ]
  | PackagePlanFinished {
      package;
      build_target;
      source_count;
      duration;
      _;
    } ->
      [
        ("package", package_json package);
        ("target", target_json build_target);
        ("source_count", Json.Int source_count);
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
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
        ("lane_count", Json.Int lane_count);
        ("package_count", Json.Int package_count);
        ("deferred_count", Json.Int deferred_count);
        ("execution_required_count", Json.Int execution_required_count);
        ("finalized_count", Json.Int finalized_count);
        ("cached_count", Json.Int cached_count);
        ("skipped_count", Json.Int skipped_count);
        ("failed_count", Json.Int failed_count);
        ("error_count", Json.Int error_count);
      ]
  | PackageActionGraphPlanned { package; build_target; action_count; _ } ->
      [
        ("package", package_json package);
        ("target", target_json build_target);
        ("action_count", Json.Int action_count);
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
        ("lane_count", Json.Int lane_count);
        ("package_count", Json.Int package_count);
        ("finalized_count", Json.Int finalized_count);
        ("built_count", Json.Int built_count);
        ("failed_count", Json.Int failed_count);
        ("error_count", Json.Int error_count);
      ]
  | TargetBuildStarted { target; host } ->
      [ ("target", target_json target); ("host", Json.Bool host) ]
  | TargetBuildFinished { target; result_count; had_partial_failure } ->
      [
        ("target", target_json target);
        ("result_count", Json.Int result_count);
        ("had_partial_failure", Json.Bool had_partial_failure);
      ]
  | CacheGenerationRecordingStarted { lane_count; new_entry_count }
  | CacheGenerationRecorded { lane_count; new_entry_count } ->
      [ ("lane_count", Json.Int lane_count); ("new_entry_count", Json.Int new_entry_count) ]
  | ReturningResults { result_count; had_partial_failure } ->
      [
        ("result_count", Json.Int result_count);
        ("had_partial_failure", Json.Bool had_partial_failure);
      ]

let build_event_json = fun __tmp1 ->
  match __tmp1 with
  | BuildStarted { packages; total_modules; workers } ->
      Json.Object [
        ("packages", package_names_json packages);
        ("total_modules", Json.Int total_modules);
        ("workers", Json.Int workers);
      ]
  | BuildCompleted {
      duration_ms;
      results;
      succeeded;
      failed;
    } ->
      Json.Object [
        ("duration_ms", Json.Int duration_ms);
        ("results", Json.Array (List.map results ~fn:build_result_json));
        ("succeeded", package_names_json succeeded);
        ("failed", package_names_json failed);
      ]
  | BuildGraphCreating -> Json.Object []
  | BuildGraphCreated { nodes; duration_ms } ->
      Json.Object [ ("nodes", Json.Int nodes); ("duration_ms", Json.Int duration_ms) ]
  | BuildPackageStarted { package } -> Json.Object [ ("package", package_name_json package) ]
  | BuildPackageCompleted result -> build_result_json result
  | BuildPackageSkipped { package; reason } ->
      Json.Object [ ("package", package_name_json package); ("reason", skip_reason_json reason) ]
  | BuildCompileError { package; error } ->
      Json.Object [ ("package", package_name_json package); ("error", compile_error_json error) ]
  | BuildCompilingInterface { package; file }
  | BuildCompilingImplementation { package; file } ->
      Json.Object [ ("package", package_name_json package); ("file", Json.String file) ]
  | BuildLinkingLibrary { package; output }
  | BuildLinkingExecutable { package; output } ->
      Json.Object [ ("package", package_name_json package); ("output", Json.String output) ]
  | BuildComputingHash { package } -> Json.Object [ ("package", package_name_json package) ]
  | BuildHashComputed { package; hash } ->
      Json.Object [ ("package", package_name_json package); ("hash", Json.String hash) ]
  | BuildCopyingFile { source; dest } ->
      Json.Object [ ("source", Json.String source); ("dest", Json.String dest) ]
  | BuildWritingFile { path } -> Json.Object [ ("path", Json.String path) ]
  | BuildCreatingDirectory { path } -> Json.Object [ ("path", Json.String path) ]
  | BuildDependencyMissing { package; missing } ->
      Json.Object [
        ("package", package_name_json package);
        ("missing", package_names_json missing);
      ]
  | BuildDependencySatisfied { package } -> Json.Object [ ("package", package_name_json package) ]
  | BuildCycleDetected { packages } -> Json.Object [ ("packages", package_names_json packages) ]
  | BuildQueuePackage { package; queue } ->
      Json.Object [
        ("package", package_name_json package);
        ("queue", Json.String (
          match queue with
          | `Ready -> "ready"
          | `Waiting -> "waiting"
        ));
      ]
  | BuildQueueStats { ready; waiting; busy } ->
      Json.Object [
        ("ready", Json.Int ready);
        ("waiting", Json.Int waiting);
        ("busy", Json.Int busy);
      ]
  | BuildWorkerStarted { worker_id }
  | BuildWorkerIdle { worker_id } ->
      Json.Object [ ("worker_id", Json.String (Worker_id.to_string worker_id)) ]
  | BuildWorkerAssigned { worker_id; package } ->
      Json.Object [
        ("worker_id", Json.String (Worker_id.to_string worker_id));
        ("package", package_name_json package);
      ]
  | BuildWorkerPoolCreating { workers }
  | BuildWorkerPoolStarted { workers } -> Json.Object [ ("workers", Json.Int workers) ]
  | BuildWorkerPoolCreated { workers; duration_ms } ->
      Json.Object [ ("workers", Json.Int workers); ("duration_ms", Json.Int duration_ms) ]
  | BuildTargetBuilding { target; host } ->
      Json.Object [ ("target", target_json target); ("host", Json.Bool host) ]
  | BuildPackageCompilationStarted { package; build_target; action_count; _ } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("action_count", Json.Int action_count);
      ]
  | BuildSandboxCreated {
      package;
      build_target;
      path;
      duration;
      _;
    } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("path", path_json path);
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildSandboxInputsCopied {
      package;
      build_target;
      input_count;
      duration;
      _;
    } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("input_count", Json.Int input_count);
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildSandboxDependenciesCopied {
      package;
      build_target;
      dependency_count;
      object_count;
      duration;
      _;
    } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("dependency_count", Json.Int dependency_count);
        ("object_count", Json.Int object_count);
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildPackageExecutionPrepared {
      package;
      build_target;
      input_count;
      dependency_count;
      dependency_object_count;
      duration;
      _;
    } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("input_count", Json.Int input_count);
        ("dependency_count", Json.Int dependency_count);
        ("dependency_object_count", Json.Int dependency_object_count);
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildPackageWarnings {
      package;
      build_target;
      source;
      messages;
    } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("source", build_warning_source_json source);
        ("messages", strings_json messages);
      ]
  | BuildPackageFinished {
      package;
      build_target;
      status;
      duration;
    } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("status", build_artifact_status_json status);
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildPackageFailed { package; build_target; error } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("error", build_package_error_json error);
      ]
  | BuildPackageSkippedDetailed { package; build_target; reason } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("reason", Json.String reason);
      ]
  | BuildActionStarted {
      package;
      build_target;
      action_id;
      action_label;
      _;
    } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("action_id", Json.String action_id);
        ("action_label", Json.String action_label);
      ]
  | BuildActionCommandStarted {
      package;
      build_target;
      action_id;
      action_label;
      command;
      _;
    } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("action_id", Json.String action_id);
        ("action_label", Json.String action_label);
        ("command", Json.String command);
      ]
  | BuildActionCompleted {
      package;
      build_target;
      action_id;
      action_label;
      status;
      duration;
      _;
    } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("action_id", Json.String action_id);
        ("action_label", Json.String action_label);
        ("status", build_artifact_status_json status);
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
      ]
  | BuildActionFailed {
      package;
      build_target;
      action_id;
      action_label;
      error;
      _;
    } ->
      Json.Object [
        ("package", package_json package);
        ("target", target_json build_target);
        ("action_id", Json.String action_id);
        ("action_label", Json.String action_label);
        ("error", Json.String error);
      ]
  | BuildActionCacheHit {
      package;
      action_id;
      action_label;
      hash;
    }
  | BuildActionCacheMiss {
      package;
      action_id;
      action_label;
      hash;
    } ->
      Json.Object [
        ("package", package_json package);
        ("action_id", Json.String action_id);
        ("action_label", Json.String action_label);
        ("hash", Json.String hash);
      ]
  | BuildPhase phase ->
      Json.Object (("phase", Json.String (build_runtime_phase_name phase))
      :: build_runtime_phase_fields phase)
  | BuildCommandFinished {
      duration;
      built_count;
      cached_count;
      failed_count;
      skipped_count;
    } ->
      Json.Object [
        ("duration_ms", Json.Int (Time.Duration.to_millis duration));
        ("built_count", Json.Int built_count);
        ("cached_count", Json.Int cached_count);
        ("failed_count", Json.Int failed_count);
        ("skipped_count", Json.Int skipped_count);
      ]

let cache_summary_json = fun summary ->
  Json.Object [
    ("ran_gc", Json.Bool summary.ran_gc);
    ("kept_generations", Json.Int summary.kept_generations);
    ("deleted_generations", Json.Int summary.deleted_generations);
    ("deleted_entries", Json.Int summary.deleted_entries);
    ("size_before_bytes", Json.String (Int64.to_string summary.size_before_bytes));
    ("size_after_bytes", Json.String (Int64.to_string summary.size_after_bytes));
  ]

let cache_event_json = fun __tmp1 ->
  match __tmp1 with
  | CacheBuildHit { package; hash }
  | CacheBuildMiss { package; hash } ->
      Json.Object [ ("package", package_name_json package); ("hash", Json.String hash) ]
  | CacheBuildStored { package; hash; artifacts } ->
      Json.Object [
        ("package", package_name_json package);
        ("hash", Json.String hash);
        ("artifacts", strings_json artifacts);
      ]
  | CacheStoreCreating -> Json.Object []
  | CacheStoreCreated { duration_ms } -> Json.Object [ ("duration_ms", Json.Int duration_ms) ]
  | CacheGcStarted { trigger } -> Json.Object [ ("trigger", json_of_trigger trigger) ]
  | CacheGcCacheScanStarted { trigger; build_root } ->
      Json.Object [ ("trigger", json_of_trigger trigger); ("build_root", path_json build_root) ]
  | CacheGcCacheEntryScanStarted { trigger; hash; path } ->
      Json.Object [
        ("trigger", json_of_trigger trigger);
        ("hash", Json.String hash);
        ("path", path_json path);
      ]
  | CacheGcCacheEntryScanned {
      trigger;
      hash;
      path;
      size_bytes;
    } ->
      Json.Object [
        ("trigger", json_of_trigger trigger);
        ("hash", Json.String hash);
        ("path", path_json path);
        ("size_bytes", Json.String (Int64.to_string size_bytes));
      ]
  | CacheGcCacheScanCompleted { trigger; entry_count; total_size_bytes } ->
      Json.Object [
        ("trigger", json_of_trigger trigger);
        ("entry_count", Json.Int entry_count);
        ("total_size_bytes", Json.String (Int64.to_string total_size_bytes));
      ]
  | CacheGcPlanComputed {
      trigger;
      deleted_entries;
      deleted_generations;
      reclaimable_bytes;
    } ->
      Json.Object [
        ("trigger", json_of_trigger trigger);
        ("deleted_entries", Json.Int deleted_entries);
        ("deleted_generations", Json.Int deleted_generations);
        ("reclaimable_bytes", Json.String (Int64.to_string reclaimable_bytes));
      ]
  | CacheGcCacheEntryDeleteStarted {
      trigger;
      hash;
      path;
      size_bytes;
    } ->
      Json.Object [
        ("trigger", json_of_trigger trigger);
        ("hash", Json.String hash);
        ("path", path_json path);
        ("size_bytes", Json.String (Int64.to_string size_bytes));
      ]
  | CacheGcGenerationDeleteStarted { trigger; path } ->
      Json.Object [ ("trigger", json_of_trigger trigger); ("path", path_json path) ]
  | CacheGcSkipped { trigger; summary }
  | CacheGcCompleted { trigger; summary } ->
      Json.Object [ ("trigger", json_of_trigger trigger); ("summary", cache_summary_json summary) ]
  | CacheGcFailed { trigger; error } ->
      Json.Object [ ("trigger", json_of_trigger trigger); ("error", Json.String error) ]
  | CacheForceCleanStarted { build_root }
  | CacheForceCleanCompleted { build_root } -> Json.Object [ ("build_root", path_json build_root) ]
  | CacheForceCleanFailed { build_root; error } ->
      Json.Object [ ("build_root", path_json build_root); ("error", Json.String error) ]

let deps_event_json = fun __tmp1 ->
  match __tmp1 with
  | DepsLockfileReadStarted { path }
  | DepsLockfileWriteStarted { path }
  | DepsResolutionUsingExistingLock { path }
  | DepsResolutionRefreshingLock { path } -> Json.Object [ ("path", Json.String path) ]
  | DepsLockfileReadFinished { path; duration_ms }
  | DepsLockfileWriteFinished { path; duration_ms } ->
      Json.Object [ ("path", Json.String path); ("duration_ms", Json.Int duration_ms) ]
  | DepsLockfileReadFailed { path; error }
  | DepsLockfileWriteFailed { path; error } ->
      Json.Object [ ("path", Json.String path); ("error", Pm_error.to_json error) ]
  | DepsResolutionStarted { packages; mode } ->
      Json.Object [
        ("packages", package_names_json packages);
        ("mode", json_of_resolution_mode mode);
      ]
  | DepsResolutionUnlocking { path } -> Json.Object [ ("path", json_of_string_option path) ]
  | DepsResolutionFinished { duration_ms; resolved_packages; resolved_edges } ->
      Json.Object [
        ("duration_ms", Json.Int duration_ms);
        ("resolved_packages", Json.Int resolved_packages);
        ("resolved_edges", Json.Int resolved_edges);
      ]
  | DepsResolutionFailed { error } -> Json.Object [ ("error", Pm_error.to_json error) ]
  | DepsRegistryIndexUpdating { registry } -> Json.Object [ ("registry", Json.String registry) ]
  | DepsUniverseBuilding { packages } -> Json.Object [ ("packages", package_names_json packages) ]
  | DepsUniverseBuilt {
      runtime_packages;
      build_packages;
      dev_packages;
      duration_ms;
    } ->
      Json.Object [
        ("runtime_packages", Json.Int runtime_packages);
        ("build_packages", Json.Int build_packages);
        ("dev_packages", Json.Int dev_packages);
        ("duration_ms", Json.Int duration_ms);
      ]
  | DepsPackageMetadataFetchStarted { registry; package } ->
      Json.Object [ ("registry", Json.String registry); ("package", package_name_json package) ]
  | DepsPackageMetadataFetchFinished {
      registry;
      package;
      version;
      duration_ms;
    } ->
      Json.Object [
        ("registry", Json.String registry);
        ("package", package_name_json package);
        ("version", json_of_string_option version);
        ("duration_ms", Json.Int duration_ms);
      ]
  | DepsPackageMetadataFetchFailed { registry; package; error } ->
      Json.Object [
        ("registry", Json.String registry);
        ("package", package_name_json package);
        ("error", Pm_error.to_json error);
      ]
  | DepsSourceMaterializationStarted { source_locator; ref_ } ->
      Json.Object [
        ("source_locator", Json.String source_locator);
        ("ref", json_of_string_option ref_);
      ]
  | DepsSourceMaterializationFinished {
      source_locator;
      ref_;
      package;
      version;
    } ->
      Json.Object [
        ("source_locator", Json.String source_locator);
        ("ref", json_of_string_option ref_);
        ("package", package_name_json package);
        ("version", json_of_string_option version);
      ]
  | DepsManifestUpdated {
      path;
      section;
      operation;
      dependency;
    } ->
      Json.Object [
        ("path", Json.String path);
        ("section", Json.String section);
        ("operation", json_of_manifest_operation operation);
        ("dependency", Json.String dependency);
      ]
  | DepsPackageVersionLocked { package; version } ->
      Json.Object [ ("package", package_name_json package); ("version", Json.String version) ]
  | DepsPackageVersionsUnchanged { packages } -> Json.Object [ ("packages", Json.Int packages) ]
  | DepsPackageVersionUpdated { package; from_version; to_version } ->
      Json.Object [
        ("package", package_name_json package);
        ("from_version", Json.String from_version);
        ("to_version", Json.String to_version);
      ]
  | DepsPackageManifestFetchStarted { package; version } ->
      Json.Object [ ("package", package_name_json package); ("version", Json.String version) ]
  | DepsPackageManifestFetchFinished { package; version; duration_ms } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", Json.String version);
        ("duration_ms", Json.Int duration_ms);
      ]
  | DepsPackageManifestFetchFailed { package; version; error } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", json_of_string_option version);
        ("error", Pm_error.to_json error);
      ]
  | DepsPackageDownloadQueued { package; version; path }
  | DepsPackageDownloadStarted { package; version; path }
  | DepsPackageCacheHit { package; version; path }
  | DepsPackageMaterializationStarted { package; version; path } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", Json.String version);
        ("path", Json.String path);
      ]
  | DepsPackageDownloadFinished {
      package;
      version;
      path;
      duration_ms;
    }
  | DepsPackageMaterializationFinished {
      package;
      version;
      path;
      duration_ms;
    } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", Json.String version);
        ("path", Json.String path);
        ("duration_ms", Json.Int duration_ms);
      ]
  | DepsPackageDownloadFailed {
      package;
      version;
      path;
      error;
    }
  | DepsPackageMaterializationFailed {
      package;
      version;
      path;
      error;
    } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", Json.String version);
        ("path", Json.String path);
        ("error", Pm_error.to_json error);
      ]
  | DepsPackageDownloadSkipped {
      package;
      version;
      path;
      reason;
    } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", Json.String version);
        ("path", Json.String path);
        ("reason", Json.String reason);
      ]
  | DepsPackageResolvedForBuild {
      package;
      version;
      path;
      workspace;
    } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", json_of_string_option version);
        ("path", Json.String path);
        ("workspace", Json.Bool workspace);
      ]

let test_suite_json = fun suite ->
  Json.Object [
    ("package", package_name_json suite.package_name);
    ("suite", Json.String suite.suite_name);
  ]

let test_case_type_json = fun __tmp1 ->
  match __tmp1 with
  | Unit -> Json.Object [ ("type", Json.String "test") ]
  | Property { examples } ->
      Json.Object [ ("type", Json.String "property"); ("examples", Json.Int examples) ]
  | Fuzz { seeds } -> Json.Object [ ("type", Json.String "fuzz"); ("seeds", Json.Int seeds) ]

let test_case_size_json = fun __tmp1 ->
  match __tmp1 with
  | Small -> Json.String "small"
  | Large -> Json.String "large"

let test_case_reliability_json = fun __tmp1 ->
  match __tmp1 with
  | Stable -> Json.Object [ ("type", Json.String "stable") ]
  | Flaky { retry_attempts } ->
      Json.Object [ ("type", Json.String "flaky"); ("retry_attempts", Json.Int retry_attempts) ]

let test_case_status_json = fun __tmp1 ->
  match __tmp1 with
  | Passed -> Json.Object [ ("type", Json.String "passed") ]
  | Failed message ->
      Json.Object [ ("type", Json.String "failed"); ("message", Json.String message) ]
  | TimedOut { timeout_ms } ->
      Json.Object [ ("type", Json.String "timed_out"); ("timeout_ms", Json.Int timeout_ms) ]
  | Skipped -> Json.Object [ ("type", Json.String "skipped") ]

let test_case_result_json = fun result ->
  Json.Object [
    ("index", Json.Int result.index);
    ("name", Json.String result.name);
    ("test_type", test_case_type_json result.test_type);
    ("size", test_case_size_json result.size);
    ("reliability", test_case_reliability_json result.reliability);
    ("attempts", Json.Int result.attempts);
    ("result", test_case_status_json result.result);
    ("duration_us", Json.Int result.duration_us);
  ]

let failed_test_json = fun failed ->
  Json.Object [
    ("suite", test_suite_json failed.suite);
    ("name", Json.String failed.name);
    ("message", Json.String failed.message);
    ("duration_us", Json.Int failed.duration_us);
  ]

let test_summary_json = fun summary ->
  Json.Object [
    ("total", Json.Int summary.total);
    ("passed", Json.Int summary.passed);
    ("failed", Json.Int summary.failed);
    ("skipped", Json.Int summary.skipped);
    ("duration_us", Json.Int summary.duration_us);
    ("results", Json.Array (List.map summary.results ~fn:test_case_result_json));
  ]

let test_event_json = fun __tmp1 ->
  match __tmp1 with
  | TestNoSuitesFound { package_name; suite_name } ->
      Json.Object [
        ("package", match package_name with
        | Some package -> package_name_json package
        | None -> Json.Null);
        ("suite", json_of_string_option suite_name);
      ]
  | TestSuitesCollected { package_name; suite_name; suite_count } ->
      Json.Object [
        ("package", match package_name with
        | Some package -> package_name_json package
        | None -> Json.Null);
        ("suite", json_of_string_option suite_name);
        ("suite_count", Json.Int suite_count);
      ]
  | TestResolvingSuiteBinary suite
  | TestRunningSuite suite -> Json.Object [ ("suite", test_suite_json suite) ]
  | TestSuiteBinaryResolved { suite; binary_path }
  | TestParsingSuiteOutput { suite; binary_path } ->
      Json.Object [ ("suite", test_suite_json suite); ("binary_path", path_json binary_path) ]
  | TestExecutingSuiteBinary { suite; binary_path; args } ->
      Json.Object [
        ("suite", test_suite_json suite);
        ("binary_path", path_json binary_path);
        ("args", strings_json args);
      ]
  | TestSuiteHeartbeat { suite; binary_path; elapsed_us } ->
      Json.Object [
        ("suite", test_suite_json suite);
        ("binary_path", path_json binary_path);
        ("elapsed_us", Json.Int elapsed_us);
      ]
  | TestSuiteBinaryFinished {
      suite;
      binary_path;
      status;
      stdout_bytes;
      stderr_bytes;
    } ->
      Json.Object [
        ("suite", test_suite_json suite);
        ("binary_path", path_json binary_path);
        ("status", Json.Int status);
        ("stdout_bytes", Json.Int stdout_bytes);
        ("stderr_bytes", Json.Int stderr_bytes);
      ]
  | TestSuiteProgress { suite; event } ->
      Json.Object [ ("suite", test_suite_json suite); ("event", event) ]
  | TestSuiteCompleted {
      suite;
      status;
      stdout;
      stderr;
      started_at_us;
      completed_at_us;
      duration_us;
      summary;
    } ->
      Json.Object [
        ("suite", test_suite_json suite);
        ("status", Json.Int status);
        ("stdout", Json.String stdout);
        ("stderr", Json.String stderr);
        ("started_at_us", json_of_int_option started_at_us);
        ("completed_at_us", json_of_int_option completed_at_us);
        ("duration_us", json_of_int_option duration_us);
        ("summary", test_summary_json summary);
      ]
  | TestSummary {
      total;
      passed;
      failed;
      skipped;
      failed_tests;
    } ->
      Json.Object [
        ("total", Json.Int total);
        ("passed", Json.Int passed);
        ("failed", Json.Int failed);
        ("skipped", Json.Int skipped);
        ("failed_tests", Json.Array (List.map failed_tests ~fn:failed_test_json));
      ]

let workspace_event_json = fun __tmp1 ->
  match __tmp1 with
  | WorkspaceEmpty
  | WorkspaceScanning -> Json.Object []
  | WorkspaceScanned { packages; duration_ms } ->
      Json.Object [ ("packages", Json.Int packages); ("duration_ms", Json.Int duration_ms) ]

let server_event_json = fun __tmp1 ->
  match __tmp1 with
  | ServerStarted { pid } -> Json.Object [ ("pid", Json.String pid) ]
  | ServerScanning { root } -> Json.Object [ ("root", Json.String root) ]
  | ServerRestarted { packages; toolchain } ->
      Json.Object [ ("packages", Json.Int packages); ("toolchain", Json.String toolchain) ]
  | ServerShutdown -> Json.Object []

let rpc_event_json = fun __tmp1 ->
  match __tmp1 with
  | RpcRequestReceived { request_type; args } ->
      Json.Object [ ("request_type", Json.String request_type); ("args", args) ]
  | RpcResponseSent { result } ->
      Json.Object [
        ("success", Json.Bool (
          match result with
          | Ok _ -> true
          | Error _ -> false
        ));
        ("error", match result with
        | Ok _ -> Json.Null
        | Error err -> Json.String err);
      ]

let mcp_event_json = fun __tmp1 ->
  match __tmp1 with
  | McpToolCall { tool; args } -> Json.Object [ ("tool", Json.String tool); ("args", args) ]

let command_event_json = fun __tmp1 ->
  match __tmp1 with
  | CommandBinaryRunning { package; binary; args } ->
      Json.Object [
        ("package", package_name_json package);
        ("binary", Json.String binary);
        ("args", strings_json args);
      ]
  | CommandBinaryInstalling { package; binary } ->
      Json.Object [ ("package", package_name_json package); ("binary", Json.String binary) ]
  | CommandBinaryPromoted { binary; destination; mode } ->
      Json.Object [
        ("binary", Json.String binary);
        ("destination", path_json destination);
        ("mode", json_of_command_install_mode mode);
      ]
  | CommandBinaryInstalled {
      binary;
      duration_ms;
      destination;
      mode;
    } ->
      Json.Object [
        ("binary", Json.String binary);
        ("duration_ms", Json.Int duration_ms);
        ("destination", path_json destination);
        ("mode", json_of_command_install_mode mode);
      ]
  | CommandError error -> Json.Object (("kind", Json.String error.kind) :: error.details)

let kind_to_json = fun __tmp1 ->
  match __tmp1 with
  | Build event -> build_event_json event
  | Cache event -> cache_event_json event
  | Deps event -> deps_event_json event
  | Test event -> test_event_json event
  | Workspace event -> workspace_event_json event
  | Server event -> server_event_json event
  | Rpc event -> rpc_event_json event
  | Mcp event -> mcp_event_json event
  | Command event -> command_event_json event
  | Unknown event -> event.data

let event_to_json = fun event ->
  let clean_message = strip_ansi_codes (display event.kind) in
  Json.Object [
    ("timestamp", Json.String (DateTime.to_iso8601 event.timestamp));
    ("session_id", Json.String (Session_id.to_string event.session_id));
    ("level", Json.String (level_to_string event.level));
    ("event", Json.String (name event.kind));
    ("message", Json.String clean_message);
    ("data", kind_to_json event.kind);
  ]

let list_to_vector = Vector.from_list

let rec json_serializer: Json.t Ser.t = {
  Ser.run =
    (fun backend state json ->
      match json with
      | Json.Null -> backend.null state
      | Json.Bool value -> backend.bool state value
      | Json.Int value -> backend.int state value
      | Json.Float value -> backend.float state value
      | Json.String value -> backend.string state value
      | Json.Array values -> backend.list state json_serializer (list_to_vector values)
      | Json.Object fields ->
          let field_serializers =
            fields
            |> List.map
              ~fn:(fun (name, value) ->
                Ser.field name json_serializer (fun (_: Json.t) -> value))
          in
          backend.record state (Ser.fields field_serializers) json
      | Json.Embed value -> json_serializer.Ser.run backend state value);
}

let serializer: t Ser.t = Ser.contramap event_to_json json_serializer

let package_name_deserializer =
  De.map
    De.string
    (fun value ->
      match Package_name.from_string value with
      | Ok package -> package
      | Error err -> De.raise_error (`Msg (Package_name.error_message err)))

let path_deserializer = De.map De.string Path.v

let de_list = fun decoder ->
  De.map
    (De.list decoder)
    (fun values ->
      let rec loop index acc =
        if index < 0 then
          acc
        else
          loop (index - 1) (Vector.get_unchecked values ~at:index :: acc)
      in
      loop (Vector.length values - 1) [])

let resolution_mode_deserializer =
  De.map
    De.string
    (fun value ->
      match value with
      | "refresh" -> `Refresh
      | "unlock" -> `Unlock
      | _ -> De.raise_error (`Msg ("invalid dependency resolution mode: " ^ value)))

let manifest_operation_deserializer =
  De.map
    De.string
    (fun value ->
      match value with
      | "add" -> `Add
      | "remove" -> `Remove
      | _ -> De.raise_error (`Msg ("invalid manifest operation: " ^ value)))

let command_install_mode_deserializer =
  De.map
    De.string
    (fun value ->
      match value with
      | "local" -> CommandInstallLocal
      | "global" -> CommandInstallGlobal
      | _ -> De.raise_error (`Msg ("invalid command install mode: " ^ value)))

let empty_unknown = fun event message -> Unknown { event; message; data = Json.Object [] }

type package_resolved_builder = {
  mutable resolved_package: Package_name.t option;
  mutable resolved_version: string option;
  mutable resolved_path: string option;
  mutable resolved_workspace: bool option;
}

let deps_package_resolved_deserializer =
  De.record_mut
    ~fields:(
      De.fields
        [
          De.field "package" `Package;
          De.field "version" `Version;
          De.field "path" `Path;
          De.field "workspace" `Workspace;
        ]
    )
    ~create:(fun () ->
      {
        resolved_package = None;
        resolved_version = None;
        resolved_path = None;
        resolved_workspace = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some `Package -> builder.resolved_package <- Some (De.read reader package_name_deserializer)
      | Some `Version -> builder.resolved_version <- De.read reader (De.option De.string)
      | Some `Path -> builder.resolved_path <- Some (De.read reader De.string)
      | Some `Workspace -> builder.resolved_workspace <- Some (De.read reader De.bool)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.resolved_package, builder.resolved_path, builder.resolved_workspace) with
      | (Some package, Some path, Some workspace) ->
          Deps (
            DepsPackageResolvedForBuild {
              package;
              version = builder.resolved_version;
              path;
              workspace;
            }
          )
      | _ -> De.missing_field ())

type package_version_builder = {
  mutable version_package: Package_name.t option;
  mutable version_value: string option;
}

let deps_package_version_locked_deserializer =
  De.record_mut
    ~fields:(
      De.fields
        [
          De.field "package" `Package;
          De.field "version" `Version;
        ]
    )
    ~create:(fun () -> { version_package = None; version_value = None })
    ~step:(fun reader builder field ->
      match field with
      | Some `Package -> builder.version_package <- Some (De.read reader package_name_deserializer)
      | Some `Version -> builder.version_value <- Some (De.read reader De.string)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.version_package, builder.version_value) with
      | (Some package, Some version) -> Deps (DepsPackageVersionLocked { package; version })
      | _ -> De.missing_field ())

type lockfile_duration_builder = {
  mutable lockfile_path: string option;
  mutable lockfile_duration_ms: int option;
}

let lockfile_read_finished_deserializer =
  De.record_mut
    ~fields:(
      De.fields
        [
          De.field "path" `Path;
          De.field "duration_ms" `Duration_ms;
        ]
    )
    ~create:(fun () -> { lockfile_path = None; lockfile_duration_ms = None })
    ~step:(fun reader builder field ->
      match field with
      | Some `Path -> builder.lockfile_path <- Some (De.read reader De.string)
      | Some `Duration_ms -> builder.lockfile_duration_ms <- Some (De.read reader De.int)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.lockfile_path, builder.lockfile_duration_ms) with
      | (Some path, Some duration_ms) -> Deps (DepsLockfileReadFinished { path; duration_ms })
      | _ -> De.missing_field ())

type resolution_started_builder = {
  mutable resolution_packages: Package_name.t list option;
  mutable resolution_mode: [`Refresh | `Unlock] option;
}

let deps_resolution_started_deserializer =
  De.record_mut
    ~fields:(
      De.fields
        [
          De.field "packages" `Packages;
          De.field "mode" `Mode;
        ]
    )
    ~create:(fun () -> { resolution_packages = None; resolution_mode = None })
    ~step:(fun reader builder field ->
      match field with
      | Some `Packages ->
          builder.resolution_packages <- Some (De.read reader (de_list package_name_deserializer))
      | Some `Mode -> builder.resolution_mode <- Some (De.read reader resolution_mode_deserializer)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match builder.resolution_packages with
      | Some packages ->
          Deps (DepsResolutionStarted {
            packages;
            mode = Option.unwrap_or ~default:`Refresh builder.resolution_mode;
          })
      | None -> De.missing_field ())

type manifest_updated_builder = {
  mutable manifest_path: string option;
  mutable manifest_section: string option;
  mutable manifest_operation: [`Add | `Remove] option;
  mutable manifest_dependency: string option;
}

let deps_manifest_updated_deserializer =
  De.record_mut
    ~fields:(
      De.fields
        [
          De.field "path" `Path;
          De.field "section" `Section;
          De.field "operation" `Operation;
          De.field "dependency" `Dependency;
        ]
    )
    ~create:(fun () ->
      {
        manifest_path = None;
        manifest_section = None;
        manifest_operation = None;
        manifest_dependency = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some `Path -> builder.manifest_path <- Some (De.read reader De.string)
      | Some `Section -> builder.manifest_section <- Some (De.read reader De.string)
      | Some `Operation ->
          builder.manifest_operation <- Some (De.read reader manifest_operation_deserializer)
      | Some `Dependency -> builder.manifest_dependency <- Some (De.read reader De.string)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.manifest_path,
        builder.manifest_section,
        builder.manifest_operation,
        builder.manifest_dependency
      ) with
      | (Some path, Some section, Some operation, Some dependency) ->
          Deps (
            DepsManifestUpdated {
              path;
              section;
              operation;
              dependency;
            }
          )
      | _ -> De.missing_field ())

let package_versions_unchanged_deserializer =
  De.record_mut
    ~fields:(
      De.fields
        [
          De.field "packages" `Packages;
        ]
    )
    ~create:(fun () -> ref None)
    ~step:(fun reader packages field ->
      match field with
      | Some `Packages -> packages := Some (De.read reader De.int)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun packages ->
      match !packages with
      | Some packages -> Deps (DepsPackageVersionsUnchanged { packages })
      | None -> De.missing_field ())

type command_running_builder = {
  mutable command_package: Package_name.t option;
  mutable command_binary: string option;
  mutable command_args: string list option;
}

let command_binary_running_deserializer =
  De.record_mut
    ~fields:(
      De.fields
        [
          De.field "package" `Package;
          De.field "binary" `Binary;
          De.field "args" `Args;
        ]
    )
    ~create:(fun () -> { command_package = None; command_binary = None; command_args = None })
    ~step:(fun reader builder field ->
      match field with
      | Some `Package -> builder.command_package <- Some (De.read reader package_name_deserializer)
      | Some `Binary -> builder.command_binary <- Some (De.read reader De.string)
      | Some `Args -> builder.command_args <- Some (De.read reader (de_list De.string))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.command_package, builder.command_binary) with
      | (Some package, Some binary) ->
          Command (CommandBinaryRunning {
            package;
            binary;
            args = Option.unwrap_or ~default:[] builder.command_args;
          })
      | _ -> De.missing_field ())

let command_binary_installing_deserializer =
  De.record_mut
    ~fields:(
      De.fields
        [
          De.field "package" `Package;
          De.field "binary" `Binary;
        ]
    )
    ~create:(fun () -> { command_package = None; command_binary = None; command_args = None })
    ~step:(fun reader builder field ->
      match field with
      | Some `Package -> builder.command_package <- Some (De.read reader package_name_deserializer)
      | Some `Binary -> builder.command_binary <- Some (De.read reader De.string)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.command_package, builder.command_binary) with
      | (Some package, Some binary) -> Command (CommandBinaryInstalling { package; binary })
      | _ -> De.missing_field ())

type command_install_builder = {
  mutable install_binary: string option;
  mutable install_duration_ms: int option;
  mutable install_destination: Path.t option;
  mutable install_mode: command_install_mode option;
}

let create_command_install_builder = fun () ->
  {
    install_binary = None;
    install_duration_ms = None;
    install_destination = None;
    install_mode = None;
  }

let command_install_step = fun reader builder field ->
  match field with
  | Some `Binary -> builder.install_binary <- Some (De.read reader De.string)
  | Some `Duration_ms -> builder.install_duration_ms <- Some (De.read reader De.int)
  | Some `Destination -> builder.install_destination <- Some (De.read reader path_deserializer)
  | Some `Mode -> builder.install_mode <- Some (De.read reader command_install_mode_deserializer)
  | None -> ignore (De.read reader De.skip_any)

let command_binary_promoted_deserializer =
  De.record_mut
    ~fields:(
      De.fields
        [
          De.field "binary" `Binary;
          De.field "destination" `Destination;
          De.field "mode" `Mode;
        ]
    )
    ~create:create_command_install_builder
    ~step:command_install_step
    ~finish:(fun builder ->
      match (builder.install_binary, builder.install_destination, builder.install_mode) with
      | (Some binary, Some destination, Some mode) ->
          Command (CommandBinaryPromoted { binary; destination; mode })
      | _ -> De.missing_field ())

let command_binary_installed_deserializer =
  De.record_mut
    ~fields:(
      De.fields
        [
          De.field "binary" `Binary;
          De.field "duration_ms" `Duration_ms;
          De.field "destination" `Destination;
          De.field "mode" `Mode;
        ]
    )
    ~create:create_command_install_builder
    ~step:command_install_step
    ~finish:(fun builder ->
      match (
        builder.install_binary,
        builder.install_duration_ms,
        builder.install_destination,
        builder.install_mode
      ) with
      | (Some binary, Some duration_ms, Some destination, Some mode) ->
          Command (
            CommandBinaryInstalled {
              binary;
              duration_ms;
              destination;
              mode;
            }
          )
      | _ -> De.missing_field ())

let data_deserializer = fun event message ->
  match event with
  | "riot.command.binary.running" -> command_binary_running_deserializer
  | "riot.command.binary.installing" -> command_binary_installing_deserializer
  | "riot.command.binary.promoted" -> command_binary_promoted_deserializer
  | "riot.command.binary.installed" -> command_binary_installed_deserializer
  | "riot.deps.lockfile.read.finished" -> lockfile_read_finished_deserializer
  | "riot.deps.resolution.started" -> deps_resolution_started_deserializer
  | "riot.deps.package.resolved_for_build" -> deps_package_resolved_deserializer
  | "riot.deps.manifest.updated" -> deps_manifest_updated_deserializer
  | "riot.deps.package.version.locked" -> deps_package_version_locked_deserializer
  | "riot.deps.package.versions.unchanged" -> package_versions_unchanged_deserializer
  | _ -> De.map De.skip_any (fun () -> empty_unknown event message)

type event_field =
  | Timestamp
  | Session_id
  | Level
  | Event
  | Message
  | Data

type event_builder = {
  mutable event_timestamp: DateTime.t option;
  mutable event_session_id: Session_id.t option;
  mutable event_level: level option;
  mutable event_name: string option;
  mutable event_message: string option;
  mutable event_kind: kind option;
}

let deserializer =
  De.record_mut
    ~fields:(De.fields
      [
        De.field "timestamp" Timestamp;
        De.field "session_id" Session_id;
        De.field "level" Level;
        De.field "event" Event;
        De.field "message" Message;
        De.field "data" Data;
      ])
    ~create:(fun () ->
      {
        event_timestamp = None;
        event_session_id = None;
        event_level = None;
        event_name = None;
        event_message = None;
        event_kind = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Timestamp ->
          let timestamp = De.read reader De.string in
          builder.event_timestamp <- Some (
            match DateTime.parse timestamp with
            | Ok parsed -> parsed
            | Error _ -> DateTime.now ()
          )
      | Some Session_id ->
          builder.event_session_id <- Some (Session_id.from_string (De.read reader De.string))
      | Some Level -> builder.event_level <- Some (level_of_string (De.read reader De.string))
      | Some Event -> builder.event_name <- Some (De.read reader De.string)
      | Some Message -> builder.event_message <- Some (De.read reader De.string)
      | Some Data -> (
          match builder.event_name with
          | Some event_name ->
              builder.event_kind <- Some (De.read
                reader
                (data_deserializer event_name builder.event_message))
          | None -> ignore (De.read reader De.skip_any)
        )
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      let kind =
        match builder.event_kind with
        | Some kind -> kind
        | None ->
            let event = Option.unwrap_or ~default:"riot.unknown" builder.event_name in
            empty_unknown event builder.event_message
      in
      {
        timestamp = Option.unwrap_or ~default:(DateTime.now ()) builder.event_timestamp;
        session_id = Option.unwrap_or ~default:(Session_id.make ()) builder.event_session_id;
        level = Option.unwrap_or ~default:Info builder.event_level;
        kind;
      })

let to_json = fun event ->
  match Serde_json.to_string serializer event with
  | Ok content -> (
      match Json.from_string content with
      | Ok json -> json
      | Error _ -> event_to_json event
    )
  | Error _ -> event_to_json event

let from_json = fun json ->
  Serde_json.from_string deserializer (Json.to_string json)
  |> Result.map_err ~fn:Serde.Error.to_string

module Tests = struct
  let package_name = fun name ->
    Result.expect
      (Package_name.from_string name)
      ~msg:("package name " ^ name)

  let event = fun kind ->
    create
      ~session_id:(Session_id.from_string "test-session")
      ~level:Info
      kind

  let test_deps_event_names_are_namespaced () =
    let actual =
      name (Deps (DepsPackageVersionLocked { package = package_name "std"; version = "0.2.0" }))
    in
    if String.equal actual "riot.deps.package.version.locked" then
      Ok ()
    else
      Error ("unexpected event name: " ^ actual) [@test]

  let test_build_cache_event_names_are_standard () =
    let actual = name (Cache (CacheBuildHit { package = package_name "std"; hash = "abc123" })) in
    if String.equal actual "riot.build.cache.hit" then
      Ok ()
    else
      Error ("unexpected event name: " ^ actual) [@test]

  let test_deps_json_roundtrip () =
    let original =
      event
        (
          Deps (
            DepsPackageResolvedForBuild {
              package = package_name "std";
              version = Some "0.1.0";
              path = "/tmp/std";
              workspace = false;
            }
          )
        )
    in
    match from_json (to_json original) with
    | Ok { kind = Deps (DepsPackageResolvedForBuild {
                          package;
                          version;
                          path;
                          workspace;
                        }); _ } ->
        if
          Package_name.equal package (package_name "std")
          && version = Some "0.1.0"
          && String.equal path "/tmp/std"
          && not workspace
        then
          Ok ()
        else
          Error "expected package resolved event to round-trip"
    | Ok _ -> Error "expected deps package resolved event"
    | Error err -> Error err [@test]
end [@test]
