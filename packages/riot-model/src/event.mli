(** Event system for riot - pure data types for events *)
open Std
open Std.Data

module Pm_error = Pm_error

(** Strip ANSI escape codes from a string *)
val strip_ansi_codes: string -> string

(** Log severity levels *)
(** Reasons why a package was skipped *)
type level =
  | Error
  | Warn
  | Info
  | Debug
  | Trace
type skip_reason =
  | DependenciesFailed of string list

(** List of failed dependency names *)
val level_to_string: level -> string

type error_kind =
  | SyntaxError
  | TypeError of { description: string }
  | UnboundValue of { name: string }
  | UnboundModule of { name: string }
  | FileNotFound of { filename: string }
  | OtherError of { message: string }
type build_error = {
  file: string;
  line: int;
  span: int * int;
  (** start, end character positions *)
  hint: string;
  (** The source line with caret pointing to error *)
  kind: error_kind;
  raw: string;
  (** Raw compiler output *)
}
(** Event kinds - the actual event data *)
type build_result = {
  package: string;
  success: bool;
  duration_ms: int;
  modules_compiled: int;
  cache_hits: int;
  cache_misses: int;
  errors: build_error list;
}
(** Complete event with metadata *)
type kind =
  | BuildComplete of {
      duration_ms: int;
      results: build_result list;
      succeeded: string list;
      failed: string list
    }
  | BuildGraphCreated of { nodes: int; duration_ms: int }
  | BuildGraphCreating
  | BuildStarted of { packages: string list; total_modules: int; workers: int }
  | CacheHit of { package: string; hash: string }
  | CacheMiss of { package: string; hash: string }
  | CacheStored of { package: string; hash: string; artifacts: string list }
  | CompileError of { package: string; error: build_error }
  | CompilingImplementation of { package: string; file: string }
  | CompilingInterface of { package: string; file: string }
  | ComputingHash of { package: string }
  | CopyingFile of { source: string; dest: string }
  | CreatingDirectory of { path: string }
  | CycleDetected of { packages: string list }
  | DependencyMissing of { package: string; missing: string list }
  | DependencySatisfied of { package: string }
  | HashComputed of { package: string; hash: string }
  | LinkingExecutable of { package: string; output: string }
  | LinkingLibrary of { package: string; output: string }
  | McpToolCall of { tool: string; args: Json.t }
  | PackageComplete of build_result
  | PackageSkipped of { package: string; reason: skip_reason }
  | PackageStarted of { package: string }
  | QueuePackage of { package: string; queue_type:
        [
          | `Ready
          | `Waiting
        ] }
  | QueueStats of { ready: int; waiting: int; busy: int }
  | RpcRequestReceived of { request_type: string; args: Json.t }
  | RpcResponseSent of { result: (unit, string) result }
  | ServerRestarted of { packages: int; toolchain: string }
  | ServerScanning of { root: string }
  | ServerShutdown
  | ServerStarted of { pid: string }
  | WorkerAssigned of { worker_id: Worker_id.t; package: string }
  | WorkerIdle of { worker_id: Worker_id.t }
  | WorkerPoolStarted of { workers: int }
  | WorkerStarted of { worker_id: Worker_id.t }
  | StoreCreating
  | StoreCreated of { duration_ms: int }
  | WorkerPoolCreating of { workers: int }
  | WorkerPoolCreated of { workers: int; duration_ms: int }
  | WorkspaceEmpty
  | WorkspaceScanning
  | WorkspaceScanned of { packages: int; duration_ms: int }
  | LockfileReadStarted of { path: string }
  | LockfileReadFinished of { path: string; duration_ms: int }
  | LockfileReadFailed of { path: string; error: Pm_error.t }
  | LockfileWriteStarted of { path: string }
  | LockfileWriteFinished of { path: string; duration_ms: int }
  | LockfileWriteFailed of { path: string; error: Pm_error.t }
  | DependencyResolutionStarted of { packages: string list; mode:
        [
          | `Refresh
          | `Unlock
        ] }
  | DependencyResolutionUsingExistingLock of { path: string }
  | DependencyResolutionRefreshingLock of { path: string }
  | DependencyResolutionUnlocking of { path: string option }
  | DependencyResolutionFinished of { duration_ms: int; resolved_packages: int; resolved_edges: int }
  | DependencyResolutionFailed of { error: Pm_error.t }
  | RegistryIndexUpdating of { registry: string }
  | DependencyUniverseBuilding of { packages: string list }
  | DependencyUniverseBuilt of {
      runtime_packages: int;
      build_packages: int;
      dev_packages: int;
      duration_ms: int
    }
  | PackageMetadataFetchStarted of { registry: string; package: string }
  | PackageMetadataFetchFinished of {
      registry: string;
      package: string;
      version: string option;
      duration_ms: int
    }
  | PackageMetadataFetchFailed of { registry: string; package: string; error: Pm_error.t }
  | PackageManifestFetchStarted of { package: string; version: string }
  | PackageManifestFetchFinished of { package: string; version: string; duration_ms: int }
  | PackageManifestFetchFailed of { package: string; version: string option; error: Pm_error.t }
  | PackageDownloadStarted of { package: string; version: string; path: string }
  | PackageDownloadFinished of { package: string; version: string; path: string; duration_ms: int }
  | PackageDownloadFailed of { package: string; version: string; path: string; error: Pm_error.t }
  | PackageDownloadSkipped of { package: string; version: string; path: string; reason: string }
  | PackageCacheHit of { package: string; version: string; path: string }
  | PackageMaterializationStarted of { package: string; version: string; path: string }
  | PackageMaterializationFinished of {
      package: string;
      version: string;
      path: string;
      duration_ms: int
    }
  | PackageMaterializationFailed of {
      package: string;
      version: string;
      path: string;
      error: Pm_error.t
    }
  | PackageResolvedForBuild of {
      package: string;
      version: string option;
      path: string;
      workspace: bool
    }
  | PackageDownloadQueued of { package: string; version: string; path: string }
  | WritingFile of { path: string }
(** Create a new event with current timestamp *)
type t = {
  timestamp: Std.Datetime.t;
  session_id: Session_id.t;
  level: level;
  kind: kind;
}
val create: session_id:Session_id.t -> level:level -> kind -> t

(** Get the machine-readable event name *)
val name: kind -> string

(** Get human-readable display message *)
val display: kind -> string

(** Convert to human-readable string with timestamp *)
val to_string: t -> string

(** Convert event to JSON representation *)
val to_json: t -> Json.t

(** Convert from JSON representation *)
val from_json: Json.t -> (t, string) result
