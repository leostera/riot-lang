open Std
open Std.Data
open Std.Collections
open Std.Result.Syntax

module Pm_error = Pm_error

(** Event system for riot - pure data types for events *)

(** Strip ANSI escape codes from a string *)
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

type skip_reason =
  | DependenciesFailed of Package_name.t list

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
  (* start, end character positions *)
  hint: string;
  (* The source line with caret pointing to error *)
  kind: error_kind;
  raw: string;
  (* Raw compiler output *)
}

type build_result = {
  package: Package_name.t;
  success: bool;
  duration_ms: int;
  modules_compiled: int;
  cache_hits: int;
  cache_misses: int;
  errors: build_error list;
}

type kind =
  | BuildComplete of {
      duration_ms: int;
      results: build_result list;
      succeeded: Package_name.t list;
      failed: Package_name.t list;
    }
  | BuildGraphCreated of { nodes: int; duration_ms: int }
  | BuildGraphCreating
  | BuildStarted of {
      packages: Package_name.t list;
      total_modules: int;
      workers: int;
    }
  | CacheHit of {
      package: Package_name.t;
      hash: string;
    }
  | CacheMiss of {
      package: Package_name.t;
      hash: string;
    }
  | CacheStored of {
      package: Package_name.t;
      hash: string;
      artifacts: string list;
    }
  | CompileError of {
      package: Package_name.t;
      error: build_error;
    }
  | CompilingImplementation of {
      package: Package_name.t;
      file: string;
    }
  | CompilingInterface of {
      package: Package_name.t;
      file: string;
    }
  | ComputingHash of {
      package: Package_name.t;
    }
  | CopyingFile of { source: string; dest: string }
  | CreatingDirectory of { path: string }
  | CycleDetected of {
      packages: Package_name.t list;
    }
  | DependencyMissing of {
      package: Package_name.t;
      missing: Package_name.t list;
    }
  | DependencySatisfied of {
      package: Package_name.t;
    }
  | HashComputed of {
      package: Package_name.t;
      hash: string;
    }
  | LinkingExecutable of {
      package: Package_name.t;
      output: string;
    }
  | LinkingLibrary of {
      package: Package_name.t;
      output: string;
    }
  | McpToolCall of {
      tool: string;
      args: Json.t;
    }
  | PackageComplete of build_result
  | PackageSkipped of {
      package: Package_name.t;
      reason: skip_reason;
    }
  | PackageStarted of {
      package: Package_name.t;
    }
  | QueuePackage of {
      package: Package_name.t;
      queue_type: [`Ready | `Waiting];
    }
  | QueueStats of { ready: int; waiting: int; busy: int }
  | RpcRequestReceived of {
      request_type: string;
      args: Json.t;
    }
  | RpcResponseSent of {
      result: (unit, string) result;
    }
  | ServerRestarted of { packages: int; toolchain: string }
  | ServerScanning of { root: string }
  | ServerShutdown
  | ServerStarted of { pid: string }
  | WorkerAssigned of {
      worker_id: Worker_id.t;
      package: Package_name.t;
    }
  | WorkerIdle of {
      worker_id: Worker_id.t;
    }
  | WorkerPoolStarted of { workers: int }
  | WorkerStarted of {
      worker_id: Worker_id.t;
    }
  | StoreCreating
  | StoreCreated of { duration_ms: int }
  | WorkerPoolCreating of { workers: int }
  | WorkerPoolCreated of { workers: int; duration_ms: int }
  | WorkspaceEmpty
  | WorkspaceScanning
  | WorkspaceScanned of { packages: int; duration_ms: int }
  | LockfileReadStarted of { path: string }
  | LockfileReadFinished of { path: string; duration_ms: int }
  | LockfileReadFailed of {
      path: string;
      error: Pm_error.t;
    }
  | LockfileWriteStarted of { path: string }
  | LockfileWriteFinished of { path: string; duration_ms: int }
  | LockfileWriteFailed of {
      path: string;
      error: Pm_error.t;
    }
  | DependencyResolutionStarted of {
      packages: Package_name.t list;
      mode: [`Refresh | `Unlock];
    }
  | DependencyResolutionUsingExistingLock of { path: string }
  | DependencyResolutionRefreshingLock of { path: string }
  | DependencyResolutionUnlocking of {
      path: string option;
    }
  | DependencyResolutionFinished of {
      duration_ms: int;
      resolved_packages: int;
      resolved_edges: int;
    }
  | DependencyResolutionFailed of {
      error: Pm_error.t;
    }
  | RegistryIndexUpdating of { registry: string }
  | DependencyUniverseBuilding of {
      packages: Package_name.t list;
    }
  | DependencyUniverseBuilt of {
      runtime_packages: int;
      build_packages: int;
      dev_packages: int;
      duration_ms: int;
    }
  | PackageMetadataFetchStarted of {
      registry: string;
      package: Package_name.t;
    }
  | PackageMetadataFetchFinished of {
      registry: string;
      package: Package_name.t;
      version: string option;
      duration_ms: int;
    }
  | PackageMetadataFetchFailed of {
      registry: string;
      package: Package_name.t;
      error: Pm_error.t;
    }
  | SourceDependencyMaterializationStarted of {
      source_locator: string;
      ref_: string option;
    }
  | SourceDependencyMaterializationFinished of {
      source_locator: string;
      ref_: string option;
      package: Package_name.t;
      version: string option;
    }
  | DependencyManifestUpdated of {
      path: string;
      section: string;
      operation: [ | `Add | `Remove];
      dependency: string;
    }
  | PackageVersionLocked of {
      package: Package_name.t;
      version: string;
    }
  | PackageVersionsUnchanged of { packages: int }
  | PackageVersionUpdated of {
      package: Package_name.t;
      from_version: string;
      to_version: string;
    }
  | PackageManifestFetchStarted of {
      package: Package_name.t;
      version: string;
    }
  | PackageManifestFetchFinished of {
      package: Package_name.t;
      version: string;
      duration_ms: int;
    }
  | PackageManifestFetchFailed of {
      package: Package_name.t;
      version: string option;
      error: Pm_error.t;
    }
  | PackageDownloadStarted of {
      package: Package_name.t;
      version: string;
      path: string;
    }
  | PackageDownloadFinished of {
      package: Package_name.t;
      version: string;
      path: string;
      duration_ms: int;
    }
  | PackageDownloadFailed of {
      package: Package_name.t;
      version: string;
      path: string;
      error: Pm_error.t;
    }
  | PackageDownloadSkipped of {
      package: Package_name.t;
      version: string;
      path: string;
      reason: string;
    }
  | PackageCacheHit of {
      package: Package_name.t;
      version: string;
      path: string;
    }
  | PackageMaterializationStarted of {
      package: Package_name.t;
      version: string;
      path: string;
    }
  | PackageMaterializationFinished of {
      package: Package_name.t;
      version: string;
      path: string;
      duration_ms: int;
    }
  | PackageMaterializationFailed of {
      package: Package_name.t;
      version: string;
      path: string;
      error: Pm_error.t;
    }
  | PackageResolvedForBuild of {
      package: Package_name.t;
      version: string option;
      path: string;
      workspace: bool;
    }
  | PackageDownloadQueued of {
      package: Package_name.t;
      version: string;
      path: string;
    }
  | WritingFile of { path: string }

type t = {
  timestamp: DateTime.t;
  session_id: Session_id.t;
  level: level;
  kind: kind;
}

(** Create a new event with current timestamp *)
let create = fun ~session_id ~level kind ->
  {
    timestamp = DateTime.now ();
    session_id;
    level;
    kind;
  }

(** Format timestamp for display *)

(** Get the machine-readable event name *)
let name = fun __tmp1 ->
  match __tmp1 with
  | BuildComplete _ -> "riot.build.completed"
  | BuildGraphCreated _ -> "riot.build_graph.created"
  | BuildGraphCreating -> "riot.build_graph.creating"
  | BuildStarted _ -> "riot.build.started"
  | CacheHit _ -> "riot.build.cache.hit"
  | CacheMiss _ -> "riot.build.cache.miss"
  | CacheStored _ -> "riot.build.cache.stored"
  | CompileError _ -> "riot.build.compile.error"
  | CompilingImplementation _ -> "riot.build.compile.implementation"
  | CompilingInterface _ -> "riot.build.compile.interface"
  | ComputingHash _ -> "riot.build.hash.computing"
  | CopyingFile _ -> "riot.build.file.copy"
  | CreatingDirectory _ -> "riot.build.directory.create"
  | CycleDetected _ -> "riot.build.cycle.detected"
  | DependencyMissing _ -> "riot.build.dependency.missing"
  | DependencySatisfied _ -> "riot.build.dependency.satisfied"
  | HashComputed _ -> "riot.build.hash.computed"
  | LinkingExecutable _ -> "riot.build.link.executable"
  | LinkingLibrary _ -> "riot.build.link.library"
  | McpToolCall _ -> "riot.mcp.tool_call"
  | PackageComplete _ -> "riot.build.package.completed"
  | PackageSkipped _ -> "riot.build.package.skipped"
  | PackageStarted _ -> "riot.build.package.started"
  | QueuePackage _ -> "riot.build.queue.package"
  | QueueStats _ -> "riot.build.queue.stats"
  | RpcRequestReceived _ -> "riot.rpc.request.received"
  | RpcResponseSent _ -> "riot.rpc.response.sent"
  | ServerRestarted _ -> "riot.server.restarted"
  | ServerScanning _ -> "riot.server.scanning"
  | ServerShutdown -> "riot.server.shutdown"
  | ServerStarted _ -> "riot.server.started"
  | WorkerAssigned _ -> "riot.build.worker.assigned"
  | WorkerIdle _ -> "riot.build.worker.idle"
  | WorkerPoolStarted _ -> "riot.build.worker_pool.started"
  | WorkerStarted _ -> "riot.build.worker.started"
  | WorkspaceEmpty -> "riot.workspace.empty"
  | WorkspaceScanned _ -> "riot.workspace.scanned"
  | WorkspaceScanning -> "riot.workspace.scanning"
  | LockfileReadStarted _ -> "riot.pm.lockfile.read.started"
  | LockfileReadFinished _ -> "riot.pm.lockfile.read.finished"
  | LockfileReadFailed _ -> "riot.pm.lockfile.read.failed"
  | LockfileWriteStarted _ -> "riot.pm.lockfile.write.started"
  | LockfileWriteFinished _ -> "riot.pm.lockfile.write.finished"
  | LockfileWriteFailed _ -> "riot.pm.lockfile.write.failed"
  | DependencyResolutionStarted _ -> "riot.pm.resolution.started"
  | DependencyResolutionUsingExistingLock _ -> "riot.pm.resolution.using_existing_lock"
  | DependencyResolutionRefreshingLock _ -> "riot.pm.resolution.refreshing_lock"
  | DependencyResolutionUnlocking _ -> "riot.pm.resolution.unlocking"
  | DependencyResolutionFinished _ -> "riot.pm.resolution.finished"
  | DependencyResolutionFailed _ -> "riot.pm.resolution.failed"
  | RegistryIndexUpdating _ -> "riot.pm.registry.index.updating"
  | DependencyUniverseBuilding _ -> "riot.pm.universe.building"
  | DependencyUniverseBuilt _ -> "riot.pm.universe.built"
  | PackageMetadataFetchStarted _ -> "riot.pm.package_metadata.fetch.started"
  | PackageMetadataFetchFinished _ -> "riot.pm.package_metadata.fetch.finished"
  | PackageMetadataFetchFailed _ -> "riot.pm.package_metadata.fetch.failed"
  | SourceDependencyMaterializationStarted _ -> "riot.pm.source_dependency.materialization.started"
  | SourceDependencyMaterializationFinished _ -> "riot.pm.source_dependency.materialization.finished"
  | DependencyManifestUpdated _ -> "riot.pm.manifest.updated"
  | PackageVersionLocked _ -> "riot.pm.package.locked"
  | PackageVersionsUnchanged _ -> "riot.pm.package.unchanged"
  | PackageVersionUpdated _ -> "riot.pm.package.updated"
  | PackageManifestFetchStarted _ -> "riot.pm.package_manifest.fetch.started"
  | PackageManifestFetchFinished _ -> "riot.pm.package_manifest.fetch.finished"
  | PackageManifestFetchFailed _ -> "riot.pm.package_manifest.fetch.failed"
  | PackageDownloadStarted _ -> "riot.pm.package_download.started"
  | PackageDownloadFinished _ -> "riot.pm.package_download.finished"
  | PackageDownloadFailed _ -> "riot.pm.package_download.failed"
  | PackageDownloadSkipped _ -> "riot.pm.package_download.skipped"
  | PackageCacheHit _ -> "riot.pm.package_cache.hit"
  | PackageMaterializationStarted _ -> "riot.pm.package_materialization.started"
  | PackageMaterializationFinished _ -> "riot.pm.package_materialization.finished"
  | PackageMaterializationFailed _ -> "riot.pm.package_materialization.failed"
  | PackageResolvedForBuild _ -> "riot.pm.package_resolved_for_build"
  | PackageDownloadQueued _ -> "riot.pm.package_download.queued"
  | WritingFile _ -> "riot.build.file.write"
  | StoreCreating -> "riot.store.creating"
  | StoreCreated _ -> "riot.store.created"
  | WorkerPoolCreating _ -> "riot.worker_pool.creating"
  | WorkerPoolCreated _ -> "riot.worker_pool.created"

(** Get human-readable display message *)
let display = fun __tmp1 ->
  match __tmp1 with
  | BuildStarted { packages; _ } ->
      "Build started for " ^ Int.to_string (List.length packages) ^ " packages"
  | BuildComplete { duration_ms; succeeded; failed; _ } ->
      "Build completed in "
      ^ Int.to_string duration_ms
      ^ "ms ("
      ^ Int.to_string (List.length succeeded)
      ^ " succeeded, "
      ^ Int.to_string (List.length failed)
      ^ " failed)"
  | PackageStarted { package } -> "Building " ^ Package_name.to_string package ^ "..."
  | PackageComplete { package; success; duration_ms; _ } ->
      if success then
        "✓ Built " ^ Package_name.to_string package ^ " in " ^ Int.to_string duration_ms ^ "ms"
      else
        "✗ Failed to build " ^ Package_name.to_string package
  | PackageSkipped { package; reason } ->
      let reason_str =
        match reason with
        | DependenciesFailed deps ->
            "dependencies failed: " ^ String.concat ", " (List.map deps ~fn:Package_name.to_string)
      in
      "⊘ Skipped " ^ Package_name.to_string package ^ " (" ^ reason_str ^ ")"
  | CompileError { package; error } ->
      let (col_start, _) = error.span in
      let kind_msg =
        match error.kind with
        | SyntaxError -> "Syntax error"
        | TypeError { description } -> description
        | UnboundValue { name } -> "Unbound value " ^ name
        | UnboundModule { name } -> "Unbound module " ^ name
        | FileNotFound { filename } -> "Cannot find file " ^ filename
        | OtherError { message } -> message
      in
      "Error in "
      ^ Package_name.to_string package
      ^ " ["
      ^ error.file
      ^ ":"
      ^ Int.to_string error.line
      ^ ":"
      ^ Int.to_string col_start
      ^ "]: "
      ^ kind_msg
  | CycleDetected { packages } ->
      "Circular dependency detected: "
      ^ String.concat " -> " (List.map packages ~fn:Package_name.to_string)
  | CacheHit { package; _ } -> "Cached " ^ Package_name.to_string package
  | CacheMiss { package; _ } -> "Cache miss for " ^ Package_name.to_string package
  | CacheStored { package; artifacts; _ } ->
      "Cached "
      ^ Package_name.to_string package
      ^ " ("
      ^ Int.to_string (List.length artifacts)
      ^ " artifacts)"
  | WorkerPoolStarted { workers } ->
      "Started worker pool with " ^ Int.to_string workers ^ " workers"
  | WorkerStarted { worker_id } -> "Worker " ^ Worker_id.to_string worker_id ^ " started"
  | WorkerAssigned { worker_id; package } ->
      "Worker " ^ Worker_id.to_string worker_id ^ " assigned to " ^ Package_name.to_string package
  | WorkerIdle { worker_id } -> "Worker " ^ Worker_id.to_string worker_id ^ " idle"
  | ServerStarted { pid } -> "Server started (pid: " ^ pid ^ ")"
  | ServerScanning { root } -> "Scanning workspace: " ^ root
  | ServerRestarted { packages; toolchain } ->
      "Server restarted with " ^ Int.to_string packages ^ " packages (toolchain: " ^ toolchain ^ ")"
  | WorkspaceEmpty -> "No packages found in workspace"
  | WorkspaceScanning -> "Scanning workspace..."
  | WorkspaceScanned { packages; duration_ms } ->
      "Scanned workspace: "
      ^ Int.to_string packages
      ^ " packages in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | LockfileReadStarted { path } -> "Reading lockfile " ^ path
  | LockfileReadFinished { path; duration_ms } ->
      "Read lockfile " ^ path ^ " in " ^ Int.to_string duration_ms ^ "ms"
  | LockfileReadFailed { path; error } ->
      "Failed to read lockfile " ^ path ^ ": " ^ Pm_error.message error
  | LockfileWriteStarted { path } -> "Writing lockfile " ^ path
  | LockfileWriteFinished { path; duration_ms } ->
      "Wrote lockfile " ^ path ^ " in " ^ Int.to_string duration_ms ^ "ms"
  | LockfileWriteFailed { path; error } ->
      "Failed to write lockfile " ^ path ^ ": " ^ Pm_error.message error
  | DependencyResolutionStarted { packages; mode } ->
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
  | DependencyResolutionUsingExistingLock { path } -> "Using existing lockfile " ^ path
  | DependencyResolutionRefreshingLock { path } -> "Refreshing lockfile " ^ path
  | DependencyResolutionUnlocking { path } -> (
      match path with
      | Some path -> "Unlocking dependency graph from " ^ path
      | None -> "Unlocking dependency graph"
    )
  | DependencyResolutionFinished { duration_ms; resolved_packages; resolved_edges } ->
      "Resolved "
      ^ Int.to_string resolved_packages
      ^ " packages and "
      ^ Int.to_string resolved_edges
      ^ " edges in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | DependencyResolutionFailed { error } ->
      "Dependency resolution failed: " ^ Pm_error.message error
  | RegistryIndexUpdating { registry } -> "Updating " ^ registry ^ " index"
  | DependencyUniverseBuilding { packages } ->
      "Building dependency universe for " ^ Int.to_string (List.length packages) ^ " packages"
  | DependencyUniverseBuilt {
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
  | PackageMetadataFetchStarted { registry; package } ->
      "Fetching package metadata for " ^ Package_name.to_string package ^ " from " ^ registry
  | PackageMetadataFetchFinished {
      registry;
      package;
      version;
      duration_ms;
    } ->
      (
          match version with
          | Some version ->
              "Fetched package metadata for "
              ^ Package_name.to_string package
              ^ "@"
              ^ version
              ^ " from "
              ^ registry
              ^ " in "
              ^ Int.to_string duration_ms
              ^ "ms"
          | None ->
              "Fetched package metadata for "
              ^ Package_name.to_string package
              ^ " from "
              ^ registry
              ^ " in "
              ^ Int.to_string duration_ms
              ^ "ms"
        )
  | PackageMetadataFetchFailed { registry; package; error } ->
      "Failed to fetch package metadata for "
      ^ Package_name.to_string package
      ^ " from "
      ^ registry
      ^ ": "
      ^ Pm_error.message error
  | SourceDependencyMaterializationStarted { source_locator; ref_ } -> (
      match ref_ with
      | Some ref_ -> "Materializing source dependency " ^ source_locator ^ "#" ^ ref_
      | None -> "Materializing source dependency " ^ source_locator
    )
  | SourceDependencyMaterializationFinished {
      source_locator = _;
      ref_ = _;
      package;
      version;
    } ->
      (
          match version with
          | Some version ->
              "Discovered source dependency " ^ Package_name.to_string package ^ "@" ^ version
          | None -> "Discovered source dependency " ^ Package_name.to_string package
        )
  | DependencyManifestUpdated {
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
  | PackageVersionLocked { package; version } ->
      "Locked " ^ Package_name.to_string package ^ " (" ^ version ^ ")"
  | PackageVersionsUnchanged { packages } ->
      "Dependencies are already up to date ("
      ^ Int.to_string packages
      ^ " locked packages unchanged)"
  | PackageVersionUpdated { package; from_version; to_version } ->
      "Updated " ^ Package_name.to_string package ^ " (" ^ from_version ^ " -> " ^ to_version ^ ")"
  | PackageManifestFetchStarted { package; version } ->
      "Fetching manifest for " ^ Package_name.to_string package ^ "@" ^ version
  | PackageManifestFetchFinished { package; version; duration_ms } ->
      "Fetched manifest for "
      ^ Package_name.to_string package
      ^ "@"
      ^ version
      ^ " in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | PackageManifestFetchFailed { package; version; error } -> (
      match version with
      | Some version ->
          "Failed to fetch manifest for "
          ^ Package_name.to_string package
          ^ "@"
          ^ version
          ^ ": "
          ^ Pm_error.message error
      | None ->
          "Failed to fetch manifest for "
          ^ Package_name.to_string package
          ^ ": "
          ^ Pm_error.message error
    )
  | PackageDownloadStarted { package; version; _ } ->
      "Downloading " ^ Package_name.to_string package ^ "@" ^ version
  | PackageDownloadFinished {
      package;
      version;
      path;
      duration_ms;
    } ->
      "Downloaded "
      ^ Package_name.to_string package
      ^ "@"
      ^ version
      ^ " to "
      ^ path
      ^ " in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | PackageDownloadFailed { package; version; error; _ } ->
      "Failed to download "
      ^ Package_name.to_string package
      ^ "@"
      ^ version
      ^ ": "
      ^ Pm_error.message error
  | PackageDownloadSkipped { package; version; reason; _ } ->
      "Skipped download for " ^ Package_name.to_string package ^ "@" ^ version ^ " (" ^ reason ^ ")"
  | PackageCacheHit { package; version; path } ->
      "Package cache hit for " ^ Package_name.to_string package ^ "@" ^ version ^ " at " ^ path
  | PackageMaterializationStarted { package; version; _ } ->
      "Materializing " ^ Package_name.to_string package ^ "@" ^ version
  | PackageMaterializationFinished {
      package;
      version;
      path;
      duration_ms;
    } ->
      "Materialized "
      ^ Package_name.to_string package
      ^ "@"
      ^ version
      ^ " at "
      ^ path
      ^ " in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | PackageMaterializationFailed { package; version; error; _ } ->
      "Failed to materialize "
      ^ Package_name.to_string package
      ^ "@"
      ^ version
      ^ ": "
      ^ Pm_error.message error
  | PackageResolvedForBuild {
      package;
      version;
      path;
      workspace;
    } ->
      (
          match version with
          | Some version ->
              "Resolved " ^ Package_name.to_string package ^ "@" ^ version ^ " for build at " ^ path ^ (
                if workspace then
                  " (workspace)"
                else
                  ""
              )
          | None ->
              "Resolved " ^ Package_name.to_string package ^ " for build at " ^ path ^ (
                if workspace then
                  " (workspace)"
                else
                  ""
              )
        )
  | PackageDownloadQueued { package; version; _ } ->
      "Queued download for " ^ Package_name.to_string package ^ "@" ^ version
  | BuildGraphCreating -> "Creating build graph..."
  | BuildGraphCreated { nodes; duration_ms } ->
      "Created build graph: "
      ^ Int.to_string nodes
      ^ " nodes in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | ServerShutdown -> "Server shutting down"
  | QueuePackage { package; queue_type } ->
      let typ =
        match queue_type with
        | `Ready -> "ready"
        | `Waiting -> "waiting"
      in
      "Queued " ^ Package_name.to_string package ^ " (" ^ typ ^ ")"
  | QueueStats { ready; waiting; busy } ->
      "Queue: "
      ^ Int.to_string ready
      ^ " ready, "
      ^ Int.to_string waiting
      ^ " waiting, "
      ^ Int.to_string busy
      ^ " busy"
  | DependencyMissing { package; missing } ->
      Package_name.to_string package
      ^ " waiting for: "
      ^ String.concat ", " (List.map missing ~fn:Package_name.to_string)
  | DependencySatisfied { package } -> Package_name.to_string package ^ " dependencies satisfied"
  | CompilingInterface { package; file } ->
      "[" ^ Package_name.to_string package ^ "] Compiling interface " ^ file
  | CompilingImplementation { package; file } ->
      "[" ^ Package_name.to_string package ^ "] Compiling " ^ file
  | LinkingLibrary { package; output } ->
      "[" ^ Package_name.to_string package ^ "] Linking library " ^ output
  | LinkingExecutable { package; output } ->
      "[" ^ Package_name.to_string package ^ "] Linking executable " ^ output
  | ComputingHash { package } -> "Computing hash for " ^ Package_name.to_string package
  | HashComputed { package; hash } -> "Hash for " ^ Package_name.to_string package ^ ": " ^ hash
  | CopyingFile { source; dest } -> "Copying " ^ source ^ " -> " ^ dest
  | WritingFile { path } -> "Writing " ^ path
  | CreatingDirectory { path } -> "Creating directory " ^ path
  | RpcRequestReceived { request_type; _ } -> "RPC request: " ^ request_type
  | RpcResponseSent { result } ->
      "RPC response sent (success: " ^ Bool.to_string
        (
          match result with
          | Ok _ -> true
          | Error _ -> false
        ) ^ ")"
  | McpToolCall { tool; _ } -> "MCP tool call: " ^ tool
  | StoreCreating -> "Creating build cache store"
  | StoreCreated { duration_ms } -> "Store created in " ^ Int.to_string duration_ms ^ "ms"
  | WorkerPoolCreating { workers } ->
      "Creating worker pool with " ^ Int.to_string workers ^ " workers"
  | WorkerPoolCreated { workers; duration_ms } ->
      "Worker pool created with "
      ^ Int.to_string workers
      ^ " workers in "
      ^ Int.to_string duration_ms
      ^ "ms"

(** Convert to human-readable string with timestamp *)
let to_string = fun event ->
  let timestamp = DateTime.to_iso8601 event.timestamp in
  let level_str =
    match event.level with
    | Error -> "[ERROR]"
    | Warn -> "[WARN]"
    | Info -> ""
    | Debug -> "[DEBUG]"
    | Trace -> "[TRACE]"
  in
  let msg = display event.kind in
  if level_str = "" then
    "[" ^ timestamp ^ "] " ^ msg
  else
    "[" ^ timestamp ^ "] " ^ level_str ^ " " ^ msg

let package_name_json = fun package -> Json.String (Package_name.to_string package)

let package_names_json = fun packages -> Json.Array (List.map packages ~fn:package_name_json)

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

let json_of_string_option = fun __tmp1 ->
  match __tmp1 with
  | Some value -> Json.String value
  | None -> Json.Null

let string_option_of_json = fun __tmp1 ->
  match __tmp1 with
  | Json.String value -> Some value
  | Json.Null -> None
  | _ -> None

let json_of_resolution_mode = fun __tmp1 ->
  match __tmp1 with
  | `Refresh -> Json.String "refresh"
  | `Unlock -> Json.String "unlock"

let resolution_mode_of_json = fun __tmp1 ->
  match __tmp1 with
  | Json.String "refresh" -> Some `Refresh
  | Json.String "unlock" -> Some `Unlock
  | _ -> None

let json_of_manifest_operation = fun __tmp1 ->
  match __tmp1 with
  | `Add -> Json.String "add"
  | `Remove -> Json.String "remove"

let manifest_operation_of_json = fun __tmp1 ->
  match __tmp1 with
  | Json.String "add" -> Some `Add
  | Json.String "remove" -> Some `Remove
  | _ -> None

(** Convert kind to JSON *)
let kind_to_json = fun __tmp1 ->
  match __tmp1 with
  | BuildComplete {
      duration_ms;
      results;
      succeeded;
      failed;
    } ->
      Json.Object [
        ("duration_ms", Json.Int duration_ms);
        ("succeeded", package_names_json succeeded);
        ("failed", package_names_json failed);
      ]
  | BuildGraphCreated { nodes; duration_ms } ->
      Json.Object [ ("nodes", Json.Int nodes); ("duration_ms", Json.Int duration_ms); ]
  | BuildGraphCreating -> Json.Object []
  | BuildStarted { packages; total_modules; workers } ->
      Json.Object [
        ("packages", package_names_json packages);
        ("total_modules", Json.Int total_modules);
        ("workers", Json.Int workers);
      ]
  | CacheHit { package; hash } ->
      Json.Object [ ("package", package_name_json package); ("hash", Json.String hash); ]
  | CacheMiss { package; hash } ->
      Json.Object [ ("package", package_name_json package); ("hash", Json.String hash); ]
  | PackageStarted { package } -> Json.Object [ ("package", package_name_json package); ]
  | PackageComplete {
      package;
      success;
      duration_ms;
      modules_compiled;
      cache_hits;
      cache_misses;
      _;
    } ->
      Json.Object [
        ("package", package_name_json package);
        ("success", Json.Bool success);
        ("duration_ms", Json.Int duration_ms);
        ("modules_compiled", Json.Int modules_compiled);
        ("cache_hits", Json.Int cache_hits);
        ("cache_misses", Json.Int cache_misses);
      ]
  | PackageSkipped { package; reason } ->
      let reason_json =
        match reason with
        | DependenciesFailed deps ->
            Json.Object [
              ("type", Json.String "dependencies_failed");
              ("dependencies", package_names_json deps);
            ]
      in
      Json.Object [ ("package", package_name_json package); ("reason", reason_json); ]
  | CompileError { package; error } ->
      let (col_start, col_end) = error.span in
      let error_message =
        match error.kind with
        | SyntaxError -> "Syntax error"
        | TypeError { description } -> strip_ansi_codes description
        | UnboundValue { name } -> "Unbound value " ^ name
        | UnboundModule { name } -> "Unbound module " ^ name
        | FileNotFound { filename } -> "Cannot find file " ^ filename
        | OtherError { message } -> strip_ansi_codes message
      in
      Json.Object [
        ("package", package_name_json package);
        ("file", Json.String error.file);
        ("line", Json.Int error.line);
        ("span", Json.Array [ Json.Int col_start; Json.Int col_end ]);
        ("message", Json.String (strip_ansi_codes error_message));
        ("hint", Json.String (strip_ansi_codes error.hint));
        ("raw", Json.String (strip_ansi_codes error.raw));
      ]
  | CacheStored { package; hash; artifacts } ->
      Json.Object [
        ("package", package_name_json package);
        ("hash", Json.String hash);
        ("artifacts", Json.Array (List.map artifacts ~fn:(fun a -> Json.String a)));
      ]
  | CompilingImplementation { package; file } ->
      Json.Object [ ("package", package_name_json package); ("file", Json.String file); ]
  | CompilingInterface { package; file } ->
      Json.Object [ ("package", package_name_json package); ("file", Json.String file); ]
  | ComputingHash { package } -> Json.Object [ ("package", package_name_json package); ]
  | CopyingFile { source; dest } ->
      Json.Object [ ("source", Json.String source); ("dest", Json.String dest); ]
  | CreatingDirectory { path } -> Json.Object [ ("path", Json.String path); ]
  | CycleDetected { packages } -> Json.Object [ ("packages", package_names_json packages); ]
  | DependencyMissing { package; missing } ->
      Json.Object [
        ("package", package_name_json package);
        ("missing", package_names_json missing);
      ]
  | DependencySatisfied { package } -> Json.Object [ ("package", package_name_json package); ]
  | HashComputed { package; hash } ->
      Json.Object [ ("package", package_name_json package); ("hash", Json.String hash); ]
  | StoreCreating -> Json.Object []
  | StoreCreated { duration_ms } -> Json.Object [ ("duration_ms", Json.Int duration_ms); ]
  | WorkerPoolCreating { workers } -> Json.Object [ ("workers", Json.Int workers); ]
  | WorkerPoolCreated { workers; duration_ms } ->
      Json.Object [ ("workers", Json.Int workers); ("duration_ms", Json.Int duration_ms); ]
  | LinkingExecutable { package; output } ->
      Json.Object [ ("package", package_name_json package); ("output", Json.String output); ]
  | LinkingLibrary { package; output } ->
      Json.Object [ ("package", package_name_json package); ("output", Json.String output); ]
  | McpToolCall { tool; args } -> Json.Object [ ("tool", Json.String tool); ("args", args); ]
  | QueuePackage { package; queue_type } ->
      Json.Object [
        ("package", package_name_json package);
        ("queue_type", Json.String (
          match queue_type with
          | `Ready -> "ready"
          | `Waiting -> "waiting"
        ));
      ]
  | QueueStats { ready; waiting; busy } ->
      Json.Object [
        ("ready", Json.Int ready);
        ("waiting", Json.Int waiting);
        ("busy", Json.Int busy);
      ]
  | RpcRequestReceived { request_type; args } ->
      Json.Object [ ("request_type", Json.String request_type); ("args", args); ]
  | RpcResponseSent { result } ->
      Json.Object [
        ("success", Json.Bool (
          match result with
          | Ok _ -> true
          | Error _ -> false
        ));
        ("error", match result with
        | Ok _ -> Json.Null
        | Error e -> Json.String e);
      ]
  | ServerRestarted { packages; toolchain } ->
      Json.Object [ ("packages", Json.Int packages); ("toolchain", Json.String toolchain); ]
  | ServerScanning { root } -> Json.Object [ ("root", Json.String root); ]
  | ServerShutdown -> Json.Object []
  | ServerStarted { pid } -> Json.Object [ ("pid", Json.String pid); ]
  | WorkerAssigned { worker_id; package } ->
      Json.Object [
        ("worker_id", Json.String (Worker_id.to_string worker_id));
        ("package", package_name_json package);
      ]
  | WorkerIdle { worker_id } ->
      Json.Object [ ("worker_id", Json.String (Worker_id.to_string worker_id)); ]
  | WorkerPoolStarted { workers } -> Json.Object [ ("workers", Json.Int workers); ]
  | WorkerStarted { worker_id } ->
      Json.Object [ ("worker_id", Json.String (Worker_id.to_string worker_id)); ]
  | WorkspaceEmpty -> Json.Object []
  | WorkspaceScanning -> Json.Object []
  | WorkspaceScanned { packages; duration_ms } ->
      Json.Object [ ("packages", Json.Int packages); ("duration_ms", Json.Int duration_ms); ]
  | LockfileReadStarted { path } -> Json.Object [ ("path", Json.String path); ]
  | LockfileReadFinished { path; duration_ms } ->
      Json.Object [ ("path", Json.String path); ("duration_ms", Json.Int duration_ms); ]
  | LockfileReadFailed { path; error } ->
      Json.Object [ ("path", Json.String path); ("error", Pm_error.to_json error); ]
  | LockfileWriteStarted { path } -> Json.Object [ ("path", Json.String path); ]
  | LockfileWriteFinished { path; duration_ms } ->
      Json.Object [ ("path", Json.String path); ("duration_ms", Json.Int duration_ms); ]
  | LockfileWriteFailed { path; error } ->
      Json.Object [ ("path", Json.String path); ("error", Pm_error.to_json error); ]
  | DependencyResolutionStarted { packages; mode } ->
      Json.Object [
        ("packages", package_names_json packages);
        ("mode", json_of_resolution_mode mode);
      ]
  | DependencyResolutionUsingExistingLock { path } -> Json.Object [ ("path", Json.String path); ]
  | DependencyResolutionRefreshingLock { path } -> Json.Object [ ("path", Json.String path); ]
  | DependencyResolutionUnlocking { path } -> Json.Object [ ("path", json_of_string_option path); ]
  | DependencyResolutionFinished { duration_ms; resolved_packages; resolved_edges } ->
      Json.Object [
        ("duration_ms", Json.Int duration_ms);
        ("resolved_packages", Json.Int resolved_packages);
        ("resolved_edges", Json.Int resolved_edges);
      ]
  | DependencyResolutionFailed { error } -> Json.Object [ ("error", Pm_error.to_json error); ]
  | RegistryIndexUpdating { registry } -> Json.Object [ ("registry", Json.String registry); ]
  | DependencyUniverseBuilding { packages } ->
      Json.Object [ ("packages", package_names_json packages); ]
  | DependencyUniverseBuilt {
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
  | PackageMetadataFetchStarted { registry; package } ->
      Json.Object [ ("registry", Json.String registry); ("package", package_name_json package); ]
  | PackageMetadataFetchFinished {
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
  | PackageMetadataFetchFailed { registry; package; error } ->
      Json.Object [
        ("registry", Json.String registry);
        ("package", package_name_json package);
        ("error", Pm_error.to_json error);
      ]
  | SourceDependencyMaterializationStarted { source_locator; ref_ } ->
      Json.Object [
        ("source_locator", Json.String source_locator);
        ("ref", json_of_string_option ref_);
      ]
  | SourceDependencyMaterializationFinished {
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
  | DependencyManifestUpdated {
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
  | PackageVersionLocked { package; version } ->
      Json.Object [ ("package", package_name_json package); ("version", Json.String version); ]
  | PackageVersionsUnchanged { packages } -> Json.Object [ ("packages", Json.Int packages); ]
  | PackageVersionUpdated { package; from_version; to_version } ->
      Json.Object [
        ("package", package_name_json package);
        ("from_version", Json.String from_version);
        ("to_version", Json.String to_version);
      ]
  | PackageManifestFetchStarted { package; version } ->
      Json.Object [ ("package", package_name_json package); ("version", Json.String version); ]
  | PackageManifestFetchFinished { package; version; duration_ms } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", Json.String version);
        ("duration_ms", Json.Int duration_ms);
      ]
  | PackageManifestFetchFailed { package; version; error } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", json_of_string_option version);
        ("error", Pm_error.to_json error);
      ]
  | PackageDownloadStarted { package; version; path } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", Json.String version);
        ("path", Json.String path);
      ]
  | PackageDownloadFinished {
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
  | PackageDownloadFailed {
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
  | PackageDownloadSkipped {
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
  | PackageCacheHit { package; version; path } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", Json.String version);
        ("path", Json.String path);
      ]
  | PackageMaterializationStarted { package; version; path } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", Json.String version);
        ("path", Json.String path);
      ]
  | PackageMaterializationFinished {
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
  | PackageMaterializationFailed {
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
  | PackageResolvedForBuild {
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
  | PackageDownloadQueued { package; version; path } ->
      Json.Object [
        ("package", package_name_json package);
        ("version", Json.String version);
        ("path", Json.String path);
      ]
  | WritingFile { path } -> Json.Object [ ("path", Json.String path); ]

(** Convert event to JSON *)
let to_json = fun event ->
  let timestamp = DateTime.to_iso8601 event.timestamp in
  (* Strip ANSI codes from the event before converting to JSON *)
  let clean_event =
    match event.kind with
    | CompileError { package; error } ->
        let clean_error = {
          error with
          raw = strip_ansi_codes error.raw;
          hint = strip_ansi_codes error.hint;
        }
        in
        { event with kind = CompileError { package; error = clean_error } }
    | _ -> event
  in
  Json.Object [
    ("timestamp", Json.String timestamp);
    ("session_id", Json.String (Session_id.to_string event.session_id));
    ("level", Json.String (level_to_string event.level));
    ("event", Json.String (name clean_event.kind));
    ("message", Json.String (strip_ansi_codes (display clean_event.kind)));
    ("data", kind_to_json clean_event.kind);
  ]

(** Convert kind from JSON *)
let kind_from_json = fun json ->
  match json with
  | Json.Object fields -> (
      match Fields.get "event" fields with
      | Some (Json.String event_name) -> (
          let data =
            Fields.get "data" fields
            |> Option.unwrap_or ~default:(Json.Object [])
          in
          match event_name with
          | "riot.build.completed" -> (
              match data with
              | Json.Object data_fields ->
                  let duration_ms =
                    match Fields.get "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let* succeeded =
                    match Fields.get "succeeded" data_fields with
                    | Some json -> package_names_of_json json
                    | None -> Ok []
                  in
                  let* failed =
                    match Fields.get "failed" data_fields with
                    | Some json -> package_names_of_json json
                    | None -> Ok []
                  in
                  Ok (
                    BuildComplete {
                      duration_ms;
                      results = [];
                      succeeded;
                      failed;
                    }
                  )
              | _ -> Error "Invalid BuildComplete data"
            )
          | "riot.build.started" -> (
              match data with
              | Json.Object data_fields ->
                  let* packages =
                    match Fields.get "packages" data_fields with
                    | Some json -> package_names_of_json json
                    | None -> Ok []
                  in
                  let total_modules =
                    match Fields.get "total_modules" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let workers =
                    match Fields.get "workers" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (BuildStarted { packages; total_modules; workers })
              | _ -> Error "Invalid BuildStarted data"
            )
          | "riot.build.package.started" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid PackageStarted data"
                  in
                  Ok (PackageStarted { package })
              | _ -> Error "Invalid PackageStarted data"
            )
          | "riot.build.package.completed" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid PackageComplete data"
                  in
                  let success =
                    match Fields.get "success" data_fields with
                    | Some (Json.Bool b) -> b
                    | _ -> false
                  in
                  let duration_ms =
                    match Fields.get "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let modules_compiled =
                    match Fields.get "modules_compiled" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let cache_hits =
                    match Fields.get "cache_hits" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let cache_misses =
                    match Fields.get "cache_misses" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (
                    PackageComplete {
                      package;
                      success;
                      duration_ms;
                      modules_compiled;
                      cache_hits;
                      cache_misses;
                      errors = [];
                    }
                  )
              | _ -> Error "Invalid PackageComplete data"
            )
          | "riot.build.package.skipped" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid PackageSkipped data"
                  in
                  let reason =
                    match Fields.get "reason" data_fields with
                    | Some (Json.Object reason_fields) -> (
                        match Fields.get "type" reason_fields with
                        | Some (Json.String "dependencies_failed") -> (
                            match Fields.get "dependencies" reason_fields with
                            | Some json -> (
                                match package_names_of_json json with
                                | Ok dep_names -> DependenciesFailed dep_names
                                | Error _ -> DependenciesFailed []
                              )
                            | None -> DependenciesFailed []
                          )
                        | _ -> DependenciesFailed []
                      )
                    | _ -> DependenciesFailed []
                  in
                  Ok (PackageSkipped { package; reason })
              | _ -> Error "Invalid PackageSkipped data"
            )
          | "riot.build.cache.hit" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid CacheHit data"
                  in
                  let hash =
                    match Fields.get "hash" data_fields with
                    | Some (Json.String h) -> h
                    | _ -> ""
                  in
                  Ok (CacheHit { package; hash })
              | _ -> Error "Invalid CacheHit data"
            )
          | "riot.build.cache.miss" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid CacheMiss data"
                  in
                  let hash =
                    match Fields.get "hash" data_fields with
                    | Some (Json.String h) -> h
                    | _ -> ""
                  in
                  Ok (CacheMiss { package; hash })
              | _ -> Error "Invalid CacheMiss data"
            )
          | "riot.build.cache.stored" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid CacheStored data"
                  in
                  let hash =
                    match Fields.get "hash" data_fields with
                    | Some (Json.String h) -> h
                    | _ -> ""
                  in
                  let artifacts =
                    match Fields.get "artifacts" data_fields with
                    | Some (Json.Array arr) ->
                        List.filter_map
                          ~fn:(fun __tmp1 ->
                            match __tmp1 with
                            | Json.String s -> Some s
                            | _ -> None)
                          arr
                    | _ -> []
                  in
                  Ok (CacheStored { package; hash; artifacts })
              | _ -> Error "Invalid CacheStored data"
            )
          | "riot.build.compile.interface" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid CompilingInterface data"
                  in
                  let file =
                    match Fields.get "file" data_fields with
                    | Some (Json.String f) -> f
                    | _ -> ""
                  in
                  Ok (CompilingInterface { package; file })
              | _ -> Error "Invalid CompilingInterface data"
            )
          | "riot.build.compile.implementation" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid CompilingImplementation data"
                  in
                  let file =
                    match Fields.get "file" data_fields with
                    | Some (Json.String f) -> f
                    | _ -> ""
                  in
                  Ok (CompilingImplementation { package; file })
              | _ -> Error "Invalid CompilingImplementation data"
            )
          | "riot.build.compile.error" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid CompileError data"
                  in
                  let file =
                    match Fields.get "file" data_fields with
                    | Some (Json.String f) -> f
                    | _ -> ""
                  in
                  let line =
                    match Fields.get "line" data_fields with
                    | Some (Json.Int l) -> l
                    | _ -> 0
                  in
                  let span =
                    match Fields.get "span" data_fields with
                    | Some (Json.Array [ Json.Int start; Json.Int end_ ]) -> (start, end_)
                    | _ -> (0, 0)
                  in
                  let message =
                    match Fields.get "message" data_fields with
                    | Some (Json.String m) -> m
                    | _ -> ""
                  in
                  let hint =
                    match Fields.get "hint" data_fields with
                    | Some (Json.String h) -> Some h
                    | _ -> None
                  in
                  let raw =
                    match Fields.get "raw" data_fields with
                    | Some (Json.String r) -> r
                    | _ -> message
                  in
                  let hint_str =
                    match hint with
                    | Some h -> h
                    | None -> ""
                  in
                  (* Try to parse error kind from message *)
                  let error_kind =
                    if message = "Syntax error" then
                      SyntaxError
                    else if String.starts_with ~prefix:"Unbound value " message then
                      UnboundValue {
                        name = String.sub message ~offset:14 ~len:(String.length message - 14);
                      }
                    else if String.starts_with ~prefix:"Unbound module " message then
                      UnboundModule {
                        name = String.sub message ~offset:15 ~len:(String.length message - 15);
                      }
                    else if String.starts_with ~prefix:"Cannot find file " message then
                      FileNotFound {
                        filename = String.sub message ~offset:17 ~len:(String.length message - 17);
                      }
                    else
                      OtherError { message }
                  in
                  Ok (
                    CompileError {
                      package;
                      error =
                        {
                          file;
                          line;
                          span;
                          hint = hint_str;
                          kind = error_kind;
                          raw;
                        };
                    }
                  )
              | _ -> Error "Invalid CompileError data"
            )
          | "riot.build.link.library" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid LinkingLibrary data"
                  in
                  let output =
                    match Fields.get "output" data_fields with
                    | Some (Json.String o) -> o
                    | _ -> ""
                  in
                  Ok (LinkingLibrary { package; output })
              | _ -> Error "Invalid LinkingLibrary data"
            )
          | "riot.build.hash.computing" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid ComputingHash data"
                  in
                  Ok (ComputingHash { package })
              | _ -> Error "Invalid ComputingHash data"
            )
          | "riot.build.hash.computed" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid HashComputed data"
                  in
                  let hash =
                    match Fields.get "hash" data_fields with
                    | Some (Json.String h) -> h
                    | _ -> ""
                  in
                  Ok (HashComputed { package; hash })
              | _ -> Error "Invalid HashComputed data"
            )
          | "riot.build.link.executable" -> (
              match data with
              | Json.Object data_fields ->
                  let* package =
                    match Fields.get "package" data_fields with
                    | Some json -> package_name_of_json json
                    | None -> Error "Invalid LinkingExecutable data"
                  in
                  let output =
                    match Fields.get "output" data_fields with
                    | Some (Json.String o) -> o
                    | _ -> ""
                  in
                  Ok (LinkingExecutable { package; output })
              | _ -> Error "Invalid LinkingExecutable data"
            )
          | "riot.workspace.scanning" -> Ok WorkspaceScanning
          | "riot.workspace.scanned" -> (
              match data with
              | Json.Object data_fields ->
                  let packages =
                    match Fields.get "packages" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let duration_ms =
                    match Fields.get "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (WorkspaceScanned { packages; duration_ms })
              | _ -> Error "Invalid WorkspaceScanned data"
            )
          | "riot.build_graph.creating" -> Ok BuildGraphCreating
          | "riot.build_graph.created" -> (
              match data with
              | Json.Object data_fields ->
                  let nodes =
                    match Fields.get "nodes" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let duration_ms =
                    match Fields.get "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (BuildGraphCreated { nodes; duration_ms })
              | _ -> Error "Invalid BuildGraphCreated data"
            )
          | "riot.store.creating" -> Ok StoreCreating
          | "riot.store.created" -> (
              match data with
              | Json.Object data_fields ->
                  let duration_ms =
                    match Fields.get "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (StoreCreated { duration_ms })
              | _ -> Error "Invalid StoreCreated data"
            )
          | "riot.worker_pool.creating" -> (
              match data with
              | Json.Object data_fields ->
                  let workers =
                    match Fields.get "workers" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (WorkerPoolCreating { workers })
              | _ -> Error "Invalid WorkerPoolCreating data"
            )
          | "riot.worker_pool.created" -> (
              match data with
              | Json.Object data_fields ->
                  let workers =
                    match Fields.get "workers" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let duration_ms =
                    match Fields.get "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (WorkerPoolCreated { workers; duration_ms })
              | _ -> Error "Invalid WorkerPoolCreated data"
            )
          | "riot.pm.lockfile.read.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match Fields.get "path" data_fields with
                  | Some (Json.String path) -> Ok (LockfileReadStarted { path })
                  | _ -> Error "Invalid LockfileReadStarted data"
                )
              | _ -> Error "Invalid LockfileReadStarted data"
            )
          | "riot.pm.lockfile.read.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match (Fields.get "path" data_fields, Fields.get "duration_ms" data_fields) with
                  | (Some (Json.String path), Some (Json.Int duration_ms)) ->
                      Ok (LockfileReadFinished { path; duration_ms })
                  | _ -> Error "Invalid LockfileReadFinished data"
                )
              | _ -> Error "Invalid LockfileReadFinished data"
            )
          | "riot.pm.lockfile.read.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match (Fields.get "path" data_fields, Fields.get "error" data_fields) with
                  | (Some (Json.String path), Some error_json) -> (
                      match Pm_error.from_json error_json with
                      | Ok error -> Ok (LockfileReadFailed { path; error })
                      | Error err -> Error ("Invalid LockfileReadFailed data: " ^ err)
                    )
                  | _ -> Error "Invalid LockfileReadFailed data"
                )
              | _ -> Error "Invalid LockfileReadFailed data"
            )
          | "riot.pm.lockfile.write.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match Fields.get "path" data_fields with
                  | Some (Json.String path) -> Ok (LockfileWriteStarted { path })
                  | _ -> Error "Invalid LockfileWriteStarted data"
                )
              | _ -> Error "Invalid LockfileWriteStarted data"
            )
          | "riot.pm.lockfile.write.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match (Fields.get "path" data_fields, Fields.get "duration_ms" data_fields) with
                  | (Some (Json.String path), Some (Json.Int duration_ms)) ->
                      Ok (LockfileWriteFinished { path; duration_ms })
                  | _ -> Error "Invalid LockfileWriteFinished data"
                )
              | _ -> Error "Invalid LockfileWriteFinished data"
            )
          | "riot.pm.lockfile.write.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match (Fields.get "path" data_fields, Fields.get "error" data_fields) with
                  | (Some (Json.String path), Some error_json) -> (
                      match Pm_error.from_json error_json with
                      | Ok error -> Ok (LockfileWriteFailed { path; error })
                      | Error err -> Error ("Invalid LockfileWriteFailed data: " ^ err)
                    )
                  | _ -> Error "Invalid LockfileWriteFailed data"
                )
              | _ -> Error "Invalid LockfileWriteFailed data"
            )
          | "riot.pm.resolution.started" -> (
              match data with
              | Json.Object data_fields ->
                  let* packages =
                    match Fields.get "packages" data_fields with
                    | Some json -> package_names_of_json json
                    | None -> Ok []
                  in
                  let mode =
                    match Fields.get "mode" data_fields with
                    | Some json -> (
                        match resolution_mode_of_json json with
                        | Some mode -> mode
                        | None -> `Refresh
                      )
                    | None -> `Refresh
                  in
                  Ok (DependencyResolutionStarted { packages; mode })
              | _ -> Error "Invalid DependencyResolutionStarted data"
            )
          | "riot.pm.resolution.using_existing_lock" -> (
              match data with
              | Json.Object data_fields -> (
                  match Fields.get "path" data_fields with
                  | Some (Json.String path) -> Ok (DependencyResolutionUsingExistingLock { path })
                  | _ -> Error "Invalid DependencyResolutionUsingExistingLock data"
                )
              | _ -> Error "Invalid DependencyResolutionUsingExistingLock data"
            )
          | "riot.pm.resolution.refreshing_lock" -> (
              match data with
              | Json.Object data_fields -> (
                  match Fields.get "path" data_fields with
                  | Some (Json.String path) -> Ok (DependencyResolutionRefreshingLock { path })
                  | _ -> Error "Invalid DependencyResolutionRefreshingLock data"
                )
              | _ -> Error "Invalid DependencyResolutionRefreshingLock data"
            )
          | "riot.pm.resolution.unlocking" -> (
              match data with
              | Json.Object data_fields ->
                  let path =
                    match Fields.get "path" data_fields with
                    | Some json -> string_option_of_json json
                    | None -> None
                  in
                  Ok (DependencyResolutionUnlocking { path })
              | _ -> Error "Invalid DependencyResolutionUnlocking data"
            )
          | "riot.pm.resolution.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "duration_ms" data_fields,
                    Fields.get "resolved_packages" data_fields,
                    Fields.get "resolved_edges" data_fields
                  ) with
                  | (
                      Some (Json.Int duration_ms),
                      Some (Json.Int resolved_packages),
                      Some (Json.Int resolved_edges)
                    ) ->
                      Ok (DependencyResolutionFinished {
                        duration_ms;
                        resolved_packages;
                        resolved_edges;
                      })
                  | _ -> Error "Invalid DependencyResolutionFinished data"
                )
              | _ -> Error "Invalid DependencyResolutionFinished data"
            )
          | "riot.pm.resolution.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match Fields.get "error" data_fields with
                  | Some error_json -> (
                      match Pm_error.from_json error_json with
                      | Ok error -> Ok (DependencyResolutionFailed { error })
                      | Error err -> Error ("Invalid DependencyResolutionFailed data: " ^ err)
                    )
                  | _ -> Error "Invalid DependencyResolutionFailed data"
                )
              | _ -> Error "Invalid DependencyResolutionFailed data"
            )
          | "riot.pm.registry.index.updating" -> (
              match data with
              | Json.Object data_fields -> (
                  match Fields.get "registry" data_fields with
                  | Some (Json.String registry) -> Ok (RegistryIndexUpdating { registry })
                  | _ -> Error "Invalid RegistryIndexUpdating data"
                )
              | _ -> Error "Invalid RegistryIndexUpdating data"
            )
          | "riot.pm.universe.building" -> (
              match data with
              | Json.Object data_fields ->
                  let* packages =
                    match Fields.get "packages" data_fields with
                    | Some json -> package_names_of_json json
                    | None -> Ok []
                  in
                  Ok (DependencyUniverseBuilding { packages })
              | _ -> Error "Invalid DependencyUniverseBuilding data"
            )
          | "riot.pm.universe.built" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "runtime_packages" data_fields,
                    Fields.get "build_packages" data_fields,
                    Fields.get "dev_packages" data_fields,
                    Fields.get "duration_ms" data_fields
                  ) with
                  | (
                      Some (Json.Int runtime_packages),
                      Some (Json.Int build_packages),
                      Some (Json.Int dev_packages),
                      Some (Json.Int duration_ms)
                    ) ->
                      Ok (
                        DependencyUniverseBuilt {
                          runtime_packages;
                          build_packages;
                          dev_packages;
                          duration_ms;
                        }
                      )
                  | _ -> Error "Invalid DependencyUniverseBuilt data"
                )
              | _ -> Error "Invalid DependencyUniverseBuilt data"
            )
          | "riot.pm.package_metadata.fetch.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match (Fields.get "registry" data_fields, Fields.get "package" data_fields) with
                  | (Some (Json.String registry), Some package_json) ->
                      let* package = package_name_of_json package_json in
                      Ok (PackageMetadataFetchStarted { registry; package })
                  | _ -> Error "Invalid PackageMetadataFetchStarted data"
                )
              | _ -> Error "Invalid PackageMetadataFetchStarted data"
            )
          | "riot.pm.package_metadata.fetch.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "registry" data_fields,
                    Fields.get "package" data_fields,
                    Fields.get "duration_ms" data_fields
                  ) with
                  | (Some (Json.String registry), Some package_json, Some (
                    Json.Int duration_ms
                  )) ->
                      let version =
                        match Fields.get "version" data_fields with
                        | Some json -> string_option_of_json json
                        | None -> None
                      in
                      let* package = package_name_of_json package_json in
                      Ok (
                        PackageMetadataFetchFinished {
                          registry;
                          package;
                          version;
                          duration_ms;
                        }
                      )
                  | _ -> Error "Invalid PackageMetadataFetchFinished data"
                )
              | _ -> Error "Invalid PackageMetadataFetchFinished data"
            )
          | "riot.pm.package_metadata.fetch.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "registry" data_fields,
                    Fields.get "package" data_fields,
                    Fields.get "error" data_fields
                  ) with
                  | (Some (Json.String registry), Some package_json, Some error_json) -> (
                      let* package = package_name_of_json package_json in
                      match Pm_error.from_json error_json with
                      | Ok error -> Ok (PackageMetadataFetchFailed { registry; package; error })
                      | Error err -> Error ("Invalid PackageMetadataFetchFailed data: " ^ err)
                    )
                  | _ -> Error "Invalid PackageMetadataFetchFailed data"
                )
              | _ -> Error "Invalid PackageMetadataFetchFailed data"
            )
          | "riot.pm.source_dependency.materialization.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match Fields.get "source_locator" data_fields with
                  | Some (Json.String source_locator) ->
                      let ref_ =
                        match Fields.get "ref" data_fields with
                        | Some json -> string_option_of_json json
                        | None -> None
                      in
                      Ok (SourceDependencyMaterializationStarted { source_locator; ref_ })
                  | _ -> Error "Invalid SourceDependencyMaterializationStarted data"
                )
              | _ -> Error "Invalid SourceDependencyMaterializationStarted data"
            )
          | "riot.pm.source_dependency.materialization.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match (Fields.get "source_locator" data_fields, Fields.get "package" data_fields) with
                  | (Some (Json.String source_locator), Some package_json) ->
                      let ref_ =
                        match Fields.get "ref" data_fields with
                        | Some json -> string_option_of_json json
                        | None -> None
                      in
                      let version =
                        match Fields.get "version" data_fields with
                        | Some json -> string_option_of_json json
                        | None -> None
                      in
                      let* package = package_name_of_json package_json in
                      Ok (
                        SourceDependencyMaterializationFinished {
                          source_locator;
                          ref_;
                          package;
                          version;
                        }
                      )
                  | _ -> Error "Invalid SourceDependencyMaterializationFinished data"
                )
              | _ -> Error "Invalid SourceDependencyMaterializationFinished data"
            )
          | "riot.pm.manifest.updated" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "path" data_fields,
                    Fields.get "section" data_fields,
                    Fields.get "operation" data_fields,
                    Fields.get "dependency" data_fields
                  ) with
                  | (
                      Some (Json.String path),
                      Some (Json.String section),
                      Some operation_json,
                      Some (Json.String dependency)
                    ) -> (
                      match manifest_operation_of_json operation_json with
                      | Some operation ->
                          Ok (
                            DependencyManifestUpdated {
                              path;
                              section;
                              operation;
                              dependency;
                            }
                          )
                      | None -> Error "Invalid DependencyManifestUpdated data"
                    )
                  | _ -> Error "Invalid DependencyManifestUpdated data"
                )
              | _ -> Error "Invalid DependencyManifestUpdated data"
            )
          | "riot.pm.package.locked" -> (
              match data with
              | Json.Object data_fields -> (
                  match (Fields.get "package" data_fields, Fields.get "version" data_fields) with
                  | (Some package_json, Some (Json.String version)) ->
                      let* package = package_name_of_json package_json in
                      Ok (PackageVersionLocked { package; version })
                  | _ -> Error "Invalid PackageVersionLocked data"
                )
              | _ -> Error "Invalid PackageVersionLocked data"
            )
          | "riot.pm.package.unchanged" -> (
              match data with
              | Json.Object data_fields -> (
                  match Fields.get "packages" data_fields with
                  | Some (Json.Int packages) -> Ok (PackageVersionsUnchanged { packages })
                  | _ -> Error "Invalid PackageVersionsUnchanged data"
                )
              | _ -> Error "Invalid PackageVersionsUnchanged data"
            )
          | "riot.pm.package.updated" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "from_version" data_fields,
                    Fields.get "to_version" data_fields
                  ) with
                  | (
                      Some package_json,
                      Some (Json.String from_version),
                      Some (Json.String to_version)
                    ) ->
                      let* package = package_name_of_json package_json in
                      Ok (PackageVersionUpdated { package; from_version; to_version })
                  | _ -> Error "Invalid PackageVersionUpdated data"
                )
              | _ -> Error "Invalid PackageVersionUpdated data"
            )
          | "riot.pm.package_manifest.fetch.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match (Fields.get "package" data_fields, Fields.get "version" data_fields) with
                  | (Some package_json, Some (Json.String version)) ->
                      let* package = package_name_of_json package_json in
                      Ok (PackageManifestFetchStarted { package; version })
                  | _ -> Error "Invalid PackageManifestFetchStarted data"
                )
              | _ -> Error "Invalid PackageManifestFetchStarted data"
            )
          | "riot.pm.package_manifest.fetch.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "version" data_fields,
                    Fields.get "duration_ms" data_fields
                  ) with
                  | (Some package_json, Some (Json.String version), Some (Json.Int duration_ms)) ->
                      let* package = package_name_of_json package_json in
                      Ok (PackageManifestFetchFinished { package; version; duration_ms })
                  | _ -> Error "Invalid PackageManifestFetchFinished data"
                )
              | _ -> Error "Invalid PackageManifestFetchFinished data"
            )
          | "riot.pm.package_manifest.fetch.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match (Fields.get "package" data_fields, Fields.get "error" data_fields) with
                  | (Some package_json, Some error_json) -> (
                      let version =
                        match Fields.get "version" data_fields with
                        | Some json -> string_option_of_json json
                        | None -> None
                      in
                      let* package = package_name_of_json package_json in
                      match Pm_error.from_json error_json with
                      | Ok error -> Ok (PackageManifestFetchFailed { package; version; error })
                      | Error err -> Error ("Invalid PackageManifestFetchFailed data: " ^ err)
                    )
                  | _ -> Error "Invalid PackageManifestFetchFailed data"
                )
              | _ -> Error "Invalid PackageManifestFetchFailed data"
            )
          | "riot.pm.package_download.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "version" data_fields,
                    Fields.get "path" data_fields
                  ) with
                  | (Some package_json, Some (Json.String version), Some (Json.String path)) ->
                      let* package = package_name_of_json package_json in
                      Ok (PackageDownloadStarted { package; version; path })
                  | _ -> Error "Invalid PackageDownloadStarted data"
                )
              | _ -> Error "Invalid PackageDownloadStarted data"
            )
          | "riot.pm.package_download.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "version" data_fields,
                    Fields.get "path" data_fields,
                    Fields.get "duration_ms" data_fields
                  ) with
                  | (
                      Some package_json,
                      Some (Json.String version),
                      Some (Json.String path),
                      Some (Json.Int duration_ms)
                    ) ->
                      let* package = package_name_of_json package_json in
                      Ok (
                        PackageDownloadFinished {
                          package;
                          version;
                          path;
                          duration_ms;
                        }
                      )
                  | _ -> Error "Invalid PackageDownloadFinished data"
                )
              | _ -> Error "Invalid PackageDownloadFinished data"
            )
          | "riot.pm.package_download.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "version" data_fields,
                    Fields.get "path" data_fields,
                    Fields.get "error" data_fields
                  ) with
                  | (
                      Some package_json,
                      Some (Json.String version),
                      Some (Json.String path),
                      Some error_json
                    ) -> (
                      let* package = package_name_of_json package_json in
                      match Pm_error.from_json error_json with
                      | Ok error ->
                          Ok (
                            PackageDownloadFailed {
                              package;
                              version;
                              path;
                              error;
                            }
                          )
                      | Error err -> Error ("Invalid PackageDownloadFailed data: " ^ err)
                    )
                  | _ -> Error "Invalid PackageDownloadFailed data"
                )
              | _ -> Error "Invalid PackageDownloadFailed data"
            )
          | "riot.pm.package_download.skipped" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "version" data_fields,
                    Fields.get "path" data_fields,
                    Fields.get "reason" data_fields
                  ) with
                  | (
                      Some package_json,
                      Some (Json.String version),
                      Some (Json.String path),
                      Some (Json.String reason)
                    ) ->
                      let* package = package_name_of_json package_json in
                      Ok (
                        PackageDownloadSkipped {
                          package;
                          version;
                          path;
                          reason;
                        }
                      )
                  | _ -> Error "Invalid PackageDownloadSkipped data"
                )
              | _ -> Error "Invalid PackageDownloadSkipped data"
            )
          | "riot.pm.package_cache.hit" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "version" data_fields,
                    Fields.get "path" data_fields
                  ) with
                  | (Some package_json, Some (Json.String version), Some (Json.String path)) ->
                      let* package = package_name_of_json package_json in
                      Ok (PackageCacheHit { package; version; path })
                  | _ -> Error "Invalid PackageCacheHit data"
                )
              | _ -> Error "Invalid PackageCacheHit data"
            )
          | "riot.pm.package_materialization.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "version" data_fields,
                    Fields.get "path" data_fields
                  ) with
                  | (Some package_json, Some (Json.String version), Some (Json.String path)) ->
                      let* package = package_name_of_json package_json in
                      Ok (PackageMaterializationStarted { package; version; path })
                  | _ -> Error "Invalid PackageMaterializationStarted data"
                )
              | _ -> Error "Invalid PackageMaterializationStarted data"
            )
          | "riot.pm.package_materialization.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "version" data_fields,
                    Fields.get "path" data_fields,
                    Fields.get "duration_ms" data_fields
                  ) with
                  | (
                      Some package_json,
                      Some (Json.String version),
                      Some (Json.String path),
                      Some (Json.Int duration_ms)
                    ) ->
                      let* package = package_name_of_json package_json in
                      Ok (
                        PackageMaterializationFinished {
                          package;
                          version;
                          path;
                          duration_ms;
                        }
                      )
                  | _ -> Error "Invalid PackageMaterializationFinished data"
                )
              | _ -> Error "Invalid PackageMaterializationFinished data"
            )
          | "riot.pm.package_materialization.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "version" data_fields,
                    Fields.get "path" data_fields,
                    Fields.get "error" data_fields
                  ) with
                  | (
                      Some package_json,
                      Some (Json.String version),
                      Some (Json.String path),
                      Some error_json
                    ) -> (
                      let* package = package_name_of_json package_json in
                      match Pm_error.from_json error_json with
                      | Ok error ->
                          Ok (
                            PackageMaterializationFailed {
                              package;
                              version;
                              path;
                              error;
                            }
                          )
                      | Error err -> Error ("Invalid PackageMaterializationFailed data: " ^ err)
                    )
                  | _ -> Error "Invalid PackageMaterializationFailed data"
                )
              | _ -> Error "Invalid PackageMaterializationFailed data"
            )
          | "riot.pm.package_resolved_for_build" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "path" data_fields,
                    Fields.get "workspace" data_fields
                  ) with
                  | (Some package_json, Some (Json.String path), Some (Json.Bool workspace)) ->
                      let version =
                        match Fields.get "version" data_fields with
                        | Some json -> string_option_of_json json
                        | None -> None
                      in
                      let* package = package_name_of_json package_json in
                      Ok (
                        PackageResolvedForBuild {
                          package;
                          version;
                          path;
                          workspace;
                        }
                      )
                  | _ -> Error "Invalid PackageResolvedForBuild data"
                )
              | _ -> Error "Invalid PackageResolvedForBuild data"
            )
          | "riot.pm.package_download.queued" -> (
              match data with
              | Json.Object data_fields -> (
                  match (
                    Fields.get "package" data_fields,
                    Fields.get "version" data_fields,
                    Fields.get "path" data_fields
                  ) with
                  | (Some package_json, Some (Json.String version), Some (Json.String path)) ->
                      let* package = package_name_of_json package_json in
                      Ok (PackageDownloadQueued { package; version; path })
                  | _ -> Error "Invalid PackageDownloadQueued data"
                )
              | _ -> Error "Invalid PackageDownloadQueued data"
            )
          | _ -> Error ("Unknown event type: " ^ event_name)
        )
      | _ -> Error "Missing event field"
    )
  | _ -> Error "Invalid JSON format"

(** Convert from JSON *)
let from_json = fun json ->
  match json with
  | Json.Object fields -> (
      let timestamp =
        match Fields.get "timestamp" fields with
        | Some (Json.String _ts) ->
            (* For now, use current time - proper timestamp parsing can be added later *)
            DateTime.now ()
        | _ -> DateTime.now ()
      in
      let session_id =
        match Fields.get "session_id" fields with
        | Some (Json.String s) -> Session_id.from_string s
        | _ -> Session_id.make ()
      in
      let level =
        match Fields.get "level" fields with
        | Some (Json.String "error") -> Error
        | Some (Json.String "warn") -> Warn
        | Some (Json.String "info") -> Info
        | Some (Json.String "debug") -> Debug
        | Some (Json.String "trace") -> Trace
        | _ -> Info
      in
      match kind_from_json json with
      | Ok kind ->
          Ok {
            timestamp;
            session_id;
            level;
            kind;
          }
      | Error e -> Error e
    )
  | _ -> Error "Invalid JSON format for Event"

module Tests = struct
  let package_name = fun name ->
    Result.expect
      (Package_name.from_string name)
      ~msg:("package name " ^ name)

  let test_lockfile_event_json_roundtrip () =
    let event =
      create
        ~session_id:(Session_id.from_string "test-session")
        ~level:Info
        (LockfileReadFinished { path = "/tmp/workspace/riot.lock"; duration_ms = 12 })
    in
    match from_json (to_json event) with
    | Ok { kind = LockfileReadFinished { path; duration_ms }; _ } ->
        if String.equal path "/tmp/workspace/riot.lock" && duration_ms = 12 then
          Ok ()
        else
          Error "expected lockfile read event to round-trip"
    | Ok _ -> Error "expected LockfileReadFinished after round-trip"
    | Error err -> Error err [@test]

  let test_resolution_event_json_roundtrip () =
    let event =
      create
        ~session_id:(Session_id.from_string "test-session")
        ~level:Info
        (
          DependencyResolutionStarted {
            packages = [ package_name "app"; package_name "std" ];
            mode = `Unlock;
          }
        )
    in
    match from_json (to_json event) with
    | Ok { kind = DependencyResolutionStarted { packages; mode = `Unlock }; _ } ->
        if packages = [ package_name "app"; package_name "std" ] then
          Ok ()
        else
          Error "expected dependency resolution packages to round-trip"
    | Ok _ -> Error "expected DependencyResolutionStarted unlock event after round-trip"
    | Error err -> Error err [@test]

  let test_package_resolved_event_json_roundtrip () =
    let event =
      create
        ~session_id:(Session_id.from_string "test-session")
        ~level:Info
        (
          PackageResolvedForBuild {
            package = package_name "std";
            version = Some "0.1.0";
            path = "/Users/example/.riot/registry/pkgs.ml/src/std/0.1.0";
            workspace = false;
          }
        )
    in
    match from_json (to_json event) with
    | Ok { kind = PackageResolvedForBuild {
                    package;
                    version;
                    path;
                    workspace;
                  }; _ } ->
        if
          Package_name.equal package (package_name "std")
          && version = Some "0.1.0"
          && String.equal path "/Users/example/.riot/registry/pkgs.ml/src/std/0.1.0"
          && not workspace
        then
          Ok ()
        else
          Error "expected package resolved event to round-trip"
    | Ok _ -> Error "expected PackageResolvedForBuild after round-trip"
    | Error err -> Error err [@test]

  let test_manifest_update_event_json_roundtrip () =
    let event =
      create
        ~session_id:(Session_id.from_string "test-session")
        ~level:Info
        (
          DependencyManifestUpdated {
            path = "/tmp/workspace/riot.toml";
            section = "dependencies";
            operation = `Add;
            dependency = "std";
          }
        )
    in
    match from_json (to_json event) with
    | Ok { kind = DependencyManifestUpdated {
                    path;
                    section;
                    operation = `Add;
                    dependency;
                  }; _ } ->
        if
          String.equal path "/tmp/workspace/riot.toml"
          && String.equal section "dependencies"
          && String.equal dependency "std"
        then
          Ok ()
        else
          Error "expected dependency manifest update event to round-trip"
    | Ok _ -> Error "expected DependencyManifestUpdated after round-trip"
    | Error err -> Error err [@test]

  let test_package_locked_event_json_roundtrip () =
    let event =
      create
        ~session_id:(Session_id.from_string "test-session")
        ~level:Info
        (PackageVersionLocked { package = package_name "std"; version = "0.2.0" })
    in
    match from_json (to_json event) with
    | Ok { kind = PackageVersionLocked { package; version }; _ } ->
        if Package_name.equal package (package_name "std") && String.equal version "0.2.0" then
          Ok ()
        else
          Error "expected package locked event to round-trip"
    | Ok _ -> Error "expected PackageVersionLocked after round-trip"
    | Error err -> Error err [@test]

  let test_package_versions_unchanged_event_json_roundtrip () =
    let event =
      create
        ~session_id:(Session_id.from_string "test-session")
        ~level:Info
        (PackageVersionsUnchanged { packages = 3 })
    in
    match from_json (to_json event) with
    | Ok { kind = PackageVersionsUnchanged { packages }; _ } ->
        if Int.equal packages 3 then
          Ok ()
        else
          Error "expected package versions unchanged event to round-trip"
    | Ok _ -> Error "expected PackageVersionsUnchanged after round-trip"
    | Error err -> Error err [@test]
end [@test]
