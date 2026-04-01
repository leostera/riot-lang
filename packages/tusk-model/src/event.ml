open Std
open Std.Data
open Std.Collections
(** Event system for tusk - pure data types for events *)

(** Strip ANSI escape codes from a string *)
let strip_ansi_codes = fun str ->
  (* ANSI escape codes pattern: ESC[...m where ESC is \027 *)
  let rec strip acc chars =
    match chars with
    | [] ->
        List.rev acc |> List.to_seq |> String.of_seq
    | '\027' :: '[' :: rest ->
        (* Found start of ANSI escape sequence, skip until 'm' *)
        let rec skip_until_m chars =
          match chars with
          | [] -> []
          | 'm' :: rest -> rest
          | _ :: rest -> skip_until_m rest
        in
        strip acc (skip_until_m rest)
    | c :: rest ->
        strip (c :: acc) rest
  in
  strip []
    (String.to_seq str |> List.of_seq)

type level =
  Error
  | Warn
  | Info
  | Debug
  | Trace

let level_to_string = function
  | Error -> "error"
  | Warn -> "warn"
  | Info -> "info"
  | Debug -> "debug"
  | Trace -> "trace"

type skip_reason =
  DependenciesFailed of string list

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
  span: int * int;  (* start, end character positions *)
  hint: string;  (* The source line with caret pointing to error *)
  kind: error_kind;
  raw: string;  (* Raw compiler output *)
}

type build_result = {
  package: string;
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
          `Ready
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
  | LockfileReadFailed of { path: string; error: string }
  | LockfileWriteStarted of { path: string }
  | LockfileWriteFinished of { path: string; duration_ms: int }
  | LockfileWriteFailed of { path: string; error: string }
  | DependencyResolutionStarted of {
      packages: string list;
      mode:
        [
          `Refresh
          | `Unlock
        ]
    }
  | DependencyResolutionUsingExistingLock of { path: string }
  | DependencyResolutionRefreshingLock of { path: string }
  | DependencyResolutionUnlocking of { path: string option }
  | DependencyResolutionFinished of {
      duration_ms: int;
      resolved_packages: int;
      resolved_edges: int
    }
  | DependencyResolutionFailed of { error: string }
  | DependencyUniverseBuilding of { packages: string list }
  | DependencyUniverseBuilt of {
      runtime_packages: int;
      build_packages: int;
      dev_packages: int;
      duration_ms: int
    }
  | PackageMetadataFetchStarted of { package: string }
  | PackageMetadataFetchFinished of {
      package: string;
      version: string option;
      duration_ms: int
    }
  | PackageMetadataFetchFailed of { package: string; error: string }
  | PackageManifestFetchStarted of { package: string; version: string }
  | PackageManifestFetchFinished of {
      package: string;
      version: string;
      duration_ms: int
    }
  | PackageManifestFetchFailed of {
      package: string;
      version: string option;
      error: string
    }
  | PackageDownloadStarted of { package: string; version: string; path: string }
  | PackageDownloadFinished of {
      package: string;
      version: string;
      path: string;
      duration_ms: int
    }
  | PackageDownloadFailed of {
      package: string;
      version: string;
      path: string;
      error: string
    }
  | PackageDownloadSkipped of {
      package: string;
      version: string;
      path: string;
      reason: string
    }
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
      error: string
    }
  | PackageResolvedForBuild of {
      package: string;
      version: string option;
      path: string;
      workspace: bool
    }
  | PackageDownloadQueued of { package: string; version: string; path: string }
  | WritingFile of { path: string }

type t = {
  timestamp: Datetime.t;
  session_id: Session_id.t;
  level: level;
  kind: kind;
}
(** Create a new event with current timestamp *)
let create = fun ~session_id ~level kind -> { timestamp = Datetime.now (); session_id; level; kind }
(** Format timestamp for display *)

(** Get the machine-readable event name *)
let name = function
  | BuildComplete _ -> "tusk.build.completed"
  | BuildGraphCreated _ -> "tusk.build_graph.created"
  | BuildGraphCreating -> "tusk.build_graph.creating"
  | BuildStarted _ -> "tusk.build.started"
  | CacheHit _ -> "tusk.build.cache.hit"
  | CacheMiss _ -> "tusk.build.cache.miss"
  | CacheStored _ -> "tusk.build.cache.stored"
  | CompileError _ -> "tusk.build.compile.error"
  | CompilingImplementation _ -> "tusk.build.compile.implementation"
  | CompilingInterface _ -> "tusk.build.compile.interface"
  | ComputingHash _ -> "tusk.build.hash.computing"
  | CopyingFile _ -> "tusk.build.file.copy"
  | CreatingDirectory _ -> "tusk.build.directory.create"
  | CycleDetected _ -> "tusk.build.cycle.detected"
  | DependencyMissing _ -> "tusk.build.dependency.missing"
  | DependencySatisfied _ -> "tusk.build.dependency.satisfied"
  | HashComputed _ -> "tusk.build.hash.computed"
  | LinkingExecutable _ -> "tusk.build.link.executable"
  | LinkingLibrary _ -> "tusk.build.link.library"
  | McpToolCall _ -> "tusk.mcp.tool_call"
  | PackageComplete _ -> "tusk.build.package.completed"
  | PackageSkipped _ -> "tusk.build.package.skipped"
  | PackageStarted _ -> "tusk.build.package.started"
  | QueuePackage _ -> "tusk.build.queue.package"
  | QueueStats _ -> "tusk.build.queue.stats"
  | RpcRequestReceived _ -> "tusk.rpc.request.received"
  | RpcResponseSent _ -> "tusk.rpc.response.sent"
  | ServerRestarted _ -> "tusk.server.restarted"
  | ServerScanning _ -> "tusk.server.scanning"
  | ServerShutdown -> "tusk.server.shutdown"
  | ServerStarted _ -> "tusk.server.started"
  | WorkerAssigned _ -> "tusk.build.worker.assigned"
  | WorkerIdle _ -> "tusk.build.worker.idle"
  | WorkerPoolStarted _ -> "tusk.build.worker_pool.started"
  | WorkerStarted _ -> "tusk.build.worker.started"
  | WorkspaceEmpty -> "tusk.workspace.empty"
  | WorkspaceScanned _ -> "tusk.workspace.scanned"
  | WorkspaceScanning -> "tusk.workspace.scanning"
  | LockfileReadStarted _ -> "tusk.pm.lockfile.read.started"
  | LockfileReadFinished _ -> "tusk.pm.lockfile.read.finished"
  | LockfileReadFailed _ -> "tusk.pm.lockfile.read.failed"
  | LockfileWriteStarted _ -> "tusk.pm.lockfile.write.started"
  | LockfileWriteFinished _ -> "tusk.pm.lockfile.write.finished"
  | LockfileWriteFailed _ -> "tusk.pm.lockfile.write.failed"
  | DependencyResolutionStarted _ -> "tusk.pm.resolution.started"
  | DependencyResolutionUsingExistingLock _ -> "tusk.pm.resolution.using_existing_lock"
  | DependencyResolutionRefreshingLock _ -> "tusk.pm.resolution.refreshing_lock"
  | DependencyResolutionUnlocking _ -> "tusk.pm.resolution.unlocking"
  | DependencyResolutionFinished _ -> "tusk.pm.resolution.finished"
  | DependencyResolutionFailed _ -> "tusk.pm.resolution.failed"
  | DependencyUniverseBuilding _ -> "tusk.pm.universe.building"
  | DependencyUniverseBuilt _ -> "tusk.pm.universe.built"
  | PackageMetadataFetchStarted _ -> "tusk.pm.package_metadata.fetch.started"
  | PackageMetadataFetchFinished _ -> "tusk.pm.package_metadata.fetch.finished"
  | PackageMetadataFetchFailed _ -> "tusk.pm.package_metadata.fetch.failed"
  | PackageManifestFetchStarted _ -> "tusk.pm.package_manifest.fetch.started"
  | PackageManifestFetchFinished _ -> "tusk.pm.package_manifest.fetch.finished"
  | PackageManifestFetchFailed _ -> "tusk.pm.package_manifest.fetch.failed"
  | PackageDownloadStarted _ -> "tusk.pm.package_download.started"
  | PackageDownloadFinished _ -> "tusk.pm.package_download.finished"
  | PackageDownloadFailed _ -> "tusk.pm.package_download.failed"
  | PackageDownloadSkipped _ -> "tusk.pm.package_download.skipped"
  | PackageCacheHit _ -> "tusk.pm.package_cache.hit"
  | PackageMaterializationStarted _ -> "tusk.pm.package_materialization.started"
  | PackageMaterializationFinished _ -> "tusk.pm.package_materialization.finished"
  | PackageMaterializationFailed _ -> "tusk.pm.package_materialization.failed"
  | PackageResolvedForBuild _ -> "tusk.pm.package_resolved_for_build"
  | PackageDownloadQueued _ -> "tusk.pm.package_download.queued"
  | WritingFile _ -> "tusk.build.file.write"
  | StoreCreating -> "tusk.store.creating"
  | StoreCreated _ -> "tusk.store.created"
  | WorkerPoolCreating _ -> "tusk.worker_pool.creating"
  | WorkerPoolCreated _ -> "tusk.worker_pool.created"
(** Get human-readable display message *)
let display = function
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
  | PackageStarted { package } ->
      "Building " ^ package ^ "..."
  | PackageComplete { package; success; duration_ms; _ } ->
      if success then
        "✓ Built " ^ package ^ " in " ^ Int.to_string duration_ms ^ "ms"
      else
        "✗ Failed to build " ^ package
  | PackageSkipped { package; reason } ->
      let reason_str =
        match reason with
        | DependenciesFailed deps -> "dependencies failed: " ^ String.concat ", " deps
      in
      "⊘ Skipped " ^ package ^ " (" ^ reason_str ^ ")"
  | CompileError { package; error } ->
      let col_start, _ = error.span in
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
      ^ package
      ^ " ["
      ^ error.file
      ^ ":"
      ^ Int.to_string error.line
      ^ ":"
      ^ Int.to_string col_start
      ^ "]: "
      ^ kind_msg
  | CycleDetected { packages } ->
      "Circular dependency detected: " ^ String.concat " -> " packages
  | CacheHit { package; _ } ->
      "Cached " ^ package
  | CacheMiss { package; _ } ->
      "Cache miss for " ^ package
  | CacheStored { package; artifacts; _ } ->
      "Cached " ^ package ^ " (" ^ Int.to_string (List.length artifacts) ^ " artifacts)"
  | WorkerPoolStarted { workers } ->
      "Started worker pool with " ^ Int.to_string workers ^ " workers"
  | WorkerStarted { worker_id } ->
      "Worker " ^ Worker_id.to_string worker_id ^ " started"
  | WorkerAssigned { worker_id; package } ->
      "Worker " ^ Worker_id.to_string worker_id ^ " assigned to " ^ package
  | WorkerIdle { worker_id } ->
      "Worker " ^ Worker_id.to_string worker_id ^ " idle"
  | ServerStarted { pid } ->
      "Server started (pid: " ^ pid ^ ")"
  | ServerScanning { root } ->
      "Scanning workspace: " ^ root
  | ServerRestarted { packages; toolchain } ->
      "Server restarted with " ^ Int.to_string packages ^ " packages (toolchain: " ^ toolchain ^ ")"
  | WorkspaceEmpty ->
      "No packages found in workspace"
  | WorkspaceScanning ->
      "Scanning workspace..."
  | WorkspaceScanned { packages; duration_ms } ->
      "Scanned workspace: "
      ^ Int.to_string packages
      ^ " packages in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | LockfileReadStarted { path } ->
      "Reading lockfile " ^ path
  | LockfileReadFinished { path; duration_ms } ->
      "Read lockfile " ^ path ^ " in " ^ Int.to_string duration_ms ^ "ms"
  | LockfileReadFailed { path; error } ->
      "Failed to read lockfile " ^ path ^ ": " ^ error
  | LockfileWriteStarted { path } ->
      "Writing lockfile " ^ path
  | LockfileWriteFinished { path; duration_ms } ->
      "Wrote lockfile " ^ path ^ " in " ^ Int.to_string duration_ms ^ "ms"
  | LockfileWriteFailed { path; error } ->
      "Failed to write lockfile " ^ path ^ ": " ^ error
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
  | DependencyResolutionUsingExistingLock { path } ->
      "Using existing lockfile " ^ path
  | DependencyResolutionRefreshingLock { path } ->
      "Refreshing lockfile " ^ path
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
      "Dependency resolution failed: " ^ error
  | DependencyUniverseBuilding { packages } ->
      "Building dependency universe for " ^ Int.to_string (List.length packages) ^ " packages"
  | DependencyUniverseBuilt { runtime_packages; build_packages; dev_packages; duration_ms } ->
      "Built dependency universe in "
      ^ Int.to_string duration_ms
      ^ "ms (runtime="
      ^ Int.to_string runtime_packages
      ^ ", build="
      ^ Int.to_string build_packages
      ^ ", dev="
      ^ Int.to_string dev_packages
      ^ ")"
  | PackageMetadataFetchStarted { package } ->
      "Fetching package metadata for " ^ package
  | PackageMetadataFetchFinished { package; version; duration_ms } -> (
      match version with
      | Some version ->
          "Fetched package metadata for "
          ^ package
          ^ "@"
          ^ version
          ^ " in "
          ^ Int.to_string duration_ms
          ^ "ms"
      | None ->
          "Fetched package metadata for "
          ^ package
          ^ " in "
          ^ Int.to_string duration_ms
          ^ "ms"
    )
  | PackageMetadataFetchFailed { package; error } ->
      "Failed to fetch package metadata for " ^ package ^ ": " ^ error
  | PackageManifestFetchStarted { package; version } ->
      "Fetching manifest for " ^ package ^ "@" ^ version
  | PackageManifestFetchFinished { package; version; duration_ms } ->
      "Fetched manifest for "
      ^ package
      ^ "@"
      ^ version
      ^ " in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | PackageManifestFetchFailed { package; version; error } -> (
      match version with
      | Some version -> "Failed to fetch manifest for " ^ package ^ "@" ^ version ^ ": " ^ error
      | None -> "Failed to fetch manifest for " ^ package ^ ": " ^ error
    )
  | PackageDownloadStarted { package; version; _ } ->
      "Downloading " ^ package ^ "@" ^ version
  | PackageDownloadFinished { package; version; path; duration_ms } ->
      "Downloaded "
      ^ package
      ^ "@"
      ^ version
      ^ " to "
      ^ path
      ^ " in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | PackageDownloadFailed { package; version; error; _ } ->
      "Failed to download " ^ package ^ "@" ^ version ^ ": " ^ error
  | PackageDownloadSkipped { package; version; reason; _ } ->
      "Skipped download for " ^ package ^ "@" ^ version ^ " (" ^ reason ^ ")"
  | PackageCacheHit { package; version; path } ->
      "Package cache hit for " ^ package ^ "@" ^ version ^ " at " ^ path
  | PackageMaterializationStarted { package; version; _ } ->
      "Materializing " ^ package ^ "@" ^ version
  | PackageMaterializationFinished { package; version; path; duration_ms } ->
      "Materialized "
      ^ package
      ^ "@"
      ^ version
      ^ " at "
      ^ path
      ^ " in "
      ^ Int.to_string duration_ms
      ^ "ms"
  | PackageMaterializationFailed { package; version; error; _ } ->
      "Failed to materialize " ^ package ^ "@" ^ version ^ ": " ^ error
  | PackageResolvedForBuild { package; version; path; workspace } -> (
      match version with
      | Some version ->
          "Resolved "
          ^ package
          ^ "@"
          ^ version
          ^ " for build at "
          ^ path
          ^ (if workspace then " (workspace)" else "")
      | None ->
          "Resolved "
          ^ package
          ^ " for build at "
          ^ path
          ^ (if workspace then " (workspace)" else "")
    )
  | PackageDownloadQueued { package; version; _ } ->
      "Queued download for " ^ package ^ "@" ^ version
  | BuildGraphCreating ->
      "Creating build graph..."
  | BuildGraphCreated { nodes; duration_ms } ->
      "Created build graph: " ^ Int.to_string nodes ^ " nodes in " ^ Int.to_string duration_ms ^ "ms"
  | ServerShutdown ->
      "Server shutting down"
  | QueuePackage { package; queue_type } ->
      let typ =
        match queue_type with
        | `Ready -> "ready"
        | `Waiting -> "waiting"
      in
      "Queued " ^ package ^ " (" ^ typ ^ ")"
  | QueueStats { ready; waiting; busy } ->
      "Queue: "
      ^ Int.to_string ready
      ^ " ready, "
      ^ Int.to_string waiting
      ^ " waiting, "
      ^ Int.to_string busy
      ^ " busy"
  | DependencyMissing { package; missing } ->
      package ^ " waiting for: " ^ String.concat ", " missing
  | DependencySatisfied { package } ->
      package ^ " dependencies satisfied"
  | CompilingInterface { package; file } ->
      "[" ^ package ^ "] Compiling interface " ^ file
  | CompilingImplementation { package; file } ->
      "[" ^ package ^ "] Compiling " ^ file
  | LinkingLibrary { package; output } ->
      "[" ^ package ^ "] Linking library " ^ output
  | LinkingExecutable { package; output } ->
      "[" ^ package ^ "] Linking executable " ^ output
  | ComputingHash { package } ->
      "Computing hash for " ^ package
  | HashComputed { package; hash } ->
      "Hash for " ^ package ^ ": " ^ hash
  | CopyingFile { source; dest } ->
      "Copying " ^ source ^ " -> " ^ dest
  | WritingFile { path } ->
      "Writing " ^ path
  | CreatingDirectory { path } ->
      "Creating directory " ^ path
  | RpcRequestReceived { request_type; _ } ->
      "RPC request: " ^ request_type
  | RpcResponseSent { result } ->
      "RPC response sent (success: " ^ Bool.to_string
        (
          match result with
          | Ok _ -> true
          | Error _ -> false
        ) ^ ")"
  | McpToolCall { tool; _ } ->
      "MCP tool call: " ^ tool
  | StoreCreating ->
      "Creating build cache store"
  | StoreCreated { duration_ms } ->
      "Store created in " ^ Int.to_string duration_ms ^ "ms"
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
  let timestamp = Datetime.to_iso8601 event.timestamp in
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

let json_of_string_option = function
  | Some value -> Json.String value
  | None -> Json.Null

let string_option_of_json = function
  | Json.String value -> Some value
  | Json.Null -> None
  | _ -> None

let json_of_resolution_mode = function
  | `Refresh -> Json.String "refresh"
  | `Unlock -> Json.String "unlock"

let resolution_mode_of_json = function
  | Json.String "refresh" -> Some `Refresh
  | Json.String "unlock" -> Some `Unlock
  | _ -> None
(** Convert kind to JSON *)
let kind_to_json = function
  | BuildComplete { duration_ms; results; succeeded; failed } ->
      Json.Object [
        ("duration_ms", Json.Int duration_ms);
        ("succeeded", Json.Array (List.map (fun s -> Json.String s) succeeded));
        ("failed", Json.Array (List.map (fun s -> Json.String s) failed));
      ]
  | BuildGraphCreated { nodes; duration_ms } ->
      Json.Object [ ("nodes", Json.Int nodes); ("duration_ms", Json.Int duration_ms) ]
  | BuildGraphCreating ->
      Json.Object []
  | BuildStarted { packages; total_modules; workers } ->
      Json.Object [
        ("packages", Json.Array (List.map (fun p -> Json.String p) packages));
        ("total_modules", Json.Int total_modules);
        ("workers", Json.Int workers);
      ]
  | CacheHit { package; hash } ->
      Json.Object [ ("package", Json.String package); ("hash", Json.String hash) ]
  | CacheMiss { package; hash } ->
      Json.Object [ ("package", Json.String package); ("hash", Json.String hash) ]
  | PackageStarted { package } ->
      Json.Object [ ("package", Json.String package) ]
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
        ("package", Json.String package);
        ("success", Json.Bool success);
        ("duration_ms", Json.Int duration_ms);
        ("modules_compiled", Json.Int modules_compiled);
        ("cache_hits", Json.Int cache_hits);
        ("cache_misses", Json.Int cache_misses);
      ]
  | PackageSkipped { package; reason } ->
      let reason_json =
        match reason with
        | DependenciesFailed deps -> Json.Object [
          ("type", Json.String "dependencies_failed");
          ("dependencies", Json.Array (List.map (fun d -> Json.String d) deps));
        ]
      in
      Json.Object [ ("package", Json.String package); ("reason", reason_json) ]
  | CompileError { package; error } ->
      let col_start, col_end = error.span in
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
        ("package", Json.String package);
        ("file", Json.String error.file);
        ("line", Json.Int error.line);
        ("span", Json.Array [ Json.Int col_start; Json.Int col_end ]);
        ("message", Json.String (strip_ansi_codes error_message));
        ("hint", Json.String (strip_ansi_codes error.hint));
        ("raw", Json.String (strip_ansi_codes error.raw));
      ]
  | CacheStored { package; hash; artifacts } ->
      Json.Object [
        ("package", Json.String package);
        ("hash", Json.String hash);
        ("artifacts", Json.Array (List.map (fun a -> Json.String a) artifacts));
      ]
  | CompilingImplementation { package; file } ->
      Json.Object [ ("package", Json.String package); ("file", Json.String file) ]
  | CompilingInterface { package; file } ->
      Json.Object [ ("package", Json.String package); ("file", Json.String file) ]
  | ComputingHash { package } ->
      Json.Object [ ("package", Json.String package) ]
  | CopyingFile { source; dest } ->
      Json.Object [ ("source", Json.String source); ("dest", Json.String dest) ]
  | CreatingDirectory { path } ->
      Json.Object [ ("path", Json.String path) ]
  | CycleDetected { packages } ->
      Json.Object [ ("packages", Json.Array (List.map (fun p -> Json.String p) packages)); ]
  | DependencyMissing { package; missing } ->
      Json.Object [
        ("package", Json.String package);
        ("missing", Json.Array (List.map (fun m -> Json.String m) missing));
      ]
  | DependencySatisfied { package } ->
      Json.Object [ ("package", Json.String package) ]
  | HashComputed { package; hash } ->
      Json.Object [ ("package", Json.String package); ("hash", Json.String hash) ]
  | StoreCreating ->
      Json.Object []
  | StoreCreated { duration_ms } ->
      Json.Object [ ("duration_ms", Json.Int duration_ms) ]
  | WorkerPoolCreating { workers } ->
      Json.Object [ ("workers", Json.Int workers) ]
  | WorkerPoolCreated { workers; duration_ms } ->
      Json.Object [ ("workers", Json.Int workers); ("duration_ms", Json.Int duration_ms) ]
  | LinkingExecutable { package; output } ->
      Json.Object [ ("package", Json.String package); ("output", Json.String output) ]
  | LinkingLibrary { package; output } ->
      Json.Object [ ("package", Json.String package); ("output", Json.String output) ]
  | McpToolCall { tool; args } ->
      Json.Object [ ("tool", Json.String tool); ("args", args) ]
  | QueuePackage { package; queue_type } ->
      Json.Object [ ("package", Json.String package); (
          "queue_type",
          Json.String (
            match queue_type with
            | `Ready -> "ready"
            | `Waiting -> "waiting"
          )
        ); ]
  | QueueStats { ready; waiting; busy } ->
      Json.Object [
        ("ready", Json.Int ready);
        ("waiting", Json.Int waiting);
        ("busy", Json.Int busy);
      ]
  | RpcRequestReceived { request_type; args } ->
      Json.Object [ ("request_type", Json.String request_type); ("args", args) ]
  | RpcResponseSent { result } ->
      Json.Object [ (
          "success",
          Json.Bool (
            match result with
            | Ok _ -> true
            | Error _ -> false
          )
        ); (
          "error",
          match result with
          | Ok _ -> Json.Null
          | Error e -> Json.String e
        ); ]
  | ServerRestarted { packages; toolchain } ->
      Json.Object [ ("packages", Json.Int packages); ("toolchain", Json.String toolchain); ]
  | ServerScanning { root } ->
      Json.Object [ ("root", Json.String root) ]
  | ServerShutdown ->
      Json.Object []
  | ServerStarted { pid } ->
      Json.Object [ ("pid", Json.String pid) ]
  | WorkerAssigned { worker_id; package } ->
      Json.Object [
        ("worker_id", Json.String (Worker_id.to_string worker_id));
        ("package", Json.String package);
      ]
  | WorkerIdle { worker_id } ->
      Json.Object [ ("worker_id", Json.String (Worker_id.to_string worker_id)) ]
  | WorkerPoolStarted { workers } ->
      Json.Object [ ("workers", Json.Int workers) ]
  | WorkerStarted { worker_id } ->
      Json.Object [ ("worker_id", Json.String (Worker_id.to_string worker_id)) ]
  | WorkspaceEmpty ->
      Json.Object []
  | WorkspaceScanning ->
      Json.Object []
  | WorkspaceScanned { packages; duration_ms } ->
      Json.Object [ ("packages", Json.Int packages); ("duration_ms", Json.Int duration_ms); ]
  | LockfileReadStarted { path } ->
      Json.Object [ ("path", Json.String path) ]
  | LockfileReadFinished { path; duration_ms } ->
      Json.Object [ ("path", Json.String path); ("duration_ms", Json.Int duration_ms) ]
  | LockfileReadFailed { path; error } ->
      Json.Object [ ("path", Json.String path); ("error", Json.String error) ]
  | LockfileWriteStarted { path } ->
      Json.Object [ ("path", Json.String path) ]
  | LockfileWriteFinished { path; duration_ms } ->
      Json.Object [ ("path", Json.String path); ("duration_ms", Json.Int duration_ms) ]
  | LockfileWriteFailed { path; error } ->
      Json.Object [ ("path", Json.String path); ("error", Json.String error) ]
  | DependencyResolutionStarted { packages; mode } ->
      Json.Object [
        ("packages", Json.Array (List.map (fun package -> Json.String package) packages));
        ("mode", json_of_resolution_mode mode);
      ]
  | DependencyResolutionUsingExistingLock { path } ->
      Json.Object [ ("path", Json.String path) ]
  | DependencyResolutionRefreshingLock { path } ->
      Json.Object [ ("path", Json.String path) ]
  | DependencyResolutionUnlocking { path } ->
      Json.Object [ ("path", json_of_string_option path) ]
  | DependencyResolutionFinished { duration_ms; resolved_packages; resolved_edges } ->
      Json.Object [
        ("duration_ms", Json.Int duration_ms);
        ("resolved_packages", Json.Int resolved_packages);
        ("resolved_edges", Json.Int resolved_edges);
      ]
  | DependencyResolutionFailed { error } ->
      Json.Object [ ("error", Json.String error) ]
  | DependencyUniverseBuilding { packages } ->
      Json.Object [ ("packages", Json.Array (List.map (fun package -> Json.String package) packages)) ]
  | DependencyUniverseBuilt { runtime_packages; build_packages; dev_packages; duration_ms } ->
      Json.Object [
        ("runtime_packages", Json.Int runtime_packages);
        ("build_packages", Json.Int build_packages);
        ("dev_packages", Json.Int dev_packages);
        ("duration_ms", Json.Int duration_ms);
      ]
  | PackageMetadataFetchStarted { package } ->
      Json.Object [ ("package", Json.String package) ]
  | PackageMetadataFetchFinished { package; version; duration_ms } ->
      Json.Object [
        ("package", Json.String package);
        ("version", json_of_string_option version);
        ("duration_ms", Json.Int duration_ms);
      ]
  | PackageMetadataFetchFailed { package; error } ->
      Json.Object [ ("package", Json.String package); ("error", Json.String error) ]
  | PackageManifestFetchStarted { package; version } ->
      Json.Object [ ("package", Json.String package); ("version", Json.String version) ]
  | PackageManifestFetchFinished { package; version; duration_ms } ->
      Json.Object [
        ("package", Json.String package);
        ("version", Json.String version);
        ("duration_ms", Json.Int duration_ms);
      ]
  | PackageManifestFetchFailed { package; version; error } ->
      Json.Object [
        ("package", Json.String package);
        ("version", json_of_string_option version);
        ("error", Json.String error);
      ]
  | PackageDownloadStarted { package; version; path } ->
      Json.Object [
        ("package", Json.String package);
        ("version", Json.String version);
        ("path", Json.String path);
      ]
  | PackageDownloadFinished { package; version; path; duration_ms } ->
      Json.Object [
        ("package", Json.String package);
        ("version", Json.String version);
        ("path", Json.String path);
        ("duration_ms", Json.Int duration_ms);
      ]
  | PackageDownloadFailed { package; version; path; error } ->
      Json.Object [
        ("package", Json.String package);
        ("version", Json.String version);
        ("path", Json.String path);
        ("error", Json.String error);
      ]
  | PackageDownloadSkipped { package; version; path; reason } ->
      Json.Object [
        ("package", Json.String package);
        ("version", Json.String version);
        ("path", Json.String path);
        ("reason", Json.String reason);
      ]
  | PackageCacheHit { package; version; path } ->
      Json.Object [
        ("package", Json.String package);
        ("version", Json.String version);
        ("path", Json.String path);
      ]
  | PackageMaterializationStarted { package; version; path } ->
      Json.Object [
        ("package", Json.String package);
        ("version", Json.String version);
        ("path", Json.String path);
      ]
  | PackageMaterializationFinished { package; version; path; duration_ms } ->
      Json.Object [
        ("package", Json.String package);
        ("version", Json.String version);
        ("path", Json.String path);
        ("duration_ms", Json.Int duration_ms);
      ]
  | PackageMaterializationFailed { package; version; path; error } ->
      Json.Object [
        ("package", Json.String package);
        ("version", Json.String version);
        ("path", Json.String path);
        ("error", Json.String error);
      ]
  | PackageResolvedForBuild { package; version; path; workspace } ->
      Json.Object [
        ("package", Json.String package);
        ("version", json_of_string_option version);
        ("path", Json.String path);
        ("workspace", Json.Bool workspace);
      ]
  | PackageDownloadQueued { package; version; path } ->
      Json.Object [
        ("package", Json.String package);
        ("version", Json.String version);
        ("path", Json.String path);
      ]
  | WritingFile { path } ->
      Json.Object [ ("path", Json.String path) ]
(** Convert event to JSON *)
let to_json = fun event ->
  let timestamp = Datetime.to_iso8601 event.timestamp in
  (* Strip ANSI codes from the event before converting to JSON *)
  let clean_event =
    match event.kind with
    | CompileError { package; error } ->
        let clean_error = {
          error
          with raw = strip_ansi_codes error.raw;
          hint = strip_ansi_codes error.hint
        } in
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
      match List.assoc_opt "event" fields with
      | Some (Json.String event_name) -> (
          let data = List.assoc_opt "data" fields |> Option.unwrap_or ~default:(Json.Object []) in
          match event_name with
          | "tusk.build.completed" -> (
              match data with
              | Json.Object data_fields ->
                  let duration_ms =
                    match List.assoc_opt "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let succeeded =
                    match List.assoc_opt "succeeded" data_fields with
                    | Some (Json.Array arr) ->
                        List.filter_map
                          (
                            function
                            | Json.String s -> Some s
                            | _ -> None
                          )
                          arr
                    | _ -> []
                  in
                  let failed =
                    match List.assoc_opt "failed" data_fields with
                    | Some (Json.Array arr) ->
                        List.filter_map
                          (
                            function
                            | Json.String s -> Some s
                            | _ -> None
                          )
                          arr
                    | _ -> []
                  in
                  Ok (BuildComplete { duration_ms; results = []; succeeded; failed })
              | _ -> Error "Invalid BuildComplete data"
            )
          | "tusk.build.started" -> (
              match data with
              | Json.Object data_fields ->
                  let packages =
                    match List.assoc_opt "packages" data_fields with
                    | Some (Json.Array arr) ->
                        List.filter_map
                          (
                            function
                            | Json.String s -> Some s
                            | _ -> None
                          )
                          arr
                    | _ -> []
                  in
                  let total_modules =
                    match List.assoc_opt "total_modules" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let workers =
                    match List.assoc_opt "workers" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (BuildStarted { packages; total_modules; workers })
              | _ -> Error "Invalid BuildStarted data"
            )
          | "tusk.build.package.started" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  Ok (PackageStarted { package })
              | _ -> Error "Invalid PackageStarted data"
            )
          | "tusk.build.package.completed" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let success =
                    match List.assoc_opt "success" data_fields with
                    | Some (Json.Bool b) -> b
                    | _ -> false
                  in
                  let duration_ms =
                    match List.assoc_opt "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let modules_compiled =
                    match List.assoc_opt "modules_compiled" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let cache_hits =
                    match List.assoc_opt "cache_hits" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let cache_misses =
                    match List.assoc_opt "cache_misses" data_fields with
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
          | "tusk.build.package.skipped" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let reason =
                    match List.assoc_opt "reason" data_fields with
                    | Some (Json.Object reason_fields) -> (
                        match List.assoc_opt "type" reason_fields with
                        | Some (Json.String "dependencies_failed") -> (
                            match List.assoc_opt "dependencies" reason_fields with
                            | Some (Json.Array deps) ->
                                let dep_names =
                                  List.filter_map
                                    (
                                      function
                                      | Json.String s -> Some s
                                      | _ -> None
                                    )
                                    deps
                                in
                                DependenciesFailed dep_names
                            | _ -> DependenciesFailed []
                          )
                        | _ -> DependenciesFailed []
                      )
                    | _ -> DependenciesFailed []
                  in
                  Ok (PackageSkipped { package; reason })
              | _ -> Error "Invalid PackageSkipped data"
            )
          | "tusk.build.cache.hit" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let hash =
                    match List.assoc_opt "hash" data_fields with
                    | Some (Json.String h) -> h
                    | _ -> ""
                  in
                  Ok (CacheHit { package; hash })
              | _ -> Error "Invalid CacheHit data"
            )
          | "tusk.build.cache.miss" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let hash =
                    match List.assoc_opt "hash" data_fields with
                    | Some (Json.String h) -> h
                    | _ -> ""
                  in
                  Ok (CacheMiss { package; hash })
              | _ -> Error "Invalid CacheMiss data"
            )
          | "tusk.build.cache.stored" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let hash =
                    match List.assoc_opt "hash" data_fields with
                    | Some (Json.String h) -> h
                    | _ -> ""
                  in
                  let artifacts =
                    match List.assoc_opt "artifacts" data_fields with
                    | Some (Json.Array arr) ->
                        List.filter_map
                          (
                            function
                            | Json.String s -> Some s
                            | _ -> None
                          )
                          arr
                    | _ -> []
                  in
                  Ok (CacheStored { package; hash; artifacts })
              | _ -> Error "Invalid CacheStored data"
            )
          | "tusk.build.compile.interface" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let file =
                    match List.assoc_opt "file" data_fields with
                    | Some (Json.String f) -> f
                    | _ -> ""
                  in
                  Ok (CompilingInterface { package; file })
              | _ -> Error "Invalid CompilingInterface data"
            )
          | "tusk.build.compile.implementation" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let file =
                    match List.assoc_opt "file" data_fields with
                    | Some (Json.String f) -> f
                    | _ -> ""
                  in
                  Ok (CompilingImplementation { package; file })
              | _ -> Error "Invalid CompilingImplementation data"
            )
          | "tusk.build.compile.error" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let file =
                    match List.assoc_opt "file" data_fields with
                    | Some (Json.String f) -> f
                    | _ -> ""
                  in
                  let line =
                    match List.assoc_opt "line" data_fields with
                    | Some (Json.Int l) -> l
                    | _ -> 0
                  in
                  let span =
                    match List.assoc_opt "span" data_fields with
                    | Some (Json.Array [Json.Int start;Json.Int end_]) -> (start, end_)
                    | _ -> (0, 0)
                  in
                  let message =
                    match List.assoc_opt "message" data_fields with
                    | Some (Json.String m) -> m
                    | _ -> ""
                  in
                  let hint =
                    match List.assoc_opt "hint" data_fields with
                    | Some (Json.String h) -> Some h
                    | _ -> None
                  in
                  let raw =
                    match List.assoc_opt "raw" data_fields with
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
                      UnboundValue { name = String.sub message 14 (String.length message - 14) }
                    else if String.starts_with ~prefix:"Unbound module " message then
                      UnboundModule { name = String.sub message 15 (String.length message - 15) }
                    else if String.starts_with ~prefix:"Cannot find file " message then
                      FileNotFound { filename = String.sub message 17 (String.length message - 17) }
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
          | "tusk.build.link.library" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let output =
                    match List.assoc_opt "output" data_fields with
                    | Some (Json.String o) -> o
                    | _ -> ""
                  in
                  Ok (LinkingLibrary { package; output })
              | _ -> Error "Invalid LinkingLibrary data"
            )
          | "tusk.build.hash.computing" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  Ok (ComputingHash { package })
              | _ -> Error "Invalid ComputingHash data"
            )
          | "tusk.build.hash.computed" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let hash =
                    match List.assoc_opt "hash" data_fields with
                    | Some (Json.String h) -> h
                    | _ -> ""
                  in
                  Ok (HashComputed { package; hash })
              | _ -> Error "Invalid HashComputed data"
            )
          | "tusk.build.link.executable" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  let output =
                    match List.assoc_opt "output" data_fields with
                    | Some (Json.String o) -> o
                    | _ -> ""
                  in
                  Ok (LinkingExecutable { package; output })
              | _ -> Error "Invalid LinkingExecutable data"
            )
          | "tusk.workspace.scanning" ->
              Ok WorkspaceScanning
          | "tusk.workspace.scanned" -> (
              match data with
              | Json.Object data_fields ->
                  let packages =
                    match List.assoc_opt "packages" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let duration_ms =
                    match List.assoc_opt "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (WorkspaceScanned { packages; duration_ms })
              | _ -> Error "Invalid WorkspaceScanned data"
            )
          | "tusk.build_graph.creating" ->
              Ok BuildGraphCreating
          | "tusk.build_graph.created" -> (
              match data with
              | Json.Object data_fields ->
                  let nodes =
                    match List.assoc_opt "nodes" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let duration_ms =
                    match List.assoc_opt "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (BuildGraphCreated { nodes; duration_ms })
              | _ -> Error "Invalid BuildGraphCreated data"
            )
          | "tusk.store.creating" ->
              Ok StoreCreating
          | "tusk.store.created" -> (
              match data with
              | Json.Object data_fields ->
                  let duration_ms =
                    match List.assoc_opt "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (StoreCreated { duration_ms })
              | _ -> Error "Invalid StoreCreated data"
            )
          | "tusk.worker_pool.creating" -> (
              match data with
              | Json.Object data_fields ->
                  let workers =
                    match List.assoc_opt "workers" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (WorkerPoolCreating { workers })
              | _ -> Error "Invalid WorkerPoolCreating data"
            )
          | "tusk.worker_pool.created" -> (
              match data with
              | Json.Object data_fields ->
                  let workers =
                    match List.assoc_opt "workers" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  let duration_ms =
                    match List.assoc_opt "duration_ms" data_fields with
                    | Some (Json.Int n) -> n
                    | _ -> 0
                  in
                  Ok (WorkerPoolCreated { workers; duration_ms })
              | _ -> Error "Invalid WorkerPoolCreated data"
            )
          | "tusk.pm.lockfile.read.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "path" data_fields with
                  | Some (Json.String path) -> Ok (LockfileReadStarted { path })
                  | _ -> Error "Invalid LockfileReadStarted data"
                )
              | _ -> Error "Invalid LockfileReadStarted data"
            )
          | "tusk.pm.lockfile.read.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "path" data_fields, List.assoc_opt "duration_ms" data_fields with
                  | Some (Json.String path), Some (Json.Int duration_ms) ->
                      Ok (LockfileReadFinished { path; duration_ms })
                  | _ -> Error "Invalid LockfileReadFinished data"
                )
              | _ -> Error "Invalid LockfileReadFinished data"
            )
          | "tusk.pm.lockfile.read.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "path" data_fields, List.assoc_opt "error" data_fields with
                  | Some (Json.String path), Some (Json.String error) ->
                      Ok (LockfileReadFailed { path; error })
                  | _ -> Error "Invalid LockfileReadFailed data"
                )
              | _ -> Error "Invalid LockfileReadFailed data"
            )
          | "tusk.pm.lockfile.write.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "path" data_fields with
                  | Some (Json.String path) -> Ok (LockfileWriteStarted { path })
                  | _ -> Error "Invalid LockfileWriteStarted data"
                )
              | _ -> Error "Invalid LockfileWriteStarted data"
            )
          | "tusk.pm.lockfile.write.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "path" data_fields, List.assoc_opt "duration_ms" data_fields with
                  | Some (Json.String path), Some (Json.Int duration_ms) ->
                      Ok (LockfileWriteFinished { path; duration_ms })
                  | _ -> Error "Invalid LockfileWriteFinished data"
                )
              | _ -> Error "Invalid LockfileWriteFinished data"
            )
          | "tusk.pm.lockfile.write.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "path" data_fields, List.assoc_opt "error" data_fields with
                  | Some (Json.String path), Some (Json.String error) ->
                      Ok (LockfileWriteFailed { path; error })
                  | _ -> Error "Invalid LockfileWriteFailed data"
                )
              | _ -> Error "Invalid LockfileWriteFailed data"
            )
          | "tusk.pm.resolution.started" -> (
              match data with
              | Json.Object data_fields ->
                  let packages =
                    match List.assoc_opt "packages" data_fields with
                    | Some (Json.Array items) ->
                        List.filter_map
                          (function
                            | Json.String package -> Some package
                            | _ -> None)
                          items
                    | _ -> []
                  in
                  let mode =
                    match List.assoc_opt "mode" data_fields with
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
          | "tusk.pm.resolution.using_existing_lock" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "path" data_fields with
                  | Some (Json.String path) -> Ok (DependencyResolutionUsingExistingLock { path })
                  | _ -> Error "Invalid DependencyResolutionUsingExistingLock data"
                )
              | _ -> Error "Invalid DependencyResolutionUsingExistingLock data"
            )
          | "tusk.pm.resolution.refreshing_lock" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "path" data_fields with
                  | Some (Json.String path) -> Ok (DependencyResolutionRefreshingLock { path })
                  | _ -> Error "Invalid DependencyResolutionRefreshingLock data"
                )
              | _ -> Error "Invalid DependencyResolutionRefreshingLock data"
            )
          | "tusk.pm.resolution.unlocking" -> (
              match data with
              | Json.Object data_fields ->
                  let path =
                    match List.assoc_opt "path" data_fields with
                    | Some json -> string_option_of_json json
                    | None -> None
                  in
                  Ok (DependencyResolutionUnlocking { path })
              | _ -> Error "Invalid DependencyResolutionUnlocking data"
            )
          | "tusk.pm.resolution.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "duration_ms" data_fields,
                    List.assoc_opt "resolved_packages" data_fields,
                    List.assoc_opt "resolved_edges" data_fields
                  with
                  | Some (Json.Int duration_ms), Some (Json.Int resolved_packages), Some (Json.Int resolved_edges) ->
                      Ok (DependencyResolutionFinished { duration_ms; resolved_packages; resolved_edges })
                  | _ -> Error "Invalid DependencyResolutionFinished data"
                )
              | _ -> Error "Invalid DependencyResolutionFinished data"
            )
          | "tusk.pm.resolution.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "error" data_fields with
                  | Some (Json.String error) -> Ok (DependencyResolutionFailed { error })
                  | _ -> Error "Invalid DependencyResolutionFailed data"
                )
              | _ -> Error "Invalid DependencyResolutionFailed data"
            )
          | "tusk.pm.universe.building" -> (
              match data with
              | Json.Object data_fields ->
                  let packages =
                    match List.assoc_opt "packages" data_fields with
                    | Some (Json.Array items) ->
                        List.filter_map
                          (function
                            | Json.String package -> Some package
                            | _ -> None)
                          items
                    | _ -> []
                  in
                  Ok (DependencyUniverseBuilding { packages })
              | _ -> Error "Invalid DependencyUniverseBuilding data"
            )
          | "tusk.pm.universe.built" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "runtime_packages" data_fields,
                    List.assoc_opt "build_packages" data_fields,
                    List.assoc_opt "dev_packages" data_fields,
                    List.assoc_opt "duration_ms" data_fields
                  with
                  | Some (Json.Int runtime_packages),
                    Some (Json.Int build_packages),
                    Some (Json.Int dev_packages),
                    Some (Json.Int duration_ms) ->
                      Ok (DependencyUniverseBuilt { runtime_packages; build_packages; dev_packages; duration_ms })
                  | _ -> Error "Invalid DependencyUniverseBuilt data"
                )
              | _ -> Error "Invalid DependencyUniverseBuilt data"
            )
          | "tusk.pm.package_metadata.fetch.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "package" data_fields with
                  | Some (Json.String package) -> Ok (PackageMetadataFetchStarted { package })
                  | _ -> Error "Invalid PackageMetadataFetchStarted data"
                )
              | _ -> Error "Invalid PackageMetadataFetchStarted data"
            )
          | "tusk.pm.package_metadata.fetch.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "package" data_fields, List.assoc_opt "duration_ms" data_fields with
                  | Some (Json.String package), Some (Json.Int duration_ms) ->
                      let version =
                        match List.assoc_opt "version" data_fields with
                        | Some json -> string_option_of_json json
                        | None -> None
                      in
                      Ok (PackageMetadataFetchFinished { package; version; duration_ms })
                  | _ -> Error "Invalid PackageMetadataFetchFinished data"
                )
              | _ -> Error "Invalid PackageMetadataFetchFinished data"
            )
          | "tusk.pm.package_metadata.fetch.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "package" data_fields, List.assoc_opt "error" data_fields with
                  | Some (Json.String package), Some (Json.String error) ->
                      Ok (PackageMetadataFetchFailed { package; error })
                  | _ -> Error "Invalid PackageMetadataFetchFailed data"
                )
              | _ -> Error "Invalid PackageMetadataFetchFailed data"
            )
          | "tusk.pm.package_manifest.fetch.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "package" data_fields, List.assoc_opt "version" data_fields with
                  | Some (Json.String package), Some (Json.String version) ->
                      Ok (PackageManifestFetchStarted { package; version })
                  | _ -> Error "Invalid PackageManifestFetchStarted data"
                )
              | _ -> Error "Invalid PackageManifestFetchStarted data"
            )
          | "tusk.pm.package_manifest.fetch.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "version" data_fields,
                    List.assoc_opt "duration_ms" data_fields
                  with
                  | Some (Json.String package), Some (Json.String version), Some (Json.Int duration_ms) ->
                      Ok (PackageManifestFetchFinished { package; version; duration_ms })
                  | _ -> Error "Invalid PackageManifestFetchFinished data"
                )
              | _ -> Error "Invalid PackageManifestFetchFinished data"
            )
          | "tusk.pm.package_manifest.fetch.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match List.assoc_opt "package" data_fields, List.assoc_opt "error" data_fields with
                  | Some (Json.String package), Some (Json.String error) ->
                      let version =
                        match List.assoc_opt "version" data_fields with
                        | Some json -> string_option_of_json json
                        | None -> None
                      in
                      Ok (PackageManifestFetchFailed { package; version; error })
                  | _ -> Error "Invalid PackageManifestFetchFailed data"
                )
              | _ -> Error "Invalid PackageManifestFetchFailed data"
            )
          | "tusk.pm.package_download.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "version" data_fields,
                    List.assoc_opt "path" data_fields
                  with
                  | Some (Json.String package), Some (Json.String version), Some (Json.String path) ->
                      Ok (PackageDownloadStarted { package; version; path })
                  | _ -> Error "Invalid PackageDownloadStarted data"
                )
              | _ -> Error "Invalid PackageDownloadStarted data"
            )
          | "tusk.pm.package_download.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "version" data_fields,
                    List.assoc_opt "path" data_fields,
                    List.assoc_opt "duration_ms" data_fields
                  with
                  | Some (Json.String package),
                    Some (Json.String version),
                    Some (Json.String path),
                    Some (Json.Int duration_ms) ->
                      Ok (PackageDownloadFinished { package; version; path; duration_ms })
                  | _ -> Error "Invalid PackageDownloadFinished data"
                )
              | _ -> Error "Invalid PackageDownloadFinished data"
            )
          | "tusk.pm.package_download.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "version" data_fields,
                    List.assoc_opt "path" data_fields,
                    List.assoc_opt "error" data_fields
                  with
                  | Some (Json.String package),
                    Some (Json.String version),
                    Some (Json.String path),
                    Some (Json.String error) ->
                      Ok (PackageDownloadFailed { package; version; path; error })
                  | _ -> Error "Invalid PackageDownloadFailed data"
                )
              | _ -> Error "Invalid PackageDownloadFailed data"
            )
          | "tusk.pm.package_download.skipped" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "version" data_fields,
                    List.assoc_opt "path" data_fields,
                    List.assoc_opt "reason" data_fields
                  with
                  | Some (Json.String package),
                    Some (Json.String version),
                    Some (Json.String path),
                    Some (Json.String reason) ->
                      Ok (PackageDownloadSkipped { package; version; path; reason })
                  | _ -> Error "Invalid PackageDownloadSkipped data"
                )
              | _ -> Error "Invalid PackageDownloadSkipped data"
            )
          | "tusk.pm.package_cache.hit" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "version" data_fields,
                    List.assoc_opt "path" data_fields
                  with
                  | Some (Json.String package), Some (Json.String version), Some (Json.String path) ->
                      Ok (PackageCacheHit { package; version; path })
                  | _ -> Error "Invalid PackageCacheHit data"
                )
              | _ -> Error "Invalid PackageCacheHit data"
            )
          | "tusk.pm.package_materialization.started" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "version" data_fields,
                    List.assoc_opt "path" data_fields
                  with
                  | Some (Json.String package), Some (Json.String version), Some (Json.String path) ->
                      Ok (PackageMaterializationStarted { package; version; path })
                  | _ -> Error "Invalid PackageMaterializationStarted data"
                )
              | _ -> Error "Invalid PackageMaterializationStarted data"
            )
          | "tusk.pm.package_materialization.finished" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "version" data_fields,
                    List.assoc_opt "path" data_fields,
                    List.assoc_opt "duration_ms" data_fields
                  with
                  | Some (Json.String package),
                    Some (Json.String version),
                    Some (Json.String path),
                    Some (Json.Int duration_ms) ->
                      Ok (PackageMaterializationFinished { package; version; path; duration_ms })
                  | _ -> Error "Invalid PackageMaterializationFinished data"
                )
              | _ -> Error "Invalid PackageMaterializationFinished data"
            )
          | "tusk.pm.package_materialization.failed" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "version" data_fields,
                    List.assoc_opt "path" data_fields,
                    List.assoc_opt "error" data_fields
                  with
                  | Some (Json.String package),
                    Some (Json.String version),
                    Some (Json.String path),
                    Some (Json.String error) ->
                      Ok (PackageMaterializationFailed { package; version; path; error })
                  | _ -> Error "Invalid PackageMaterializationFailed data"
                )
              | _ -> Error "Invalid PackageMaterializationFailed data"
            )
          | "tusk.pm.package_resolved_for_build" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "path" data_fields,
                    List.assoc_opt "workspace" data_fields
                  with
                  | Some (Json.String package), Some (Json.String path), Some (Json.Bool workspace) ->
                      let version =
                        match List.assoc_opt "version" data_fields with
                        | Some json -> string_option_of_json json
                        | None -> None
                      in
                      Ok (PackageResolvedForBuild { package; version; path; workspace })
                  | _ -> Error "Invalid PackageResolvedForBuild data"
                )
              | _ -> Error "Invalid PackageResolvedForBuild data"
            )
          | "tusk.pm.package_download.queued" -> (
              match data with
              | Json.Object data_fields -> (
                  match
                    List.assoc_opt "package" data_fields,
                    List.assoc_opt "version" data_fields,
                    List.assoc_opt "path" data_fields
                  with
                  | Some (Json.String package), Some (Json.String version), Some (Json.String path) ->
                      Ok (PackageDownloadQueued { package; version; path })
                  | _ -> Error "Invalid PackageDownloadQueued data"
                )
              | _ -> Error "Invalid PackageDownloadQueued data"
            )
          | _ ->
              Error ("Unknown event type: " ^ event_name)
        )
      | _ -> Error "Missing event field"
    )
  | _ -> Error "Invalid JSON format"
(** Convert from JSON *)
let from_json = fun json ->
  match json with
  | Json.Object fields -> (
      let timestamp =
        match List.assoc_opt "timestamp" fields with
        | Some (Json.String _ts) ->
            (* For now, use current time - proper timestamp parsing can be added later *)
            Datetime.now ()
        | _ -> Datetime.now ()
      in
      let session_id =
        match List.assoc_opt "session_id" fields with
        | Some (Json.String s) -> Session_id.of_string s
        | _ -> Session_id.make ()
      in
      let level =
        match List.assoc_opt "level" fields with
        | Some (Json.String "error") -> Error
        | Some (Json.String "warn") -> Warn
        | Some (Json.String "info") -> Info
        | Some (Json.String "debug") -> Debug
        | Some (Json.String "trace") -> Trace
        | _ -> Info
      in
      match kind_from_json json with
      | Ok kind -> Ok { timestamp; session_id; level; kind }
      | Error e -> Error e
    )
  | _ -> Error "Invalid JSON format for Event"

module Tests = struct
  let test_lockfile_event_json_roundtrip () : (unit, string) result =
    let event =
      create
        ~session_id:(Session_id.of_string "test-session")
        ~level:Info
        (LockfileReadFinished { path = "/tmp/workspace/tusk.lock"; duration_ms = 12 })
    in
    match from_json (to_json event) with
    | Ok { kind = LockfileReadFinished { path; duration_ms }; _ } ->
        if String.equal path "/tmp/workspace/tusk.lock" && duration_ms = 12 then
          Ok ()
        else
          Error "expected lockfile read event to round-trip"
    | Ok _ -> Error "expected LockfileReadFinished after round-trip"
    | Error err -> Error err
    [@test]

  let test_resolution_event_json_roundtrip () : (unit, string) result =
    let event =
      create
        ~session_id:(Session_id.of_string "test-session")
        ~level:Info
        (DependencyResolutionStarted {
          packages = [ "app"; "std" ];
          mode = `Unlock;
        })
    in
    match from_json (to_json event) with
    | Ok { kind = DependencyResolutionStarted { packages; mode = `Unlock }; _ } ->
        if packages = [ "app"; "std" ] then
          Ok ()
        else
          Error "expected dependency resolution packages to round-trip"
    | Ok _ -> Error "expected DependencyResolutionStarted unlock event after round-trip"
    | Error err -> Error err
    [@test]

  let test_package_resolved_event_json_roundtrip () : (unit, string) result =
    let event =
      create
        ~session_id:(Session_id.of_string "test-session")
        ~level:Info
        (PackageResolvedForBuild {
          package = "std";
          version = Some "0.1.0";
          path = "/Users/example/.tusk/registry/pkgs.ml/src/std/0.1.0";
          workspace = false;
        })
    in
    match from_json (to_json event) with
    | Ok { kind = PackageResolvedForBuild { package; version; path; workspace }; _ } ->
        if
          String.equal package "std"
          && version = Some "0.1.0"
          && String.equal path "/Users/example/.tusk/registry/pkgs.ml/src/std/0.1.0"
          && not workspace
        then
          Ok ()
        else
          Error "expected package resolved event to round-trip"
    | Ok _ -> Error "expected PackageResolvedForBuild after round-trip"
    | Error err -> Error err
    [@test]
end [@test]
