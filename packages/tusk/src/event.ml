open Std
(** Event system for tusk - pure data types for events *)

type level = Error | Warn | Info | Debug | Trace

let level_to_string = function
  | Error -> "error"
  | Warn -> "warn"
  | Info -> "info"
  | Debug -> "debug"
  | Trace -> "trace"

type build_error = {
  package : string;
  file : string;
  line : int;
  column : int option;
  message : string;
  hint : string option;
}

type build_result = {
  package : string;
  success : bool;
  duration_ms : int;
  modules_compiled : int;
  cache_hits : int;
  cache_misses : int;
  errors : build_error list;
}

type kind =
  | BuildComplete of {
      duration_ms : int;
      results : build_result list;
      succeeded : string list;
      failed : string list;
    }
  | BuildGraphCreated of { nodes : int; duration_ms : int }
  | BuildGraphCreating
  | BuildStarted of {
      packages : string list;
      total_modules : int;
      workers : int;
    }
  | CacheHit of { package : string; hash : string }
  | CacheMiss of { package : string; hash : string }
  | CacheStored of { package : string; hash : string; artifacts : string list }
  | CompileError of build_error
  | CompilingImplementation of { package : string; file : string }
  | CompilingInterface of { package : string; file : string }
  | ComputingHash of { package : string }
  | CopyingFile of { source : string; dest : string }
  | CreatingDirectory of { path : string }
  | CycleDetected of { packages : string list }
  | DependencyMissing of { package : string; missing : string list }
  | DependencySatisfied of { package : string }
  | HashComputed of { package : string; hash : string }
  | LinkingExecutable of { package : string; output : string }
  | LinkingLibrary of { package : string; output : string }
  | McpToolCall of { tool : string; args : Json.t }
  | PackageComplete of build_result
  | PackageStarted of { package : string }
  | QueuePackage of { package : string; queue_type : [ `Ready | `Waiting ] }
  | QueueStats of { ready : int; waiting : int; busy : int }
  | RpcRequestReceived of { request_type : string; args : Json.t }
  | RpcResponseSent of { result : (unit, string) result }
  | ServerRestarted of { packages : int; toolchain : string }
  | ServerScanning of { root : string }
  | ServerShutdown
  | ServerStarted of { pid : string }
  | WorkerAssigned of { worker_id : Worker_id.t; package : string }
  | WorkerIdle of { worker_id : Worker_id.t }
  | WorkerPoolStarted of { workers : int }
  | WorkerStarted of { worker_id : Worker_id.t }
  | WorkspaceEmpty
  | WorkspaceScanning
  | WorkspaceScanned of { packages : int; duration_ms : int }
  | WritingFile of { path : string }

type t = {
  timestamp : Datetime.t;
  session_id : Session_id.t;
  level : level;
  kind : kind;
}

(** Create a new event with current timestamp *)
let create ~session_id ~level kind =
  { timestamp = Datetime.now (); session_id; level; kind }

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
  | WritingFile _ -> "tusk.build.file.write"

(** Get human-readable display message *)
let display = function
  | BuildStarted { packages; _ } ->
      Printf.sprintf "Build started for %d packages" (List.length packages)
  | BuildComplete { duration_ms; succeeded; failed; _ } ->
      Printf.sprintf "Build completed in %dms (%d succeeded, %d failed)"
        duration_ms (List.length succeeded) (List.length failed)
  | PackageStarted { package } -> Printf.sprintf "Building %s..." package
  | PackageComplete { package; success; duration_ms; _ } ->
      if success then Printf.sprintf "✓ Built %s in %dms" package duration_ms
      else Printf.sprintf "✗ Failed to build %s" package
  | CompileError { package; file; line; message; _ } ->
      Printf.sprintf "Error in %s [%s:%d]: %s" package file line message
  | CycleDetected { packages } ->
      Printf.sprintf "Circular dependency detected: %s"
        (String.concat " -> " packages)
  | CacheHit { package; _ } -> Printf.sprintf "Cached %s" package
  | CacheMiss { package; _ } -> Printf.sprintf "Cache miss for %s" package
  | CacheStored { package; artifacts; _ } ->
      Printf.sprintf "Cached %s (%d artifacts)" package (List.length artifacts)
  | WorkerPoolStarted { workers } ->
      Printf.sprintf "Started worker pool with %d workers" workers
  | WorkerStarted { worker_id } ->
      Printf.sprintf "Worker %s started" (Worker_id.to_string worker_id)
  | WorkerAssigned { worker_id; package } ->
      Printf.sprintf "Worker %s assigned to %s"
        (Worker_id.to_string worker_id)
        package
  | WorkerIdle { worker_id } ->
      Printf.sprintf "Worker %s idle" (Worker_id.to_string worker_id)
  | ServerStarted { pid } -> Printf.sprintf "Server started (pid: %s)" pid
  | ServerScanning { root } -> Printf.sprintf "Scanning workspace: %s" root
  | ServerRestarted { packages; toolchain } ->
      Printf.sprintf "Server restarted with %d packages (toolchain: %s)"
        packages toolchain
  | WorkspaceEmpty -> "No packages found in workspace"
  | WorkspaceScanning -> "Scanning workspace..."
  | WorkspaceScanned { packages; duration_ms } ->
      Printf.sprintf "Scanned workspace: %d packages in %dms" packages
        duration_ms
  | BuildGraphCreating -> "Creating build graph..."
  | BuildGraphCreated { nodes; duration_ms } ->
      Printf.sprintf "Created build graph: %d nodes in %dms" nodes duration_ms
  | ServerShutdown -> "Server shutting down"
  | QueuePackage { package; queue_type } ->
      let typ =
        match queue_type with `Ready -> "ready" | `Waiting -> "waiting"
      in
      Printf.sprintf "Queued %s (%s)" package typ
  | QueueStats { ready; waiting; busy } ->
      Printf.sprintf "Queue: %d ready, %d waiting, %d busy" ready waiting busy
  | DependencyMissing { package; missing } ->
      Printf.sprintf "%s waiting for: %s" package (String.concat ", " missing)
  | DependencySatisfied { package } ->
      Printf.sprintf "%s dependencies satisfied" package
  | CompilingInterface { package; file } ->
      Printf.sprintf "[%s] Compiling interface %s" package file
  | CompilingImplementation { package; file } ->
      Printf.sprintf "[%s] Compiling %s" package file
  | LinkingLibrary { package; output } ->
      Printf.sprintf "[%s] Linking library %s" package output
  | LinkingExecutable { package; output } ->
      Printf.sprintf "[%s] Linking executable %s" package output
  | ComputingHash { package } -> Printf.sprintf "Computing hash for %s" package
  | HashComputed { package; hash } ->
      Printf.sprintf "Hash for %s: %s" package hash
  | CopyingFile { source; dest } ->
      Printf.sprintf "Copying %s -> %s" source dest
  | WritingFile { path } -> Printf.sprintf "Writing %s" path
  | CreatingDirectory { path } -> Printf.sprintf "Creating directory %s" path
  | RpcRequestReceived { request_type; _ } ->
      Printf.sprintf "RPC request: %s" request_type
  | RpcResponseSent { result } ->
      Printf.sprintf "RPC response sent (success: %b)"
        (match result with Ok _ -> true | Error _ -> false)
  | McpToolCall { tool; _ } -> Printf.sprintf "MCP tool call: %s" tool

(** Convert to human-readable string with timestamp *)
let to_string event =
  let tm = Datetime.localtime (Datetime.to_float event.timestamp) in
  let timestamp =
    Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec
  in
  let level_str =
    match event.level with
    | Error -> "[ERROR]"
    | Warn -> "[WARN]"
    | Info -> ""
    | Debug -> "[DEBUG]"
    | Trace -> "[TRACE]"
  in
  let msg = display event.kind in
  if level_str = "" then Printf.sprintf "[%s] %s" timestamp msg
  else Printf.sprintf "[%s] %s %s" timestamp level_str msg

(** Convert kind to JSON *)
let kind_to_json = function
  | BuildComplete { duration_ms; results; succeeded; failed } ->
      Json.Object [
        ("duration_ms", Json.Int duration_ms);
        ("succeeded", Json.Array (List.map (fun s -> Json.String s) succeeded));
        ("failed", Json.Array (List.map (fun s -> Json.String s) failed));
      ]
  | BuildGraphCreated { nodes; duration_ms } ->
      Json.Object [
        ("nodes", Json.Int nodes);
        ("duration_ms", Json.Int duration_ms);
      ]
  | BuildGraphCreating ->
      Json.Object []
  | BuildStarted { packages; total_modules; workers } ->
      Json.Object [
        ("packages", Json.Array (List.map (fun p -> Json.String p) packages));
        ("total_modules", Json.Int total_modules);
        ("workers", Json.Int workers);
      ]
  | CacheHit { package; hash } ->
      Json.Object [
        ("package", Json.String package);
        ("hash", Json.String hash);
      ]
  | CacheMiss { package; hash } ->
      Json.Object [
        ("package", Json.String package);
        ("hash", Json.String hash);
      ]
  | PackageStarted { package } ->
      Json.Object [
        ("package", Json.String package);
      ]
  | PackageComplete { package; success; duration_ms; modules_compiled; cache_hits; cache_misses; _ } ->
      Json.Object [
        ("package", Json.String package);
        ("success", Json.Bool success);
        ("duration_ms", Json.Int duration_ms);
        ("modules_compiled", Json.Int modules_compiled);
        ("cache_hits", Json.Int cache_hits);
        ("cache_misses", Json.Int cache_misses);
      ]
  | CompileError { package; file; line; column; message; hint } ->
      Json.Object [
        ("package", Json.String package);
        ("file", Json.String file);
        ("line", Json.Int line);
        ("column", match column with Some c -> Json.Int c | None -> Json.Null);
        ("message", Json.String message);
        ("hint", match hint with Some h -> Json.String h | None -> Json.Null);
      ]
  | CacheStored { package; hash; artifacts } ->
      Json.Object [
        ("package", Json.String package);
        ("hash", Json.String hash);
        ("artifacts", Json.Array (List.map (fun a -> Json.String a) artifacts));
      ]
  | CompilingImplementation { package; file } ->
      Json.Object [
        ("package", Json.String package);
        ("file", Json.String file);
      ]
  | CompilingInterface { package; file } ->
      Json.Object [
        ("package", Json.String package);
        ("file", Json.String file);
      ]
  | ComputingHash { package } ->
      Json.Object [
        ("package", Json.String package);
      ]
  | CopyingFile { source; dest } ->
      Json.Object [
        ("source", Json.String source);
        ("dest", Json.String dest);
      ]
  | CreatingDirectory { path } ->
      Json.Object [
        ("path", Json.String path);
      ]
  | CycleDetected { packages } ->
      Json.Object [
        ("packages", Json.Array (List.map (fun p -> Json.String p) packages));
      ]
  | DependencyMissing { package; missing } ->
      Json.Object [
        ("package", Json.String package);
        ("missing", Json.Array (List.map (fun m -> Json.String m) missing));
      ]
  | DependencySatisfied { package } ->
      Json.Object [
        ("package", Json.String package);
      ]
  | HashComputed { package; hash } ->
      Json.Object [
        ("package", Json.String package);
        ("hash", Json.String hash);
      ]
  | LinkingExecutable { package; output } ->
      Json.Object [
        ("package", Json.String package);
        ("output", Json.String output);
      ]
  | LinkingLibrary { package; output } ->
      Json.Object [
        ("package", Json.String package);
        ("output", Json.String output);
      ]
  | McpToolCall { tool; args } ->
      Json.Object [
        ("tool", Json.String tool);
        ("args", args);
      ]
  | QueuePackage { package; queue_type } ->
      Json.Object [
        ("package", Json.String package);
        ("queue_type", Json.String (match queue_type with `Ready -> "ready" | `Waiting -> "waiting"));
      ]
  | QueueStats { ready; waiting; busy } ->
      Json.Object [
        ("ready", Json.Int ready);
        ("waiting", Json.Int waiting);
        ("busy", Json.Int busy);
      ]
  | RpcRequestReceived { request_type; args } ->
      Json.Object [
        ("request_type", Json.String request_type);
        ("args", args);
      ]
  | RpcResponseSent { result } ->
      Json.Object [
        ("success", Json.Bool (match result with Ok _ -> true | Error _ -> false));
        ("error", match result with Ok _ -> Json.Null | Error e -> Json.String e);
      ]
  | ServerRestarted { packages; toolchain } ->
      Json.Object [
        ("packages", Json.Int packages);
        ("toolchain", Json.String toolchain);
      ]
  | ServerScanning { root } ->
      Json.Object [
        ("root", Json.String root);
      ]
  | ServerShutdown ->
      Json.Object []
  | ServerStarted { pid } ->
      Json.Object [
        ("pid", Json.String pid);
      ]
  | WorkerAssigned { worker_id; package } ->
      Json.Object [
        ("worker_id", Json.String (Worker_id.to_string worker_id));
        ("package", Json.String package);
      ]
  | WorkerIdle { worker_id } ->
      Json.Object [
        ("worker_id", Json.String (Worker_id.to_string worker_id));
      ]
  | WorkerPoolStarted { workers } ->
      Json.Object [
        ("workers", Json.Int workers);
      ]
  | WorkerStarted { worker_id } ->
      Json.Object [
        ("worker_id", Json.String (Worker_id.to_string worker_id));
      ]
  | WorkspaceEmpty ->
      Json.Object []
  | WorkspaceScanning ->
      Json.Object []
  | WorkspaceScanned { packages; duration_ms } ->
      Json.Object [
        ("packages", Json.Int packages);
        ("duration_ms", Json.Int duration_ms);
      ]
  | WritingFile { path } ->
      Json.Object [
        ("path", Json.String path);
      ]

(** Convert event to JSON *)
let to_json event =
  let tm = Datetime.localtime (Datetime.to_float event.timestamp) in
  let timestamp = Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec in
  Json.Object [
    ("timestamp", Json.String timestamp);
    ("session_id", Json.String (Session_id.to_string event.session_id));
    ("level", Json.String (level_to_string event.level));
    ("event", Json.String (name event.kind));
    ("message", Json.String (display event.kind));
    ("data", kind_to_json event.kind);
  ]

(** Convert kind from JSON *)
let kind_from_json json =
  match json with
  | Json.Object fields -> (
      match List.assoc_opt "event" fields with
      | Some (Json.String event_name) ->
          let data = List.assoc_opt "data" fields |> Option.value ~default:(Json.Object []) in
          (match event_name with
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
                          (function Json.String s -> Some s | _ -> None)
                          arr
                    | _ -> []
                  in
                  let failed =
                    match List.assoc_opt "failed" data_fields with
                    | Some (Json.Array arr) ->
                        List.filter_map
                          (function Json.String s -> Some s | _ -> None)
                          arr
                    | _ -> []
                  in
                  Ok (BuildComplete { duration_ms; results = []; succeeded; failed })
              | _ -> Error "Invalid BuildComplete data")
          | "tusk.build.started" -> (
              match data with
              | Json.Object data_fields ->
                  let packages =
                    match List.assoc_opt "packages" data_fields with
                    | Some (Json.Array arr) ->
                        List.filter_map
                          (function Json.String s -> Some s | _ -> None)
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
              | _ -> Error "Invalid BuildStarted data")
          | "tusk.build.package.started" -> (
              match data with
              | Json.Object data_fields ->
                  let package =
                    match List.assoc_opt "package" data_fields with
                    | Some (Json.String p) -> p
                    | _ -> ""
                  in
                  Ok (PackageStarted { package })
              | _ -> Error "Invalid PackageStarted data")
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
                  Ok (PackageComplete { package; success; duration_ms; modules_compiled; cache_hits; cache_misses; errors = [] })
              | _ -> Error "Invalid PackageComplete data")
          | _ -> Error ("Unknown event type: " ^ event_name))
      | _ -> Error "Missing event field")
  | _ -> Error "Invalid JSON format"

(** Convert from JSON *)
let from_json json =
  match json with
  | Json.Object fields ->
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
      (match kind_from_json json with
      | Ok kind -> Ok { timestamp; session_id; level; kind }
      | Error e -> Error e)
  | _ -> Error "Invalid JSON format for Event"
