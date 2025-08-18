(** Tusk JSON-RPC Protocol - JSON-RPC 2.0 compliant *)

(** Method names *)
let method_ping = "tusk.ping"

let method_get_build_graph = "tusk.getBuildGraph"
let method_get_workspace_config = "tusk.getWorkspaceConfig"
let method_build_package = "tusk.buildPackage"
let method_build_all = "tusk.buildAll"
let method_restart = "tusk.restart"
let method_shutdown = "tusk.shutdown"
let method_build_event = "tusk.buildEvent"

(** Helper to create method-specific parameters *)
let build_package_params package =
  Jsonrpc.Named [ ("package", Json.String package) ]

(** TuskProtocol implementation for JSON-RPC *)
module TuskProtocol = struct
  type request = Rpc.request
  type response = Rpc.response

  (* Helper to serialize log events to JSON *)
  let log_event_of_json json =
    match json with
    | Json.Object fields -> (
        match List.assoc_opt "type" fields with
        | Some (Json.String "BuildStarted") ->
            let packages = match List.assoc_opt "packages" fields with
              | Some (Json.Array arr) -> List.filter_map (function Json.String s -> Some s | _ -> None) arr
              | _ -> []
            in
            let total_modules = match List.assoc_opt "total_modules" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            let workers = match List.assoc_opt "workers" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            Ok (Log.BuildStarted { packages; total_modules; workers })
        | Some (Json.String "BuildComplete") ->
            let duration_ms = match List.assoc_opt "duration_ms" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            let results = match List.assoc_opt "results" fields with
              | Some (Json.Array arr) ->
                  List.filter_map (function
                    | Json.Object r ->
                        let package = match List.assoc_opt "package" r with
                          | Some (Json.String s) -> s | _ -> ""
                        in
                        let success = match List.assoc_opt "success" r with
                          | Some (Json.Bool b) -> b | _ -> false
                        in
                        let duration_ms = match List.assoc_opt "duration_ms" r with
                          | Some (Json.Int n) -> n | _ -> 0
                        in
                        let modules_compiled = match List.assoc_opt "modules_compiled" r with
                          | Some (Json.Int n) -> n | _ -> 0
                        in
                        let cache_hits = match List.assoc_opt "cache_hits" r with
                          | Some (Json.Int n) -> n | _ -> 0
                        in
                        let cache_misses = match List.assoc_opt "cache_misses" r with
                          | Some (Json.Int n) -> n | _ -> 0
                        in
                        let errors = match List.assoc_opt "errors" r with
                          | Some (Json.Array errs) ->
                              List.filter_map (function
                                | Json.Object e ->
                                    let file = match List.assoc_opt "file" e with
                                      | Some (Json.String s) -> s | _ -> ""
                                    in
                                    let line = match List.assoc_opt "line" e with
                                      | Some (Json.Int n) -> n | _ -> 0
                                    in
                                    let column = match List.assoc_opt "column" e with
                                      | Some (Json.Int n) -> Some n | _ -> None
                                    in
                                    let message = match List.assoc_opt "message" e with
                                      | Some (Json.String s) -> s | _ -> ""
                                    in
                                    let hint = match List.assoc_opt "hint" e with
                                      | Some (Json.String s) -> Some s | _ -> None
                                    in
                                    Some { Log.package = ""; file; line; column; message; hint }
                                | _ -> None) errs
                          | _ -> []
                        in
                        Some { Log.package; success; duration_ms; modules_compiled; cache_hits; cache_misses; errors }
                    | _ -> None) arr
              | _ -> []
            in
            let succeeded = match List.assoc_opt "succeeded" fields with
              | Some (Json.Array arr) -> List.filter_map (function Json.String s -> Some s | _ -> None) arr
              | _ -> []
            in
            let failed = match List.assoc_opt "failed" fields with
              | Some (Json.Array arr) -> List.filter_map (function Json.String s -> Some s | _ -> None) arr
              | _ -> []
            in
            Ok (Log.BuildComplete { duration_ms; results; succeeded; failed })
        | Some (Json.String "PackageStarted") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.PackageStarted { package })
        | Some (Json.String "PackageComplete") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let success = match List.assoc_opt "success" fields with
              | Some (Json.Bool b) -> b | _ -> false
            in
            let duration_ms = match List.assoc_opt "duration_ms" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            let modules_compiled = match List.assoc_opt "modules_compiled" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            let cache_hits = match List.assoc_opt "cache_hits" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            let cache_misses = match List.assoc_opt "cache_misses" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            let errors = match List.assoc_opt "errors" fields with
              | Some (Json.Array errs) ->
                  List.filter_map (function
                    | Json.Object e ->
                        let file = match List.assoc_opt "file" e with
                          | Some (Json.String s) -> s | _ -> ""
                        in
                        let line = match List.assoc_opt "line" e with
                          | Some (Json.Int n) -> n | _ -> 0
                        in
                        let column = match List.assoc_opt "column" e with
                          | Some (Json.Int n) -> Some n | _ -> None
                        in
                        let message = match List.assoc_opt "message" e with
                          | Some (Json.String s) -> s | _ -> ""
                        in
                        let hint = match List.assoc_opt "hint" e with
                          | Some (Json.String s) -> Some s | _ -> None
                        in
                        Some { Log.package = ""; file; line; column; message; hint }
                    | _ -> None) errs
              | _ -> []
            in
            Ok (Log.PackageComplete { package; success; duration_ms; modules_compiled; cache_hits; cache_misses; errors })
        | Some (Json.String "CompileError") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let file = match List.assoc_opt "file" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let line = match List.assoc_opt "line" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            let column = match List.assoc_opt "column" fields with
              | Some (Json.Int n) -> Some n | _ -> None
            in
            let message = match List.assoc_opt "message" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let hint = match List.assoc_opt "hint" fields with
              | Some (Json.String s) -> Some s | _ -> None
            in
            Ok (Log.CompileError { package; file; line; column; message; hint })
        | Some (Json.String "CacheHit") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let hash = match List.assoc_opt "hash" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.CacheHit { package; hash })
        | Some (Json.String "CacheMiss") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let hash = match List.assoc_opt "hash" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.CacheMiss { package; hash })
        | Some (Json.String "CacheStored") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let hash = match List.assoc_opt "hash" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let artifacts = match List.assoc_opt "artifacts" fields with
              | Some (Json.Array arr) -> List.filter_map (function Json.String s -> Some s | _ -> None) arr
              | _ -> []
            in
            Ok (Log.CacheStored { package; hash; artifacts })
        | Some (Json.String "WorkerPoolStarted") ->
            let workers = match List.assoc_opt "workers" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            Ok (Log.WorkerPoolStarted { workers })
        | Some (Json.String "WorkerStarted") ->
            let worker_id = match List.assoc_opt "worker_id" fields with
              | Some (Json.String s) -> Worker_id.make (try int_of_string s with _ -> 0) | _ -> Worker_id.make 0
            in
            Ok (Log.WorkerStarted { worker_id })
        | Some (Json.String "WorkerAssigned") ->
            let worker_id = match List.assoc_opt "worker_id" fields with
              | Some (Json.String s) -> Worker_id.make (try int_of_string s with _ -> 0) | _ -> Worker_id.make 0
            in
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.WorkerAssigned { worker_id; package })
        | Some (Json.String "WorkerIdle") ->
            let worker_id = match List.assoc_opt "worker_id" fields with
              | Some (Json.String s) -> Worker_id.make (try int_of_string s with _ -> 0) | _ -> Worker_id.make 0
            in
            Ok (Log.WorkerIdle { worker_id })
        | Some (Json.String "ServerStarted") ->
            let pid = match List.assoc_opt "pid" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.ServerStarted { pid })
        | Some (Json.String "ServerScanning") ->
            let root = match List.assoc_opt "root" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.ServerScanning { root })
        | Some (Json.String "ServerRestarted") ->
            let packages = match List.assoc_opt "packages" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            let toolchain = match List.assoc_opt "toolchain" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.ServerRestarted { packages; toolchain })
        | Some (Json.String "QueuePackage") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let queue_type = match List.assoc_opt "queue_type" fields with
              | Some (Json.String "ready") -> `Ready
              | Some (Json.String "waiting") -> `Waiting
              | _ -> `Ready
            in
            Ok (Log.QueuePackage { package; queue_type })
        | Some (Json.String "QueueStats") ->
            let ready = match List.assoc_opt "ready" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            let waiting = match List.assoc_opt "waiting" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            let busy = match List.assoc_opt "busy" fields with
              | Some (Json.Int n) -> n | _ -> 0
            in
            Ok (Log.QueueStats { ready; waiting; busy })
        | Some (Json.String "DependencyMissing") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let missing = match List.assoc_opt "missing" fields with
              | Some (Json.Array arr) -> List.filter_map (function Json.String s -> Some s | _ -> None) arr
              | _ -> []
            in
            Ok (Log.DependencyMissing { package; missing })
        | Some (Json.String "DependencySatisfied") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.DependencySatisfied { package })
        | Some (Json.String "CompilingInterface") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let file = match List.assoc_opt "file" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.CompilingInterface { package; file })
        | Some (Json.String "CompilingImplementation") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let file = match List.assoc_opt "file" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.CompilingImplementation { package; file })
        | Some (Json.String "LinkingLibrary") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let output = match List.assoc_opt "output" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.LinkingLibrary { package; output })
        | Some (Json.String "LinkingExecutable") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let output = match List.assoc_opt "output" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.LinkingExecutable { package; output })
        | Some (Json.String "ComputingHash") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.ComputingHash { package })
        | Some (Json.String "HashComputed") ->
            let package = match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let hash = match List.assoc_opt "hash" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.HashComputed { package; hash })
        | Some (Json.String "CopyingFile") ->
            let source = match List.assoc_opt "source" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let dest = match List.assoc_opt "dest" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.CopyingFile { source; dest })
        | Some (Json.String "WritingFile") ->
            let path = match List.assoc_opt "path" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.WritingFile { path })
        | Some (Json.String "CreatingDirectory") ->
            let path = match List.assoc_opt "path" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.CreatingDirectory { path })
        | Some (Json.String "RpcRequestReceived") ->
            let session_id = match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s | _ -> Session_id.make ()
            in
            let request_type = match List.assoc_opt "request_type" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.RpcRequestReceived { session_id; request_type })
        | Some (Json.String "RpcResponseSent") ->
            let session_id = match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s | _ -> Session_id.make ()
            in
            let success = match List.assoc_opt "success" fields with
              | Some (Json.Bool b) -> b | _ -> false
            in
            Ok (Log.RpcResponseSent { session_id; success })
        | Some (Json.String "McpToolCall") ->
            let session_id = match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s | _ -> Session_id.make ()
            in
            let tool = match List.assoc_opt "tool" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            let args = match List.assoc_opt "args" fields with
              | Some (Json.String s) -> s | _ -> ""
            in
            Ok (Log.McpToolCall { session_id; tool; args })
        | Some (Json.String "ServerShutdown") ->
            Ok Log.ServerShutdown
        | Some (Json.String "WorkspaceEmpty") ->
            Ok Log.WorkspaceEmpty
        | _ -> Error (Json.String "Unknown log event type"))
    | _ -> Error (Json.String "Invalid log event JSON")

  let log_event_to_json = function
    | Log.BuildStarted { packages; total_modules; workers } ->
        Json.Object [
          ("type", Json.String "BuildStarted");
          ("packages", Json.Array (List.map (fun p -> Json.String p) packages));
          ("total_modules", Json.Int total_modules);
          ("workers", Json.Int workers)
        ]
    | Log.BuildComplete { duration_ms; results; succeeded; failed } ->
        Json.Object [
          ("type", Json.String "BuildComplete");
          ("duration_ms", Json.Int duration_ms);
          ("results", Json.Array (List.map (fun r -> 
            Json.Object [
              ("package", Json.String r.Log.package);
              ("success", Json.Bool r.Log.success);
              ("duration_ms", Json.Int r.Log.duration_ms);
              ("modules_compiled", Json.Int r.Log.modules_compiled);
              ("cache_hits", Json.Int r.Log.cache_hits);
              ("cache_misses", Json.Int r.Log.cache_misses);
              ("errors", Json.Array (List.map (fun e ->
                Json.Object [
                  ("file", Json.String e.Log.file);
                  ("line", Json.Int e.Log.line);
                  ("column", match e.Log.column with Some c -> Json.Int c | None -> Json.Null);
                  ("message", Json.String e.Log.message);
                  ("hint", match e.Log.hint with Some h -> Json.String h | None -> Json.Null)
                ]) r.Log.errors))
            ]) results));
          ("succeeded", Json.Array (List.map (fun s -> Json.String s) succeeded));
          ("failed", Json.Array (List.map (fun f -> Json.String f) failed))
        ]
    | Log.PackageStarted { package } ->
        Json.Object [
          ("type", Json.String "PackageStarted");
          ("package", Json.String package)
        ]
    | Log.PackageComplete result ->
        Json.Object [
          ("type", Json.String "PackageComplete");
          ("package", Json.String result.Log.package);
          ("success", Json.Bool result.Log.success);
          ("duration_ms", Json.Int result.Log.duration_ms);
          ("modules_compiled", Json.Int result.Log.modules_compiled);
          ("cache_hits", Json.Int result.Log.cache_hits);
          ("cache_misses", Json.Int result.Log.cache_misses);
          ("errors", Json.Array (List.map (fun e ->
            Json.Object [
              ("file", Json.String e.Log.file);
              ("line", Json.Int e.Log.line);
              ("column", match e.Log.column with Some c -> Json.Int c | None -> Json.Null);
              ("message", Json.String e.Log.message);
              ("hint", match e.Log.hint with Some h -> Json.String h | None -> Json.Null)
            ]) result.Log.errors))
        ]
    | Log.CompileError { package; file; line; column; message; hint } ->
        Json.Object [
          ("type", Json.String "CompileError");
          ("package", Json.String package);
          ("file", Json.String file);
          ("line", Json.Int line);
          ("column", match column with Some c -> Json.Int c | None -> Json.Null);
          ("message", Json.String message);
          ("hint", match hint with Some h -> Json.String h | None -> Json.Null)
        ]
    | Log.CacheHit { package; hash } ->
        Json.Object [
          ("type", Json.String "CacheHit");
          ("package", Json.String package);
          ("hash", Json.String hash)
        ]
    | Log.CacheMiss { package; hash } ->
        Json.Object [
          ("type", Json.String "CacheMiss");
          ("package", Json.String package);
          ("hash", Json.String hash)
        ]
    | Log.CacheStored { package; hash; artifacts } ->
        Json.Object [
          ("type", Json.String "CacheStored");
          ("package", Json.String package);
          ("hash", Json.String hash);
          ("artifacts", Json.Array (List.map (fun a -> Json.String a) artifacts))
        ]
    | Log.WorkerPoolStarted { workers } ->
        Json.Object [
          ("type", Json.String "WorkerPoolStarted");
          ("workers", Json.Int workers)
        ]
    | Log.WorkerStarted { worker_id } ->
        Json.Object [
          ("type", Json.String "WorkerStarted");
          ("worker_id", Json.String (Worker_id.to_string worker_id))
        ]
    | Log.WorkerAssigned { worker_id; package } ->
        Json.Object [
          ("type", Json.String "WorkerAssigned");
          ("worker_id", Json.String (Worker_id.to_string worker_id));
          ("package", Json.String package)
        ]
    | Log.WorkerIdle { worker_id } ->
        Json.Object [
          ("type", Json.String "WorkerIdle");
          ("worker_id", Json.String (Worker_id.to_string worker_id))
        ]
    | Log.ServerStarted { pid } ->
        Json.Object [
          ("type", Json.String "ServerStarted");
          ("pid", Json.String pid)
        ]
    | Log.ServerScanning { root } ->
        Json.Object [
          ("type", Json.String "ServerScanning");
          ("root", Json.String root)
        ]
    | Log.ServerRestarted { packages; toolchain } ->
        Json.Object [
          ("type", Json.String "ServerRestarted");
          ("packages", Json.Int packages);
          ("toolchain", Json.String toolchain)
        ]
    | Log.QueuePackage { package; queue_type } ->
        Json.Object [
          ("type", Json.String "QueuePackage");
          ("package", Json.String package);
          ("queue_type", Json.String (match queue_type with `Ready -> "ready" | `Waiting -> "waiting"))
        ]
    | Log.QueueStats { ready; waiting; busy } ->
        Json.Object [
          ("type", Json.String "QueueStats");
          ("ready", Json.Int ready);
          ("waiting", Json.Int waiting);
          ("busy", Json.Int busy)
        ]
    | Log.DependencyMissing { package; missing } ->
        Json.Object [
          ("type", Json.String "DependencyMissing");
          ("package", Json.String package);
          ("missing", Json.Array (List.map (fun m -> Json.String m) missing))
        ]
    | Log.DependencySatisfied { package } ->
        Json.Object [
          ("type", Json.String "DependencySatisfied");
          ("package", Json.String package)
        ]
    | Log.CompilingInterface { package; file } ->
        Json.Object [
          ("type", Json.String "CompilingInterface");
          ("package", Json.String package);
          ("file", Json.String file)
        ]
    | Log.CompilingImplementation { package; file } ->
        Json.Object [
          ("type", Json.String "CompilingImplementation");
          ("package", Json.String package);
          ("file", Json.String file)
        ]
    | Log.LinkingLibrary { package; output } ->
        Json.Object [
          ("type", Json.String "LinkingLibrary");
          ("package", Json.String package);
          ("output", Json.String output)
        ]
    | Log.LinkingExecutable { package; output } ->
        Json.Object [
          ("type", Json.String "LinkingExecutable");
          ("package", Json.String package);
          ("output", Json.String output)
        ]
    | Log.ComputingHash { package } ->
        Json.Object [
          ("type", Json.String "ComputingHash");
          ("package", Json.String package)
        ]
    | Log.HashComputed { package; hash } ->
        Json.Object [
          ("type", Json.String "HashComputed");
          ("package", Json.String package);
          ("hash", Json.String hash)
        ]
    | Log.CopyingFile { source; dest } ->
        Json.Object [
          ("type", Json.String "CopyingFile");
          ("source", Json.String source);
          ("dest", Json.String dest)
        ]
    | Log.WritingFile { path } ->
        Json.Object [
          ("type", Json.String "WritingFile");
          ("path", Json.String path)
        ]
    | Log.CreatingDirectory { path } ->
        Json.Object [
          ("type", Json.String "CreatingDirectory");
          ("path", Json.String path)
        ]
    | Log.RpcRequestReceived { session_id; request_type } ->
        Json.Object [
          ("type", Json.String "RpcRequestReceived");
          ("session_id", Json.String (Session_id.to_string session_id));
          ("request_type", Json.String request_type)
        ]
    | Log.RpcResponseSent { session_id; success } ->
        Json.Object [
          ("type", Json.String "RpcResponseSent");
          ("session_id", Json.String (Session_id.to_string session_id));
          ("success", Json.Bool success)
        ]
    | Log.McpToolCall { session_id; tool; args } ->
        Json.Object [
          ("type", Json.String "McpToolCall");
          ("session_id", Json.String (Session_id.to_string session_id));
          ("tool", Json.String tool);
          ("args", Json.String args)
        ]
    | Log.ServerShutdown ->
        Json.Object [
          ("type", Json.String "ServerShutdown")
        ]
    | Log.WorkspaceEmpty ->
        Json.Object [
          ("type", Json.String "WorkspaceEmpty")
        ]

  let request_to_params = function
    | Rpc.Ping -> { Jsonrpc.method_ = method_ping; params = NoParams }
    | Rpc.GetWorkspaceConfig -> { method_ = method_get_workspace_config; params = NoParams }
    | Rpc.GetBuildGraph -> { method_ = method_get_build_graph; params = NoParams }
    | Rpc.BuildPackage pkg -> { method_ = method_build_package; params = build_package_params pkg }
    | Rpc.BuildAll -> { method_ = method_build_all; params = NoParams }
    | Rpc.Shutdown -> { method_ = method_shutdown; params = NoParams }
    | Rpc.Restart -> { method_ = method_restart; params = NoParams }

  let request_of_params params =
    (* This would parse params back to request, but we don't need it for server *)
    Error (Json.String "Not implemented")

  let response_to_json = function
    | Rpc.Pong -> Json.String "pong"
    | Rpc.WorkspaceConfig config ->
        Json.Object [
          ("workspace_root", Json.String config.workspace_root);
          ("toolchain", Json.String config.toolchain);
          ("packages", Json.Array (List.map (fun p -> Json.String p) config.packages))
        ]
    | Rpc.BuildGraph graph ->
        Json.Object [
          ("nodes", Json.Array (List.map (fun (node : Rpc.build_node) ->
            Json.Object [
              ("package_name", Json.String node.Rpc.package_name);
              ("src_dir", Json.String node.Rpc.src_dir);
              ("out_dir", Json.String node.Rpc.out_dir);
              ("status", Json.String node.Rpc.status);
              ("deps", Json.Array (List.map (fun d -> Json.String d) node.Rpc.deps))
            ]) graph.Rpc.nodes))
        ]
    | Rpc.BuildStarted { session_id } ->
        Json.Object [
          ("type", Json.String "build_started");
          ("session_id", Json.String (Session_id.to_string session_id))
        ]
    | Rpc.BuildEvent { session_id; log_event } ->
        (* Serialize log event with type information *)
        let event_type = match log_event with
          | Log.BuildStarted _ -> "BuildStarted"
          | Log.BuildComplete _ -> "BuildComplete"
          | Log.PackageStarted _ -> "PackageStarted"
          | Log.PackageComplete _ -> "PackageComplete"
          | Log.CompileError _ -> "CompileError"
          | Log.CacheHit _ -> "CacheHit"
          | Log.CacheMiss _ -> "CacheMiss"
          | Log.CacheStored _ -> "CacheStored"
          | Log.WorkerPoolStarted _ -> "WorkerPoolStarted"
          | Log.WorkerStarted _ -> "WorkerStarted"
          | Log.WorkerAssigned _ -> "WorkerAssigned"
          | Log.WorkerIdle _ -> "WorkerIdle"
          | Log.ServerStarted _ -> "ServerStarted"
          | Log.ServerScanning _ -> "ServerScanning"
          | Log.ServerRestarted _ -> "ServerRestarted"
          | Log.QueuePackage _ -> "QueuePackage"
          | Log.QueueStats _ -> "QueueStats"
          | Log.DependencyMissing _ -> "DependencyMissing"
          | Log.DependencySatisfied _ -> "DependencySatisfied"
          | Log.CompilingInterface _ -> "CompilingInterface"
          | Log.CompilingImplementation _ -> "CompilingImplementation"
          | Log.LinkingLibrary _ -> "LinkingLibrary"
          | Log.LinkingExecutable _ -> "LinkingExecutable"
          | Log.ComputingHash _ -> "ComputingHash"
          | Log.HashComputed _ -> "HashComputed"
          | Log.CopyingFile _ -> "CopyingFile"
          | Log.WritingFile _ -> "WritingFile"
          | Log.CreatingDirectory _ -> "CreatingDirectory"
          | Log.RpcRequestReceived _ -> "RpcRequestReceived"
          | Log.RpcResponseSent _ -> "RpcResponseSent"
          | Log.McpToolCall _ -> "McpToolCall"
          | Log.ServerShutdown -> "ServerShutdown"
          | Log.WorkspaceEmpty -> "WorkspaceEmpty"
        in
        let event_str = Log.event_to_string log_event in
        Json.Object [
          ("type", Json.String "build_event");
          ("session_id", Json.String (Session_id.to_string session_id));
          ("event_type", Json.String event_type);
          ("message", Json.String event_str);
          ("event_data", log_event_to_json log_event);
        ]
    | Rpc.Success -> Json.String "success"
    | Rpc.Error msg -> Json.Object [("error", Json.String msg)]

  let response_of_json json =
    (* This would parse JSON back to response, needed for client *)
    match json with
    | Json.String "pong" -> Ok Rpc.Pong
    | Json.String "success" -> Ok Rpc.Success
    | Json.Object fields -> (
        match List.assoc_opt "type" fields with
        | Some (Json.String "build_started") ->
            let session_id = match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            Ok (Rpc.BuildStarted { session_id })
        | Some (Json.String "build_event") ->
            let session_id = match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            (* Use proper deserialization of log events *)
            let log_event = match List.assoc_opt "event_data" fields with
              | Some event_json -> (
                  match log_event_of_json event_json with
                  | Ok evt -> evt
                  | Error _ -> Log.BuildStarted { packages = []; total_modules = 0; workers = 0 })
              | None -> Log.BuildStarted { packages = []; total_modules = 0; workers = 0 }
            in
            Ok (Rpc.BuildEvent { session_id; log_event })
        | _ ->
            (* Try other response types *)
            match List.assoc_opt "workspace_root" fields with
            | Some (Json.String _) ->
                let workspace_root = match List.assoc_opt "workspace_root" fields with
                  | Some (Json.String s) -> s | _ -> ""
                in
                let toolchain = match List.assoc_opt "toolchain" fields with
                  | Some (Json.String s) -> s | _ -> ""
                in
                let packages = match List.assoc_opt "packages" fields with
                  | Some (Json.Array arr) ->
                      List.filter_map (function Json.String s -> Some s | _ -> None) arr
                  | _ -> []
                in
                Ok (Rpc.WorkspaceConfig { workspace_root; toolchain; packages })
            | _ ->
                match List.assoc_opt "nodes" fields with
                | Some (Json.Array _) ->
                    let nodes = match List.assoc_opt "nodes" fields with
                      | Some (Json.Array arr) ->
                          List.filter_map (function
                            | Json.Object node_fields ->
                                let package_name = match List.assoc_opt "package_name" node_fields with
                                  | Some (Json.String s) -> s | _ -> ""
                                in
                                let src_dir = match List.assoc_opt "src_dir" node_fields with
                                  | Some (Json.String s) -> s | _ -> ""
                                in
                                let out_dir = match List.assoc_opt "out_dir" node_fields with
                                  | Some (Json.String s) -> s | _ -> ""
                                in
                                let status = match List.assoc_opt "status" node_fields with
                                  | Some (Json.String s) -> s | _ -> ""
                                in
                                let deps = match List.assoc_opt "deps" node_fields with
                                  | Some (Json.Array d) ->
                                      List.filter_map (function Json.String s -> Some s | _ -> None) d
                                  | _ -> []
                                in
                                Some Rpc.{ package_name; src_dir; out_dir; status; deps }
                            | _ -> None) arr
                      | _ -> []
                    in
                    Ok (Rpc.BuildGraph { nodes })
                | _ ->
                    match List.assoc_opt "error" fields with
                    | Some (Json.String msg) -> Ok (Rpc.Error msg)
                    | _ -> Error json)
    | _ -> Error json
end

(** Client module for Tusk RPC *)
module Client = struct
  open Miniriot
  
  type t = { 
    client : (TuskProtocol.request, TuskProtocol.response) Jsonrpc.Client.t; 
    transport : Net.TcpClient.t 
  }
  
  (** Build request type *)
  type build_request =
    | BuildPackage of string
    | BuildAll
  
  (** Streaming build event *)
  type streaming_event =
    | BuildStarted of Session_id.t
    | BuildEvent of Log.log_event
    | BuildFinished of (unit, string) result
  
  (** Create a new Tusk RPC client *)
  let create ~host ~port =
    (* Create TCP transport using Net.TcpClient *)
    match Net.TcpClient.connect ~host ~port with
    | Ok transport ->
        let client = Jsonrpc.Client.create 
          ~transport:(module Net.TcpClient) 
          ~protocol:(module TuskProtocol)
          transport in
        Ok { client; transport }
    | Error e -> 
        let error_msg = match e with
        | `Connection_refused -> "Connection refused"
        | `Closed -> "Connection closed"
        | `System_error msg -> Printf.sprintf "System error: %s" msg
        in
        Error (Printf.sprintf "Failed to connect to server: %s" error_msg)
  
  (** Close the client *)
  let close t =
    (* Jsonrpc.Client.close already closes the transport *)
    Jsonrpc.Client.close t.client
  
  (** Ping the server *)
  let ping t =
    match
      Jsonrpc.Client.call t.client ~method_:method_ping
        ~params:Jsonrpc.NoParams ()
    with
    | Ok _ -> Ok ()
    | Error e ->
        Error
          (Printf.sprintf "Error %d: %s"
             (Jsonrpc.error_code_to_int e.code)
             e.message)
  
  (** Get workspace configuration *)
  let get_workspace_config t =
    match
      Jsonrpc.Client.call t.client
        ~method_:method_get_workspace_config ~params:Jsonrpc.NoParams
        ()
    with
    | Ok (Rpc.WorkspaceConfig config) -> Ok config
    | Ok _ -> Error "Invalid workspace config response"
    | Error e ->
        Error
          (Printf.sprintf "Error %d: %s"
             (Jsonrpc.error_code_to_int e.code)
             e.message)
  
  (** Get build graph *)
  let get_build_graph t =
    match
      Jsonrpc.Client.call t.client ~method_:method_get_build_graph
        ~params:Jsonrpc.NoParams ()
    with
    | Ok (Rpc.BuildGraph graph) -> Ok graph
    | Ok _ -> Error "Invalid build graph response"
    | Error e ->
        Error
          (Printf.sprintf "Error %d: %s"
             (Jsonrpc.error_code_to_int e.code)
             e.message)
  
  (** Build with streaming support *)
  let build_streaming t request callback =
    let typed_request =
      match request with
      | BuildPackage pkg -> Rpc.BuildPackage pkg
      | BuildAll -> Rpc.BuildAll
    in
    
    (* Send the typed build request - this starts a streaming response *)
    match Jsonrpc.Client.send_request t.client typed_request with
    | Error e -> Error (Printf.sprintf "Failed to send request: %s" e)
    | Ok () -> (
        (* Receive the first response *)
        match Jsonrpc.Client.receive_response t.client with
        | Error e -> Error (Printf.sprintf "Failed to receive response: %s" e)
        | Ok response -> (
            match response.Jsonrpc.result with
            | Ok (Rpc.BuildStarted { session_id }) ->
                (* Got BuildStarted *)
                callback (BuildStarted session_id);
                
                (* Now receive streaming events until build completes *)
                let rec receive_events () =
                  match Jsonrpc.Client.receive_response t.client with
                  | Ok { result = Ok (Rpc.BuildEvent { session_id = _; log_event }); _ } ->
                      callback (BuildEvent log_event);
                      receive_events ()
                  | Ok { result = Ok (Rpc.Success); _ } ->
                      Ok (BuildFinished (Ok ()))
                  | Ok { result = Ok (Rpc.Error msg); _ } ->
                      Ok (BuildFinished (Error msg))
                  | Ok { result = Error err; _ } ->
                      Ok (BuildFinished (Error err.message))
                  | Error e -> Error (Printf.sprintf "Failed to receive event: %s" e)
                  | _ -> Error "Unexpected response type"
                in
                receive_events ()
            | Ok (Rpc.Success) ->
                (* Direct success (no build needed) *)
                Ok (BuildFinished (Ok ()))
            | Ok (Rpc.Error msg) ->
                (* Direct error *)
                Ok (BuildFinished (Error msg))
            | Error err ->
                Error (Printf.sprintf "Build request failed: %s" err.Jsonrpc.message)
            | _ -> Error "Unexpected response type"))
  
  (** Shutdown the server *)
  let shutdown t =
    match
      Jsonrpc.Client.call t.client ~method_:method_shutdown
        ~params:Jsonrpc.NoParams ()
    with
    | Ok _ -> Ok ()
    | Error e ->
        Error
          (Printf.sprintf "Error %d: %s"
             (Jsonrpc.error_code_to_int e.code)
             e.message)
  
  (** Build a specific package *)
  let build_package t package =
    match
      Jsonrpc.Client.call t.client ~method_:method_build_package
        ~params:(build_package_params package) ()
    with
    | Ok response -> Ok response
    | Error e ->
        Error
          (Printf.sprintf "Error %d: %s"
             (Jsonrpc.error_code_to_int e.code)
             e.message)

  (** Build all packages *)
  let build_all t =
    match
      Jsonrpc.Client.call t.client ~method_:method_build_all
        ~params:Jsonrpc.NoParams ()
    with
    | Ok response -> Ok response
    | Error e ->
        Error
          (Printf.sprintf "Error %d: %s"
             (Jsonrpc.error_code_to_int e.code)
             e.message)

  (** Restart the server *)
  let restart t =
    match
      Jsonrpc.Client.call t.client ~method_:method_restart
        ~params:Jsonrpc.NoParams ()
    with
    | Ok _ -> Ok ()
    | Error e ->
        Error
          (Printf.sprintf "Error %d: %s"
             (Jsonrpc.error_code_to_int e.code)
             e.message)
end

(** Server module for Tusk RPC *)
module Server = struct
  open Miniriot
  
  type ctx = { server_pid : Pid.t }
  
  let handle_build ctx reply request =
    (* request is already typed - either BuildPackage or BuildAll *)
    send ctx.server_pid (Rpc.ClientRequest (self (), request));
    
    (* Wait for response *)
    let selector = function
      | Rpc.ServerResponse response -> `select response
      | _ -> `skip
    in
    match receive ~selector () with
    | Rpc.BuildStarted { session_id } ->
        (* Send BuildStarted response with session_id *)
        reply (Rpc.BuildStarted { session_id });
        
        (* Now enter receive loop for build events *)
        let rec receive_events () =
          (* Need a different selector for the event loop that accepts both Log.Event and ServerResponse *)
          let event_selector = function
            | Log.Event (sid, evt) -> `select (`log_event (sid, evt))
            | Rpc.ServerResponse resp -> `select (`server_response resp)
            | _ -> `skip
          in
          match receive ~selector:event_selector () with
          | `log_event (sid_opt, log_event) ->
              (* Use the session_id from the event if present, otherwise use the one from BuildStarted *)
              let sid = Option.value sid_opt ~default:session_id in
              reply (Rpc.BuildEvent { session_id = sid; log_event });
              receive_events () (* Continue receiving events *)
          | `server_response resp -> (
              match resp with
              | Rpc.Success ->
                  (* Build completed successfully *)
                  reply Rpc.Success
              | Rpc.Error msg ->
                  (* Build failed *)
                  reply (Rpc.Error msg)
              | _ -> receive_events ())
          (* Ignore other responses and continue *)
        in
        receive_events ()
    | _ ->
        reply (Rpc.Error "Unexpected response")
  
  let handle_ping ctx reply request =
    (* request is already Rpc.Ping *)
    send ctx.server_pid (Rpc.ClientRequest (self (), request));
    (* Wait for response *)
    let selector = function
      | Rpc.ServerResponse response -> `select response
      | _ -> `skip
    in
    match receive ~selector () with
    | Rpc.Pong ->
        reply Rpc.Pong
    | _ ->
        reply (Rpc.Error "Unexpected response")
  
  let handle_shutdown ctx reply request =
    (* request is already Rpc.Shutdown *)
    send ctx.server_pid (Rpc.ClientRequest (self (), request));
    reply Rpc.Success
  
  let handle_workspace_config ctx reply request =
    (* request is already Rpc.GetWorkspaceConfig *)
    send ctx.server_pid (Rpc.ClientRequest (self (), request));
    let selector = function
      | Rpc.ServerResponse response -> `select response
      | _ -> `skip
    in
    match receive ~selector () with
    | Rpc.WorkspaceConfig config ->
        reply (Rpc.WorkspaceConfig config)
    | Rpc.Error msg ->
        reply (Rpc.Error msg)
    | _ ->
        reply (Rpc.Error "Unexpected response")
  
  let handle_build_graph ctx reply request =
    (* request is already Rpc.GetBuildGraph *)
    send ctx.server_pid (Rpc.ClientRequest (self (), request));
    let selector = function
      | Rpc.ServerResponse response -> `select response
      | _ -> `skip
    in
    match receive ~selector () with
    | Rpc.BuildGraph graph ->
        reply (Rpc.BuildGraph graph)
    | Rpc.Error msg ->
        reply (Rpc.Error msg)
    | _ ->
        reply (Rpc.Error "Unexpected response")
  
  let handle_restart ctx reply request =
    (* request is already Rpc.Restart *)
    send ctx.server_pid (Rpc.ClientRequest (self (), request));
    reply Rpc.Success
  
  (** Create a JSON-RPC server handler for the tusk server *)
  let create server_pid =
    let ctx = { server_pid } in
    (* Create handlers that match on the request type *)
    let methods =
      [
        { 
          Jsonrpc.Server.method_ = method_ping; 
          fn = fun reply request ->
            match request with
            | Rpc.Ping -> handle_ping ctx reply request
            | _ -> ()
        };
        { 
          Jsonrpc.Server.method_ = method_build_package; 
          fn = fun reply request ->
            match request with
            | Rpc.BuildPackage _ -> handle_build ctx reply request
            | _ -> ()
        };
        { 
          Jsonrpc.Server.method_ = method_build_all; 
          fn = fun reply request ->
            match request with
            | Rpc.BuildAll -> handle_build ctx reply request
            | _ -> ()
        };
        { 
          Jsonrpc.Server.method_ = method_get_workspace_config; 
          fn = fun reply request ->
            match request with
            | Rpc.GetWorkspaceConfig -> handle_workspace_config ctx reply request
            | _ -> ()
        };
        { 
          Jsonrpc.Server.method_ = method_get_build_graph; 
          fn = fun reply request ->
            match request with
            | Rpc.GetBuildGraph -> handle_build_graph ctx reply request
            | _ -> ()
        };
        { 
          Jsonrpc.Server.method_ = method_restart; 
          fn = fun reply request ->
            match request with
            | Rpc.Restart -> handle_restart ctx reply request
            | _ -> ()
        };
        { 
          Jsonrpc.Server.method_ = method_shutdown; 
          fn = fun reply request ->
            match request with
            | Rpc.Shutdown -> handle_shutdown ctx reply request
            | _ -> ()
        };
      ]
    in
    Printf.eprintf "[RPC SERVER DEBUG] Registering methods:\n";
    List.iter
      (fun h -> Printf.eprintf "  - %s\n" h.Jsonrpc.Server.method_)
      methods;
    flush stderr;
    Jsonrpc.Server.create ~protocol:(module TuskProtocol) ~methods
end
