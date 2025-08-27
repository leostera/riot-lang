(** Tusk JSON-RPC Protocol - JSON-RPC 2.0 compliant *)

(** Method names *)
let method_ping = "tusk.ping"

let method_get_build_graph = "tusk.getBuildGraph"
let method_get_workspace_config = "tusk.getWorkspaceConfig"
let method_get_package_info = "tusk.getPackageInfo"
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
  (* Define request/response types for JSON-RPC communication *)
  type build_node = {
    package_name : string;
    src_dir : string;
    out_dir : string;
    status : string;
    deps : string list;
  }

  type build_graph_response = { nodes : build_node list }

  type package_info = {
    name : string;
    path : string;
    dependencies : string list;
  }

  type workspace_config = {
    workspace_root : string;
    target_dir : string;
    toolchain : string;
    toolchain_path : string;
    packages : package_info list;
    total_packages : int;
  }

  type package_detail = {
    package : package_info;
    sources : string list;
    dependency_names : string list;
  }

  type request =
    | Ping
    | GetBuildGraph
    | GetWorkspaceConfig
    | GetPackageInfo of string
    | BuildPackage of string
    | BuildAll
    | Restart
    | Shutdown

  type build_stats = {
    duration_ms : int;
    packages_built : int;
    packages_failed : int;
    total_modules : int;
    cache_hits : int;
    cache_misses : int;
  }

  type response =
    | Pong
    | BuildGraph of build_graph_response
    | WorkspaceConfig of workspace_config
    | PackageInfo of package_detail
    | BuildStarted of { session_id : Session_id.t; started_at : Std.Datetime.t }
    | BuildEvent of { session_id : Session_id.t; event : Event.t }
    | CycleDetected of { session_id : Session_id.t; detected_at : Std.Datetime.t; cycle_nodes : string list }
    | BuildComplete of { session_id : Session_id.t; completed_at : Std.Datetime.t; stats : build_stats }
    | BuildFailed of {
        session_id : Session_id.t;
        failed_at : Std.Datetime.t;
        stats : build_stats;
        error : string;
      }
    | ShutdownAck
    | RestartAck
    | Error of string

  (* Helper to deserialize event kind from JSON *)
  let event_kind_of_json json =
    match json with
    | Json.Object fields -> (
        match List.assoc_opt "type" fields with
        | Some (Json.String "BuildStarted") ->
            let packages =
              match List.assoc_opt "packages" fields with
              | Some (Json.Array arr) ->
                  List.filter_map
                    (function Json.String s -> Some s | _ -> None)
                    arr
              | _ -> []
            in
            let total_modules =
              match List.assoc_opt "total_modules" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let workers =
              match List.assoc_opt "workers" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            Ok (Event.BuildStarted { packages; total_modules; workers })
        | Some (Json.String "BuildComplete") ->
            let duration_ms =
              match List.assoc_opt "duration_ms" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let results =
              match List.assoc_opt "results" fields with
              | Some (Json.Array arr) ->
                  List.filter_map
                    (function
                      | Json.Object r ->
                          let package =
                            match List.assoc_opt "package" r with
                            | Some (Json.String s) -> s
                            | _ -> ""
                          in
                          let success =
                            match List.assoc_opt "success" r with
                            | Some (Json.Bool b) -> b
                            | _ -> false
                          in
                          let duration_ms =
                            match List.assoc_opt "duration_ms" r with
                            | Some (Json.Int n) -> n
                            | _ -> 0
                          in
                          let modules_compiled =
                            match List.assoc_opt "modules_compiled" r with
                            | Some (Json.Int n) -> n
                            | _ -> 0
                          in
                          let cache_hits =
                            match List.assoc_opt "cache_hits" r with
                            | Some (Json.Int n) -> n
                            | _ -> 0
                          in
                          let cache_misses =
                            match List.assoc_opt "cache_misses" r with
                            | Some (Json.Int n) -> n
                            | _ -> 0
                          in
                          let errors =
                            match List.assoc_opt "errors" r with
                            | Some (Json.Array errs) ->
                                List.filter_map
                                  (function
                                    | Json.Object e ->
                                        let file =
                                          match List.assoc_opt "file" e with
                                          | Some (Json.String s) -> s
                                          | _ -> ""
                                        in
                                        let line =
                                          match List.assoc_opt "line" e with
                                          | Some (Json.Int n) -> n
                                          | _ -> 0
                                        in
                                        let column =
                                          match List.assoc_opt "column" e with
                                          | Some (Json.Int n) -> Some n
                                          | _ -> None
                                        in
                                        let message =
                                          match List.assoc_opt "message" e with
                                          | Some (Json.String s) -> s
                                          | _ -> ""
                                        in
                                        let hint =
                                          match List.assoc_opt "hint" e with
                                          | Some (Json.String s) -> Some s
                                          | _ -> None
                                        in
                                        Some
                                          {
                                            Event.package = "";
                                            file;
                                            line;
                                            column;
                                            message;
                                            hint;
                                          }
                                    | _ -> None)
                                  errs
                            | _ -> []
                          in
                          Some
                            {
                              Event.package;
                              success;
                              duration_ms;
                              modules_compiled;
                              cache_hits;
                              cache_misses;
                              errors;
                            }
                      | _ -> None)
                    arr
              | _ -> []
            in
            let succeeded =
              match List.assoc_opt "succeeded" fields with
              | Some (Json.Array arr) ->
                  List.filter_map
                    (function Json.String s -> Some s | _ -> None)
                    arr
              | _ -> []
            in
            let failed =
              match List.assoc_opt "failed" fields with
              | Some (Json.Array arr) ->
                  List.filter_map
                    (function Json.String s -> Some s | _ -> None)
                    arr
              | _ -> []
            in
            Ok (Event.BuildComplete { duration_ms; results; succeeded; failed })
        | Some (Json.String "PackageStarted") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.PackageStarted { package })
        | Some (Json.String "PackageComplete") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let success =
              match List.assoc_opt "success" fields with
              | Some (Json.Bool b) -> b
              | _ -> false
            in
            let duration_ms =
              match List.assoc_opt "duration_ms" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let modules_compiled =
              match List.assoc_opt "modules_compiled" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let cache_hits =
              match List.assoc_opt "cache_hits" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let cache_misses =
              match List.assoc_opt "cache_misses" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let errors =
              match List.assoc_opt "errors" fields with
              | Some (Json.Array errs) ->
                  List.filter_map
                    (function
                      | Json.Object e ->
                          let file =
                            match List.assoc_opt "file" e with
                            | Some (Json.String s) -> s
                            | _ -> ""
                          in
                          let line =
                            match List.assoc_opt "line" e with
                            | Some (Json.Int n) -> n
                            | _ -> 0
                          in
                          let column =
                            match List.assoc_opt "column" e with
                            | Some (Json.Int n) -> Some n
                            | _ -> None
                          in
                          let message =
                            match List.assoc_opt "message" e with
                            | Some (Json.String s) -> s
                            | _ -> ""
                          in
                          let hint =
                            match List.assoc_opt "hint" e with
                            | Some (Json.String s) -> Some s
                            | _ -> None
                          in
                          Some
                            {
                              Event.package = "";
                              file;
                              line;
                              column;
                              message;
                              hint;
                            }
                      | _ -> None)
                    errs
              | _ -> []
            in
            Ok
              (Event.PackageComplete
                 {
                   package;
                   success;
                   duration_ms;
                   modules_compiled;
                   cache_hits;
                   cache_misses;
                   errors;
                 })
        | Some (Json.String "CompileError") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let file =
              match List.assoc_opt "file" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let line =
              match List.assoc_opt "line" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let column =
              match List.assoc_opt "column" fields with
              | Some (Json.Int n) -> Some n
              | _ -> None
            in
            let message =
              match List.assoc_opt "message" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let hint =
              match List.assoc_opt "hint" fields with
              | Some (Json.String s) -> Some s
              | _ -> None
            in
            Ok
              (Event.CompileError { package; file; line; column; message; hint })
        | Some (Json.String "CycleDetected") ->
            let packages =
              match List.assoc_opt "packages" fields with
              | Some (Json.Array arr) ->
                  List.filter_map
                    (function Json.String s -> Some s | _ -> None)
                    arr
              | _ -> []
            in
            Ok (Event.CycleDetected { packages })
        | Some (Json.String "CacheHit") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let hash =
              match List.assoc_opt "hash" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.CacheHit { package; hash })
        | Some (Json.String "CacheMiss") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let hash =
              match List.assoc_opt "hash" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.CacheMiss { package; hash })
        | Some (Json.String "CacheStored") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let hash =
              match List.assoc_opt "hash" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let artifacts =
              match List.assoc_opt "artifacts" fields with
              | Some (Json.Array arr) ->
                  List.filter_map
                    (function Json.String s -> Some s | _ -> None)
                    arr
              | _ -> []
            in
            Ok (Event.CacheStored { package; hash; artifacts })
        | Some (Json.String "WorkerPoolStarted") ->
            let workers =
              match List.assoc_opt "workers" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            Ok (Event.WorkerPoolStarted { workers })
        | Some (Json.String "WorkerStarted") ->
            let worker_id =
              match List.assoc_opt "worker_id" fields with
              | Some (Json.String s) ->
                  Worker_id.make (try int_of_string s with _ -> 0)
              | _ -> Worker_id.make 0
            in
            Ok (Event.WorkerStarted { worker_id })
        | Some (Json.String "WorkerAssigned") ->
            let worker_id =
              match List.assoc_opt "worker_id" fields with
              | Some (Json.String s) ->
                  Worker_id.make (try int_of_string s with _ -> 0)
              | _ -> Worker_id.make 0
            in
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.WorkerAssigned { worker_id; package })
        | Some (Json.String "WorkerIdle") ->
            let worker_id =
              match List.assoc_opt "worker_id" fields with
              | Some (Json.String s) ->
                  Worker_id.make (try int_of_string s with _ -> 0)
              | _ -> Worker_id.make 0
            in
            Ok (Event.WorkerIdle { worker_id })
        | Some (Json.String "ServerStarted") ->
            let pid =
              match List.assoc_opt "pid" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.ServerStarted { pid })
        | Some (Json.String "ServerScanning") ->
            let root =
              match List.assoc_opt "root" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.ServerScanning { root })
        | Some (Json.String "ServerRestarted") ->
            let packages =
              match List.assoc_opt "packages" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let toolchain =
              match List.assoc_opt "toolchain" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.ServerRestarted { packages; toolchain })
        | Some (Json.String "QueuePackage") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let queue_type =
              match List.assoc_opt "queue_type" fields with
              | Some (Json.String "ready") -> `Ready
              | Some (Json.String "waiting") -> `Waiting
              | _ -> `Ready
            in
            Ok (Event.QueuePackage { package; queue_type })
        | Some (Json.String "QueueStats") ->
            let ready =
              match List.assoc_opt "ready" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let waiting =
              match List.assoc_opt "waiting" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let busy =
              match List.assoc_opt "busy" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            Ok (Event.QueueStats { ready; waiting; busy })
        | Some (Json.String "DependencyMissing") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let missing =
              match List.assoc_opt "missing" fields with
              | Some (Json.Array arr) ->
                  List.filter_map
                    (function Json.String s -> Some s | _ -> None)
                    arr
              | _ -> []
            in
            Ok (Event.DependencyMissing { package; missing })
        | Some (Json.String "DependencySatisfied") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.DependencySatisfied { package })
        | Some (Json.String "CompilingInterface") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let file =
              match List.assoc_opt "file" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.CompilingInterface { package; file })
        | Some (Json.String "CompilingImplementation") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let file =
              match List.assoc_opt "file" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.CompilingImplementation { package; file })
        | Some (Json.String "LinkingLibrary") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let output =
              match List.assoc_opt "output" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.LinkingLibrary { package; output })
        | Some (Json.String "LinkingExecutable") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let output =
              match List.assoc_opt "output" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.LinkingExecutable { package; output })
        | Some (Json.String "ComputingHash") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.ComputingHash { package })
        | Some (Json.String "HashComputed") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let hash =
              match List.assoc_opt "hash" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.HashComputed { package; hash })
        | Some (Json.String "CopyingFile") ->
            let source =
              match List.assoc_opt "source" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let dest =
              match List.assoc_opt "dest" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.CopyingFile { source; dest })
        | Some (Json.String "WritingFile") ->
            let path =
              match List.assoc_opt "path" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.WritingFile { path })
        | Some (Json.String "CreatingDirectory") ->
            let path =
              match List.assoc_opt "path" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.CreatingDirectory { path })
        | Some (Json.String "RpcRequestReceived") ->
            let session_id =
              match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            let request_type =
              match List.assoc_opt "request_type" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.RpcRequestReceived { request_type; args = Json.Null })
        | Some (Json.String "RpcResponseSent") ->
            let session_id =
              match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            let success =
              match List.assoc_opt "success" fields with
              | Some (Json.Bool b) -> b
              | _ -> false
            in
            Ok
              (Event.RpcResponseSent
                 { result = (if success then Ok () else Error "failed") })
        | Some (Json.String "McpToolCall") ->
            let tool =
              match List.assoc_opt "tool" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let args =
              match List.assoc_opt "args" fields with
              | Some j -> j
              | _ -> Json.Null
            in
            Ok (Event.McpToolCall { tool; args })
        | Some (Json.String "ServerShutdown") -> Ok Event.ServerShutdown
        | Some (Json.String "WorkspaceEmpty") -> Ok Event.WorkspaceEmpty
        | _ -> Error (Json.String "Unknown log event type"))
    | _ -> Error (Json.String "Invalid log event JSON")

  let log_event_to_json = function
    | Event.BuildStarted { packages; total_modules; workers } ->
        Json.Object
          [
            ("type", Json.String "BuildStarted");
            ("packages", Json.Array (List.map (fun p -> Json.String p) packages));
            ("total_modules", Json.Int total_modules);
            ("workers", Json.Int workers);
          ]
    | Event.BuildComplete { duration_ms; results; succeeded; failed } ->
        Json.Object
          [
            ("type", Json.String "BuildComplete");
            ("duration_ms", Json.Int duration_ms);
            ( "results",
              Json.Array
                (List.map
                   (fun r ->
                     Json.Object
                       [
                         ("package", Json.String r.Event.package);
                         ("success", Json.Bool r.Event.success);
                         ("duration_ms", Json.Int r.Event.duration_ms);
                         ("modules_compiled", Json.Int r.Event.modules_compiled);
                         ("cache_hits", Json.Int r.Event.cache_hits);
                         ("cache_misses", Json.Int r.Event.cache_misses);
                         ( "errors",
                           Json.Array
                             (List.map
                                (fun e ->
                                  Json.Object
                                    [
                                      ("file", Json.String e.Event.file);
                                      ("line", Json.Int e.Event.line);
                                      ( "column",
                                        match e.Event.column with
                                        | Some c -> Json.Int c
                                        | None -> Json.Null );
                                      ("message", Json.String e.Event.message);
                                      ( "hint",
                                        match e.Event.hint with
                                        | Some h -> Json.String h
                                        | None -> Json.Null );
                                    ])
                                r.Event.errors) );
                       ])
                   results) );
            ( "succeeded",
              Json.Array (List.map (fun s -> Json.String s) succeeded) );
            ("failed", Json.Array (List.map (fun f -> Json.String f) failed));
          ]
    | Event.PackageStarted { package } ->
        Json.Object
          [
            ("type", Json.String "PackageStarted");
            ("package", Json.String package);
          ]
    | Event.PackageComplete result ->
        Json.Object
          [
            ("type", Json.String "PackageComplete");
            ("package", Json.String result.Event.package);
            ("success", Json.Bool result.Event.success);
            ("duration_ms", Json.Int result.Event.duration_ms);
            ("modules_compiled", Json.Int result.Event.modules_compiled);
            ("cache_hits", Json.Int result.Event.cache_hits);
            ("cache_misses", Json.Int result.Event.cache_misses);
            ( "errors",
              Json.Array
                (List.map
                   (fun e ->
                     Json.Object
                       [
                         ("file", Json.String e.Event.file);
                         ("line", Json.Int e.Event.line);
                         ( "column",
                           match e.Event.column with
                           | Some c -> Json.Int c
                           | None -> Json.Null );
                         ("message", Json.String e.Event.message);
                         ( "hint",
                           match e.Event.hint with
                           | Some h -> Json.String h
                           | None -> Json.Null );
                       ])
                   result.Event.errors) );
          ]
    | Event.CycleDetected { packages } ->
        Json.Object
          [
            ("type", Json.String "CycleDetected");
            ("packages", Json.Array (List.map (fun s -> Json.String s) packages));
          ]
    | Event.CompileError { package; file; line; column; message; hint } ->
        Json.Object
          [
            ("type", Json.String "CompileError");
            ("package", Json.String package);
            ("file", Json.String file);
            ("line", Json.Int line);
            ( "column",
              match column with Some c -> Json.Int c | None -> Json.Null );
            ("message", Json.String message);
            ( "hint",
              match hint with Some h -> Json.String h | None -> Json.Null );
          ]
    | Event.CacheHit { package; hash } ->
        Json.Object
          [
            ("type", Json.String "CacheHit");
            ("package", Json.String package);
            ("hash", Json.String hash);
          ]
    | Event.CacheMiss { package; hash } ->
        Json.Object
          [
            ("type", Json.String "CacheMiss");
            ("package", Json.String package);
            ("hash", Json.String hash);
          ]
    | Event.CacheStored { package; hash; artifacts } ->
        Json.Object
          [
            ("type", Json.String "CacheStored");
            ("package", Json.String package);
            ("hash", Json.String hash);
            ( "artifacts",
              Json.Array (List.map (fun a -> Json.String a) artifacts) );
          ]
    | Event.WorkerPoolStarted { workers } ->
        Json.Object
          [
            ("type", Json.String "WorkerPoolStarted");
            ("workers", Json.Int workers);
          ]
    | Event.WorkerStarted { worker_id } ->
        Json.Object
          [
            ("type", Json.String "WorkerStarted");
            ("worker_id", Json.String (Worker_id.to_string worker_id));
          ]
    | Event.WorkerAssigned { worker_id; package } ->
        Json.Object
          [
            ("type", Json.String "WorkerAssigned");
            ("worker_id", Json.String (Worker_id.to_string worker_id));
            ("package", Json.String package);
          ]
    | Event.WorkerIdle { worker_id } ->
        Json.Object
          [
            ("type", Json.String "WorkerIdle");
            ("worker_id", Json.String (Worker_id.to_string worker_id));
          ]
    | Event.ServerStarted { pid } ->
        Json.Object
          [ ("type", Json.String "ServerStarted"); ("pid", Json.String pid) ]
    | Event.ServerScanning { root } ->
        Json.Object
          [ ("type", Json.String "ServerScanning"); ("root", Json.String root) ]
    | Event.ServerRestarted { packages; toolchain } ->
        Json.Object
          [
            ("type", Json.String "ServerRestarted");
            ("packages", Json.Int packages);
            ("toolchain", Json.String toolchain);
          ]
    | Event.QueuePackage { package; queue_type } ->
        Json.Object
          [
            ("type", Json.String "QueuePackage");
            ("package", Json.String package);
            ( "queue_type",
              Json.String
                (match queue_type with
                | `Ready -> "ready"
                | `Waiting -> "waiting") );
          ]
    | Event.QueueStats { ready; waiting; busy } ->
        Json.Object
          [
            ("type", Json.String "QueueStats");
            ("ready", Json.Int ready);
            ("waiting", Json.Int waiting);
            ("busy", Json.Int busy);
          ]
    | Event.DependencyMissing { package; missing } ->
        Json.Object
          [
            ("type", Json.String "DependencyMissing");
            ("package", Json.String package);
            ("missing", Json.Array (List.map (fun m -> Json.String m) missing));
          ]
    | Event.DependencySatisfied { package } ->
        Json.Object
          [
            ("type", Json.String "DependencySatisfied");
            ("package", Json.String package);
          ]
    | Event.CompilingInterface { package; file } ->
        Json.Object
          [
            ("type", Json.String "CompilingInterface");
            ("package", Json.String package);
            ("file", Json.String file);
          ]
    | Event.CompilingImplementation { package; file } ->
        Json.Object
          [
            ("type", Json.String "CompilingImplementation");
            ("package", Json.String package);
            ("file", Json.String file);
          ]
    | Event.LinkingLibrary { package; output } ->
        Json.Object
          [
            ("type", Json.String "LinkingLibrary");
            ("package", Json.String package);
            ("output", Json.String output);
          ]
    | Event.LinkingExecutable { package; output } ->
        Json.Object
          [
            ("type", Json.String "LinkingExecutable");
            ("package", Json.String package);
            ("output", Json.String output);
          ]
    | Event.ComputingHash { package } ->
        Json.Object
          [
            ("type", Json.String "ComputingHash");
            ("package", Json.String package);
          ]
    | Event.HashComputed { package; hash } ->
        Json.Object
          [
            ("type", Json.String "HashComputed");
            ("package", Json.String package);
            ("hash", Json.String hash);
          ]
    | Event.CopyingFile { source; dest } ->
        Json.Object
          [
            ("type", Json.String "CopyingFile");
            ("source", Json.String source);
            ("dest", Json.String dest);
          ]
    | Event.WritingFile { path } ->
        Json.Object
          [ ("type", Json.String "WritingFile"); ("path", Json.String path) ]
    | Event.CreatingDirectory { path } ->
        Json.Object
          [
            ("type", Json.String "CreatingDirectory"); ("path", Json.String path);
          ]
    | Event.RpcRequestReceived { request_type; args } ->
        Json.Object
          [
            ("type", Json.String "RpcRequestReceived");
            ("request_type", Json.String request_type);
            ("args", args);
          ]
    | Event.RpcResponseSent { result } ->
        Json.Object
          [
            ("type", Json.String "RpcResponseSent");
            ( "success",
              Json.Bool (match result with Ok _ -> true | Error _ -> false) );
          ]
    | Event.McpToolCall { tool; args } ->
        Json.Object
          [
            ("type", Json.String "McpToolCall");
            ("tool", Json.String tool);
            ("args", args);
          ]
    | Event.ServerShutdown ->
        Json.Object [ ("type", Json.String "ServerShutdown") ]
    | Event.WorkspaceEmpty ->
        Json.Object [ ("type", Json.String "WorkspaceEmpty") ]
    | Event.WorkspaceScanning ->
        Json.Object [ ("type", Json.String "WorkspaceScanning") ]
    | Event.WorkspaceScanned { packages; duration_ms } ->
        Json.Object
          [
            ("type", Json.String "WorkspaceScanned");
            ("packages", Json.Int packages);
            ("duration_ms", Json.Int duration_ms);
          ]
    | Event.BuildGraphCreating ->
        Json.Object [ ("type", Json.String "BuildGraphCreating") ]
    | Event.BuildGraphCreated { nodes; duration_ms } ->
        Json.Object
          [
            ("type", Json.String "BuildGraphCreated");
            ("nodes", Json.Int nodes);
            ("duration_ms", Json.Int duration_ms);
          ]

  let request_to_params = function
    | Ping -> { Jsonrpc.method_ = method_ping; params = NoParams }
    | GetWorkspaceConfig ->
        { method_ = method_get_workspace_config; params = NoParams }
    | GetPackageInfo pkg ->
        {
          method_ = method_get_package_info;
          params = Jsonrpc.Named [ ("package", Json.String pkg) ];
        }
    | GetBuildGraph -> { method_ = method_get_build_graph; params = NoParams }
    | BuildPackage pkg ->
        { method_ = method_build_package; params = build_package_params pkg }
    | BuildAll -> { method_ = method_build_all; params = NoParams }
    | Shutdown -> { method_ = method_shutdown; params = NoParams }
    | Restart -> { method_ = method_restart; params = NoParams }

  let request_of_params method_ params =
    (* Parse params based on the method name *)
    match method_ with
    | "tusk.ping" -> Ok Ping
    | "tusk.buildAll" -> Ok BuildAll
    | "tusk.buildPackage" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match List.assoc_opt "package" fields with
            | Some (Json.String pkg) -> Ok (BuildPackage pkg)
            | _ -> Error (Json.String "Missing or invalid 'package' parameter"))
        | _ -> Error (Json.String "BuildPackage requires named parameters"))
    | "tusk.getWorkspaceConfig" -> Ok GetWorkspaceConfig
    | "tusk.getPackageInfo" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match List.assoc_opt "package" fields with
            | Some (Json.String pkg) -> Ok (GetPackageInfo pkg)
            | _ -> Error (Json.String "Missing or invalid 'package' parameter"))
        | _ -> Error (Json.String "GetPackageInfo requires named parameters"))
    | "tusk.getBuildGraph" -> Ok GetBuildGraph
    | "tusk.restart" -> Ok Restart
    | "tusk.shutdown" -> Ok Shutdown
    | _ -> Error (Json.String ("Unknown method: " ^ method_))

  let response_to_json = function
    | Pong -> Json.String "pong"
    | PackageInfo detail ->
        Json.Object
          [
            ( "package",
              Json.Object
                [
                  ("name", Json.String detail.package.name);
                  ("path", Json.String detail.package.path);
                  ( "dependencies",
                    Json.Array
                      (List.map
                         (fun d -> Json.String d)
                         detail.package.dependencies) );
                ] );
            ( "sources",
              Json.Array (List.map (fun s -> Json.String s) detail.sources) );
            ( "dependency_names",
              Json.Array
                (List.map (fun d -> Json.String d) detail.dependency_names) );
          ]
    | WorkspaceConfig config ->
        Json.Object
          [
            ("workspace_root", Json.String config.workspace_root);
            ("target_dir", Json.String config.target_dir);
            ("toolchain", Json.String config.toolchain);
            ("toolchain_path", Json.String config.toolchain_path);
            ( "packages",
              Json.Array
                (List.map
                   (fun (pkg : package_info) ->
                     Json.Object
                       [
                         ("name", Json.String pkg.name);
                         ("path", Json.String pkg.path);
                         ( "dependencies",
                           Json.Array
                             (List.map
                                (fun d -> Json.String d)
                                pkg.dependencies) );
                       ])
                   config.packages) );
            ("total_packages", Json.Int config.total_packages);
          ]
    | BuildGraph graph ->
        Json.Object
          [
            ( "nodes",
              Json.Array
                (List.map
                   (fun (node : build_node) ->
                     Json.Object
                       [
                         ("package_name", Json.String node.package_name);
                         ("src_dir", Json.String node.src_dir);
                         ("out_dir", Json.String node.out_dir);
                         ("status", Json.String node.status);
                         ( "deps",
                           Json.Array
                             (List.map (fun d -> Json.String d) node.deps) );
                       ])
                   graph.nodes) );
          ]
    | BuildStarted { session_id; started_at } ->
        let tm = Std.Datetime.localtime (Std.Datetime.to_float started_at) in
        let timestamp = Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec in
        Json.Object
          [
            ("type", Json.String "build_started");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("started_at", Json.String timestamp);
          ]
    | CycleDetected { session_id; detected_at; cycle_nodes } ->
        let tm = Std.Datetime.localtime (Std.Datetime.to_float detected_at) in
        let timestamp = Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec in
        Json.Object
          [
            ("type", Json.String "cycle_detected");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("detected_at", Json.String timestamp);
            ( "cycle_nodes",
              Json.Array (List.map (fun s -> Json.String s) cycle_nodes) );
          ]
    | BuildEvent { session_id; event } ->
        (* Use Event.to_json for the event *)
        Event.to_json event
    | BuildComplete { session_id; completed_at; stats } ->
        let tm = Std.Datetime.localtime (Std.Datetime.to_float completed_at) in
        let timestamp = Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec in
        Json.Object
          [
            ("type", Json.String "build_complete");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("completed_at", Json.String timestamp);
            ("duration_ms", Json.Int stats.duration_ms);
            ("packages_built", Json.Int stats.packages_built);
            ("packages_failed", Json.Int stats.packages_failed);
            ("total_modules", Json.Int stats.total_modules);
            ("cache_hits", Json.Int stats.cache_hits);
            ("cache_misses", Json.Int stats.cache_misses);
          ]
    | BuildFailed { session_id; failed_at; stats; error } ->
        let tm = Std.Datetime.localtime (Std.Datetime.to_float failed_at) in
        let timestamp = Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec in
        Json.Object
          [
            ("type", Json.String "build_failed");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("failed_at", Json.String timestamp);
            ("error", Json.String error);
            ("duration_ms", Json.Int stats.duration_ms);
            ("packages_built", Json.Int stats.packages_built);
            ("packages_failed", Json.Int stats.packages_failed);
            ("total_modules", Json.Int stats.total_modules);
            ("cache_hits", Json.Int stats.cache_hits);
            ("cache_misses", Json.Int stats.cache_misses);
          ]
    | ShutdownAck -> Json.Object [ ("type", Json.String "shutdown_ack") ]
    | RestartAck -> Json.Object [ ("type", Json.String "restart_ack") ]
    | Error msg -> Json.Object [ ("error", Json.String msg) ]

  let response_of_json json =
    (* This would parse JSON back to response, needed for client *)
    (* Debug: log what we're trying to parse *)
    (* Printf.eprintf "[TUSK PROTOCOL] Parsing response JSON: %s\n" (Json.to_string json);
    flush stderr; *)
    match json with
    | Json.String "pong" -> Ok Pong
    | Json.Object fields -> (
        match List.assoc_opt "type" fields with
        | Some (Json.String "build_started") ->
            let session_id =
              match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            let started_at = Std.Datetime.now () in (* Will be overridden by server timestamp *)
            Ok (BuildStarted { session_id; started_at })
        | Some (Json.String "build_event") ->
            let session_id =
              match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            (* Parse the full event - it should have timestamp, level, etc *)
            let event =
              (* The build_event response should contain the full Event.to_json output *)
              (* which includes timestamp, session_id, level, event (name), message, and data fields *)
              let timestamp = 
                match List.assoc_opt "timestamp" fields with
                | Some (Json.String ts) -> 
                    (* Parse HH:MM:SS format back to Datetime.t *)
                    (* For now, use current time as fallback *)
                    Std.Datetime.now ()
                | _ -> Std.Datetime.now ()
              in
              let level =
                match List.assoc_opt "level" fields with
                | Some (Json.String "error") -> Event.Error
                | Some (Json.String "warn") -> Event.Warn
                | Some (Json.String "info") -> Event.Info
                | Some (Json.String "debug") -> Event.Debug
                | Some (Json.String "trace") -> Event.Trace
                | _ -> Event.Info
              in
              let kind =
                match List.assoc_opt "data" fields with
                | Some event_json -> (
                    match event_kind_of_json (Json.Object [("type", Json.String "BuildStarted"); ("event_data", event_json)]) with
                    | Ok evt -> evt
                    | Error _ ->
                        Event.BuildStarted
                          { packages = []; total_modules = 0; workers = 0 })
                | None ->
                    Event.BuildStarted
                      { packages = []; total_modules = 0; workers = 0 }
              in
              { Event.timestamp; session_id; level; kind }
            in
            Ok (BuildEvent { session_id; event })
        | Some (Json.String "cycle_detected") ->
            let session_id =
              match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            let cycle_nodes =
              match List.assoc_opt "cycle_nodes" fields with
              | Some (Json.Array arr) ->
                  List.filter_map
                    (function Json.String s -> Some s | _ -> None)
                    arr
              | _ -> []
            in
            let detected_at = Std.Datetime.now () in (* Will be overridden by server timestamp *)
            Ok (CycleDetected { session_id; detected_at; cycle_nodes })
        | Some (Json.String "build_complete") ->
            let duration_ms =
              match List.assoc_opt "duration_ms" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let packages_built =
              match List.assoc_opt "packages_built" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let packages_failed =
              match List.assoc_opt "packages_failed" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let total_modules =
              match List.assoc_opt "total_modules" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let cache_hits =
              match List.assoc_opt "cache_hits" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let cache_misses =
              match List.assoc_opt "cache_misses" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let session_id =
              match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            let completed_at = Std.Datetime.now () in (* Will be overridden by server timestamp *)
            Ok
              (BuildComplete
                 {
                   session_id;
                   completed_at;
                   stats =
                     {
                       duration_ms;
                       packages_built;
                       packages_failed;
                       total_modules;
                       cache_hits;
                       cache_misses;
                     };
                 })
        | Some (Json.String "build_failed") ->
            let error =
              match List.assoc_opt "error" fields with
              | Some (Json.String s) -> s
              | _ -> "Unknown error"
            in
            let duration_ms =
              match List.assoc_opt "duration_ms" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let packages_built =
              match List.assoc_opt "packages_built" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let packages_failed =
              match List.assoc_opt "packages_failed" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let total_modules =
              match List.assoc_opt "total_modules" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let cache_hits =
              match List.assoc_opt "cache_hits" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let cache_misses =
              match List.assoc_opt "cache_misses" fields with
              | Some (Json.Int n) -> n
              | _ -> 0
            in
            let session_id =
              match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            let failed_at = Std.Datetime.now () in (* Will be overridden by server timestamp *)
            Ok
              (BuildFailed
                 {
                   session_id;
                   failed_at;
                   stats =
                     {
                       duration_ms;
                       packages_built;
                       packages_failed;
                       total_modules;
                       cache_hits;
                       cache_misses;
                     };
                   error;
                 })
        | Some (Json.String "shutdown_ack") -> Ok ShutdownAck
        | Some (Json.String "restart_ack") -> Ok RestartAck
        | _ -> (
            (* Try other response types *)
            (* First check if this is a PackageInfo response *)
            match List.assoc_opt "package" fields with
            | Some (Json.Object _) ->
                (* Parse PackageInfo response *)
                let package =
                  match List.assoc_opt "package" fields with
                  | Some (Json.Object pkg_fields) ->
                      let name =
                        match List.assoc_opt "name" pkg_fields with
                        | Some (Json.String s) -> s
                        | _ -> ""
                      in
                      let path =
                        match List.assoc_opt "path" pkg_fields with
                        | Some (Json.String s) -> s
                        | _ -> ""
                      in
                      let dependencies =
                        match List.assoc_opt "dependencies" pkg_fields with
                        | Some (Json.Array deps) ->
                            List.filter_map
                              (function Json.String s -> Some s | _ -> None)
                              deps
                        | _ -> []
                      in
                      { name; path; dependencies }
                  | _ -> { name = ""; path = ""; dependencies = [] }
                in
                let sources =
                  match List.assoc_opt "sources" fields with
                  | Some (Json.Array arr) ->
                      List.filter_map
                        (function Json.String s -> Some s | _ -> None)
                        arr
                  | _ -> []
                in
                let dependency_names =
                  match List.assoc_opt "dependency_names" fields with
                  | Some (Json.Array arr) ->
                      List.filter_map
                        (function Json.String s -> Some s | _ -> None)
                        arr
                  | _ -> []
                in
                Ok (PackageInfo { package; sources; dependency_names })
            | Some _ | None -> (
                (* Check if this is a WorkspaceConfig response *)
                match List.assoc_opt "workspace_root" fields with
                | Some (Json.String _) ->
                    (* Parse WorkspaceConfig response *)
                    let workspace_root =
                      match List.assoc_opt "workspace_root" fields with
                      | Some (Json.String s) -> s
                      | _ -> ""
                    in
                    let target_dir =
                      match List.assoc_opt "target_dir" fields with
                      | Some (Json.String s) -> s
                      | _ -> ""
                    in
                    let toolchain =
                      match List.assoc_opt "toolchain" fields with
                      | Some (Json.String s) -> s
                      | _ -> ""
                    in
                    let toolchain_path =
                      match List.assoc_opt "toolchain_path" fields with
                      | Some (Json.String s) -> s
                      | _ -> ""
                    in
                    let packages =
                      match List.assoc_opt "packages" fields with
                      | Some (Json.Array arr) ->
                          List.filter_map
                            (function
                              | Json.Object pkg_fields ->
                                  let name =
                                    match List.assoc_opt "name" pkg_fields with
                                    | Some (Json.String s) -> s
                                    | _ -> ""
                                  in
                                  let path =
                                    match List.assoc_opt "path" pkg_fields with
                                    | Some (Json.String s) -> s
                                    | _ -> ""
                                  in
                                  let dependencies =
                                    match
                                      List.assoc_opt "dependencies" pkg_fields
                                    with
                                    | Some (Json.Array deps) ->
                                        List.filter_map
                                          (function
                                            | Json.String s -> Some s
                                            | _ -> None)
                                          deps
                                    | _ -> []
                                  in
                                  Some { name; path; dependencies }
                              | _ -> None)
                            arr
                      | _ -> []
                    in
                    let total_packages =
                      match List.assoc_opt "total_packages" fields with
                      | Some (Json.Int n) -> n
                      | _ -> 0
                    in
                    Ok
                      (WorkspaceConfig
                         {
                           workspace_root;
                           target_dir;
                           toolchain;
                           toolchain_path;
                           packages;
                           total_packages;
                         })
                | _ -> (
                    match List.assoc_opt "nodes" fields with
                    | Some (Json.Array _) ->
                        let nodes =
                          match List.assoc_opt "nodes" fields with
                          | Some (Json.Array arr) ->
                              List.filter_map
                                (function
                                  | Json.Object node_fields ->
                                      let package_name =
                                        match
                                          List.assoc_opt "package_name"
                                            node_fields
                                        with
                                        | Some (Json.String s) -> s
                                        | _ -> ""
                                      in
                                      let src_dir =
                                        match
                                          List.assoc_opt "src_dir" node_fields
                                        with
                                        | Some (Json.String s) -> s
                                        | _ -> ""
                                      in
                                      let out_dir =
                                        match
                                          List.assoc_opt "out_dir" node_fields
                                        with
                                        | Some (Json.String s) -> s
                                        | _ -> ""
                                      in
                                      let status =
                                        match
                                          List.assoc_opt "status" node_fields
                                        with
                                        | Some (Json.String s) -> s
                                        | _ -> ""
                                      in
                                      let deps =
                                        match
                                          List.assoc_opt "deps" node_fields
                                        with
                                        | Some (Json.Array d) ->
                                            List.filter_map
                                              (function
                                                | Json.String s -> Some s
                                                | _ -> None)
                                              d
                                        | _ -> []
                                      in
                                      Some
                                        {
                                          package_name;
                                          src_dir;
                                          out_dir;
                                          status;
                                          deps;
                                        }
                                  | _ -> None)
                                arr
                          | _ -> []
                        in
                        Ok (BuildGraph { nodes })
                    | _ -> (
                        match List.assoc_opt "error" fields with
                        | Some (Json.String msg) -> Ok (Error msg)
                        | _ -> Error json)))))
    | _ -> Error json
end

(** Client module for Tusk RPC *)
module Client = struct
  open Miniriot

  type t = {
    client : (TuskProtocol.request, TuskProtocol.response) Jsonrpc.Client.t;
    transport : Std.Net.TcpClient.t;
  }

  (** Build request type *)
  type build_request = BuildPackage of string | BuildAll

  (** Streaming build event *)
  type streaming_event =
    | BuildStarted of Session_id.t
    | BuildEvent of Event.t
    | BuildFinished of (unit, string) result

  (** Create a new Tusk RPC client *)
  let create ~host ~port =
    (* Create TCP transport using Std.Net.TcpClient *)
    match Std.Net.TcpClient.connect ~host ~port with
    | Ok transport ->
        let client =
          Jsonrpc.Client.create
            ~transport:(module Std.Net.TcpClient)
            ~protocol:(module TuskProtocol)
            transport
        in
        Ok { client; transport }
    | Error e ->
        let error_msg =
          match e with
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
      Jsonrpc.Client.call t.client ~method_:method_ping ~params:Jsonrpc.NoParams
        ()
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
      Jsonrpc.Client.call t.client ~method_:method_get_workspace_config
        ~params:Jsonrpc.NoParams ()
    with
    | Ok (TuskProtocol.WorkspaceConfig config) -> Ok config
    | Ok _ -> Error "Invalid workspace config response"
    | Error e ->
        Error
          (Printf.sprintf "Error %d: %s"
             (Jsonrpc.error_code_to_int e.code)
             e.message)

  (** Get package information *)
  let get_package_info t package_name =
    match
      Jsonrpc.Client.call t.client ~method_:method_get_package_info
        ~params:(Jsonrpc.Named [ ("package", Json.String package_name) ])
        ()
    with
    | Ok (TuskProtocol.PackageInfo detail) -> Ok detail
    | Ok _ -> Error "Invalid package info response"
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
    | Ok (TuskProtocol.BuildGraph graph) -> Ok graph
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
      | BuildPackage pkg -> TuskProtocol.BuildPackage pkg
      | BuildAll -> TuskProtocol.BuildAll
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
            | Ok (TuskProtocol.BuildStarted { session_id; started_at = _ }) ->
                (* Got BuildStarted *)
                callback (BuildStarted session_id);

                (* Now receive streaming events until build completes *)
                let rec receive_events () =
                  match Jsonrpc.Client.receive_response t.client with
                  | Ok
                      {
                        result =
                          Ok
                            (TuskProtocol.BuildEvent
                               { session_id = _; event });
                        _;
                      } ->
                      callback (BuildEvent event);
                      receive_events ()
                  | Ok
                      {
                        result =
                          Ok
                            (TuskProtocol.CycleDetected
                               { session_id; cycle_nodes });
                        _;
                      } ->
                      (* Report cycle detected as a log event *)
                      callback
                        (BuildEvent
                           (Event.create ~session_id ~level:Error
                              (Event.CycleDetected { packages = cycle_nodes })));
                      receive_events ()
                  | Ok { result = Ok (TuskProtocol.BuildComplete _); _ } ->
                      Ok (BuildFinished (Ok ()))
                  | Ok
                      {
                        result =
                          Ok (TuskProtocol.BuildFailed { session_id; error; _ });
                        _;
                      } ->
                      Ok (BuildFinished (Error error))
                  | Ok { result = Ok (TuskProtocol.Error msg); _ } ->
                      (* Got a general error response *)
                      Error (Printf.sprintf "Server error: %s" msg)
                  | Ok { result = Error err; _ } ->
                      Ok (BuildFinished (Error err.message))
                  | Error e ->
                      Error (Printf.sprintf "Failed to receive event: %s" e)
                  | Ok resp ->
                      (* Debug: print what response type we got *)
                      let resp_type =
                        match resp.result with
                        | Ok TuskProtocol.Pong -> "Pong"
                        | Ok (TuskProtocol.BuildGraph _) -> "BuildGraph"
                        | Ok (TuskProtocol.WorkspaceConfig _) ->
                            "WorkspaceConfig"
                        | Ok (TuskProtocol.PackageInfo _) -> "PackageInfo"
                        | Ok (TuskProtocol.BuildStarted _) -> "BuildStarted"
                        | Ok (TuskProtocol.BuildEvent _) -> "BuildEvent"
                        | Ok (TuskProtocol.CycleDetected _) -> "CycleDetected"
                        | Ok (TuskProtocol.BuildComplete _) -> "BuildComplete"
                        | Ok (TuskProtocol.BuildFailed _) -> "BuildFailed"
                        | Ok TuskProtocol.ShutdownAck -> "ShutdownAck"
                        | Ok TuskProtocol.RestartAck -> "RestartAck"
                        | Ok (TuskProtocol.Error _) -> "Error"
                        | Error e -> Printf.sprintf "JsonRpcError(%s)" e.message
                      in
                      Printf.eprintf
                        "[CLIENT] Unexpected response in receive_events: %s\n"
                        resp_type;
                      flush stderr;
                      Error "Unexpected response type"
                in
                receive_events ()
            | Ok (TuskProtocol.BuildComplete { session_id; completed_at = _; stats = _ }) ->
                (* Direct success (no build needed) *)
                Ok (BuildFinished (Ok ()))
            | Ok (TuskProtocol.BuildFailed { session_id; error; _ }) ->
                (* Direct error *)
                Ok (BuildFinished (Error error))
            | Ok (TuskProtocol.Error msg) ->
                (* Other error *)
                Ok (BuildFinished (Error msg))
            | Error err ->
                Error
                  (Printf.sprintf "Build request failed: %s" err.Jsonrpc.message)
            | Ok resp ->
                (* Log unexpected response for debugging *)
                Error "Unexpected response type"))

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
        ~params:(build_package_params package)
        ()
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
    (* Convert to tusk_protocol message type *)
    let server_request =
      match request with
      | TuskProtocol.BuildPackage pkg ->
          Tusk_protocol.Build
            {
              client_pid = self ();
              target = Tusk_protocol.Package pkg;
              session_id = None;
            }
      | TuskProtocol.BuildAll ->
          Tusk_protocol.Build
            {
              client_pid = self ();
              target = Tusk_protocol.All;
              session_id = None;
            }
      | _ -> failwith "Invalid build request"
    in
    send ctx.server_pid (Tusk_protocol.ServerRequest server_request);

    (* Wait for response *)
    let selector = function
      | Tusk_protocol.ServerResponse response -> `select response
      | _ -> `skip
    in
    match receive ~selector () with
    | Tusk_protocol.BuildStarted { session_id; started_at } ->
        (* Register ourselves with the logger to receive events *)
        Log.add_rpc_handler ~session_id ~client:(self ());

        (* Send BuildStarted to client *)
        reply (TuskProtocol.BuildStarted { session_id; started_at = Std.Datetime.now () });

        (* Now handle log events, CycleDetected and BuildCompleted *)
        let rec event_loop () =
          let selector = function
            | Log.Event event when event.Event.session_id = session_id ->
                `select (`log_event event)
            | Tusk_protocol.ServerResponse
                (Tusk_protocol.CycleDetected
                   { session_id = sid; cycle_nodes; detected_at })
              when sid = session_id ->
                `select (`cycle_detected (cycle_nodes, detected_at))
            | Tusk_protocol.ServerResponse
                (Tusk_protocol.BuildCompleted { session_id = sid; completed_At })
              when sid = session_id ->
                `select (`build_complete completed_At)
            | _ -> `skip
          in
          match receive ~selector () with
          | `log_event event ->
              (* Forward log event to client *)
              reply (TuskProtocol.BuildEvent { session_id; event });
              event_loop ()
          | `cycle_detected (cycle_nodes, detected_at) ->
              (* Forward cycle detected event to client *)
              reply
                (TuskProtocol.CycleDetected
                   { session_id; detected_at; cycle_nodes });
              event_loop ()
          | `build_complete completed_at ->
              (* Build is done, send final response *)
              reply
                (TuskProtocol.BuildComplete
                   {
                     session_id;
                     completed_at;
                     stats =
                       {
                         duration_ms = 0;
                         packages_built = 0;
                         packages_failed = 0;
                         total_modules = 0;
                         cache_hits = 0;
                         cache_misses = 0;
                       };
                   })
        in
        event_loop ()
    | _ -> reply (TuskProtocol.Error "Unexpected response")

  let handle_ping ctx reply request =
    (* Convert to tusk_protocol message type and send *)
    Printf.eprintf "[HANDLER] Sending Ping to server %s from %s\n"
      (Pid.to_string ctx.server_pid)
      (Pid.to_string (self ()));
    flush stderr;
    send ctx.server_pid
      (Tusk_protocol.ServerRequest (Tusk_protocol.Ping { client_pid = self () }));
    Printf.eprintf "[HANDLER] Waiting for response...\n";
    flush stderr;
    (* Wait for response *)
    let selector = function
      | Tusk_protocol.ServerResponse response -> `select response
      | _ -> `skip
    in
    match receive ~selector () with
    | Tusk_protocol.Pong ->
        Printf.eprintf "[HANDLER] Got Pong response\n";
        flush stderr;
        reply TuskProtocol.Pong
    | _ ->
        Printf.eprintf "[HANDLER] Got unexpected response\n";
        flush stderr;
        reply (TuskProtocol.Error "Unexpected response")

  let handle_shutdown ctx reply request =
    (* For now, just reply with success *)
    reply TuskProtocol.ShutdownAck

  let handle_workspace_config ctx reply request =
    (* Send request to the actual tusk server *)
    send ctx.server_pid
      (Tusk_protocol.ServerRequest
         (Tusk_protocol.GetWorkspaceConfig { client_pid = self () }));

    (* Wait for response *)
    let selector = function
      | Tusk_protocol.ServerResponse
          (Tusk_protocol.WorkspaceConfig { workspace; toolchain }) ->
          `select (workspace, toolchain)
      | _ -> `skip
    in
    match receive ~selector () with
    | workspace, toolchain ->
        (* Convert to JSON-RPC response with full details *)
        let packages =
          List.map
            (fun (pkg : Workspace.package) ->
              {
                TuskProtocol.name = pkg.name;
                path = Std.Path.to_string pkg.path;
                dependencies =
                  List.map
                    (fun (dep : Workspace.dependency) -> dep.name)
                    pkg.dependencies;
              })
            workspace.packages
        in
        reply
          (TuskProtocol.WorkspaceConfig
             {
               workspace_root = Std.Path.to_string workspace.root;
               target_dir = Std.Path.to_string workspace.target_dir_root;
               toolchain = Toolchains.get_version toolchain;
               toolchain_path = Toolchains.get_toolchain_path toolchain;
               packages;
               total_packages = List.length workspace.packages;
             })
    | exception _ -> reply (TuskProtocol.Error "Failed to get workspace config")

  let handle_package_info ctx reply package_name =
    (* Send request to the actual tusk server *)
    send ctx.server_pid
      (Tusk_protocol.ServerRequest
         (Tusk_protocol.GetPackageInfo { client_pid = self (); package_name }));

    (* Wait for response *)
    let selector = function
      | Tusk_protocol.ServerResponse
          (Tusk_protocol.PackageInfo { package; sources; dependencies }) ->
          `select (package, sources, dependencies)
      | _ -> `skip
    in
    match receive ~selector () with
    | package, sources, dependencies ->
        (* Convert to JSON-RPC response *)
        let package_info =
          {
            TuskProtocol.name = package.Workspace.name;
            path = Std.Path.to_string package.path;
            dependencies =
              List.map
                (fun (dep : Workspace.dependency) -> dep.name)
                package.dependencies;
          }
        in
        let source_strings = List.map Std.Path.to_string sources in
        let dep_names =
          List.map (fun (node : Build_node.t) -> node.package.name) dependencies
        in
        reply
          (TuskProtocol.PackageInfo
             {
               package = package_info;
               sources = source_strings;
               dependency_names = dep_names;
             })
    | exception _ -> reply (TuskProtocol.Error "Failed to get package info")

  let handle_build_graph ctx reply request =
    (* Send request to the actual tusk server *)
    send ctx.server_pid
      (Tusk_protocol.ServerRequest
         (Tusk_protocol.GetBuildGraph { client_pid = self () }));

    (* Wait for response *)
    let selector = function
      | Tusk_protocol.ServerResponse (Tusk_protocol.BuildGraph { nodes }) ->
          `select nodes
      | _ -> `skip
    in
    match receive ~selector () with
    | nodes ->
        (* Convert Build_node.t list to simplified JSON-RPC format *)
        let json_nodes =
          List.map
            (fun (node : Build_node.t) ->
              {
                TuskProtocol.package_name = node.package.name;
                src_dir = Std.Path.to_string node.package.path;
                out_dir = Std.Path.to_string node.package.path;
                (* TODO: get actual out dir *)
                status =
                  (match node.spec with
                  | Build_node.Unplanned -> "unplanned"
                  | Build_node.Planned _ -> "planned");
                deps = List.map Node_id.to_string node.deps;
              })
            nodes
        in
        reply (TuskProtocol.BuildGraph { nodes = json_nodes })
    | exception _ -> reply (TuskProtocol.Error "Failed to get build graph")

  let handle_restart ctx reply request =
    (* For now, just reply with success *)
    reply TuskProtocol.RestartAck

  (** Create a JSON-RPC server handler for the tusk server *)
  let create server_pid =
    let ctx = { server_pid } in
    (* Create handlers that match on the request type *)
    let methods =
      Jsonrpc.Server.
        [
          {
            method_ = method_ping;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.Ping -> handle_ping ctx reply request
                | _ -> ());
          };
          {
            method_ = method_build_package;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.BuildPackage _ -> handle_build ctx reply request
                | _ -> ());
          };
          {
            method_ = method_build_all;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.BuildAll -> handle_build ctx reply request
                | _ -> ());
          };
          {
            method_ = method_get_workspace_config;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.GetWorkspaceConfig ->
                    handle_workspace_config ctx reply request
                | _ -> ());
          };
          {
            method_ = method_get_package_info;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.GetPackageInfo pkg ->
                    handle_package_info ctx reply pkg
                | _ -> ());
          };
          {
            method_ = method_get_build_graph;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.GetBuildGraph ->
                    handle_build_graph ctx reply request
                | _ -> ());
          };
          {
            method_ = method_restart;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.Restart -> handle_restart ctx reply request
                | _ -> ());
          };
          {
            method_ = method_shutdown;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.Shutdown -> handle_shutdown ctx reply request
                | _ -> ());
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
