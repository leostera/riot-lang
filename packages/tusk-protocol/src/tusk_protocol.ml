(** Tusk JSON-RPC Protocol - JSON-RPC 2.0 compliant *)

open Std
open Std.Data
open Tusk_model

(** Method names *)
let method_ping = "tusk.ping"

let method_get_package_graph = "tusk.getPackageGraph"
let method_get_workspace_config = "tusk.getWorkspaceConfig"
let method_get_package_info = "tusk.getPackageInfo"
let method_build_package = "tusk.buildPackage"
let method_build_all = "tusk.buildAll"
let method_restart = "tusk.restart"
let method_shutdown = "tusk.shutdown"
let method_build_event = "tusk.buildEvent"
let method_format_file = "tusk.formatFile"
let method_format_code = "tusk.formatCode"
let method_format_all = "tusk.formatAll"
let method_new_package = "tusk.newPackage"
let method_find_executable = "tusk.findExecutable"
let method_find_artifact = "tusk.findArtifact"
let method_get_symbol = "tusk.getSymbol"

(** Helper to create method-specific parameters *)
let build_package_params package =
  Jsonrpc.Named [ ("package", Json.String package) ]

(** TuskProtocol implementation for JSON-RPC *)
module WireProtocol = struct
  (** WireProtocol - External RPC Wire Format

      This module defines the JSON-RPC wire protocol for external clients (CLI,
      MCP). It uses simple, JSON-serializable types only (strings, ints,
      records).

      The JSONRPC handlers convert between WireProtocol and TuskProtocol:
      - Incoming requests: WireProtocol → TuskProtocol (adds client_pid)
      - Outgoing responses: TuskProtocol → WireProtocol (converts rich types to
        strings)

      For the internal server protocol, see TuskProtocol in
      core/tusk_protocol.ml. *)

  (* Define request/response types for JSON-RPC communication *)
  type build_node = {
    package_name : string;
    src_dir : string;
    out_dir : string;
    status : string;
    deps : string list;
  }

  type package_graph_response = { nodes : build_node list }

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
    | GetPackageGraph
    | GetWorkspaceConfig
    | GetPackageInfo of string
    | BuildPackage of string
    | BuildAll
    | Restart
    | Shutdown
    | FindExecutable of string
    | FindArtifact of { package : string; kind : string; name : string }
    | GetSymbol of { kind : string option; name : string }
    | FormatFile of { file_path : string; check_only : bool }
    | FormatCode of { code : string; file_path : string option }
    | FormatAll of { mode : [ `check | `write ] }
    | NewPackage of { path : string; name : string; is_library : bool }

  type build_stats = {
    duration_ms : int;
    packages_built : int;
    packages_failed : int;
    total_modules : int;
    cache_hits : int;
    cache_misses : int;
  }

  type package_error = Tusk_executor.Package_builder.package_error =
    | PlanningFailed of Tusk_planner.Planning_error.t
    | ExecutionFailed of { message : string }
    | ActionExecutionFailed of { message : string }
    | ActionOutputsNotCreated of { missing : Path.t list }
    | ActionDependenciesFailed of { failed : Graph.SimpleGraph.Node_id.t list }

  type build_status = Tusk_executor.Package_builder.build_status =
    | Cached of Tusk_store.Artifact.t
    | Built of Tusk_store.Artifact.t
    | Failed of package_error

  type build_result = Tusk_executor.Package_builder.build_result = {
    package : Package.t;
    status : build_status;
    duration : Std.Time.Duration.t;
  }

  let package_error_to_json =
    Tusk_executor.Package_builder.package_error_to_json

  let build_status_to_json = Tusk_executor.Package_builder.build_status_to_json
  let build_result_to_json = Tusk_executor.Package_builder.build_result_to_json

  type response =
    | Pong
    | PackageGraph of package_graph_response
    | WorkspaceConfig of workspace_config
    | PackageInfo of package_detail
    | BuildStarted of { session_id : Session_id.t; started_at : Std.Datetime.t }
    | BuildEvent of { session_id : Session_id.t; event : Std.Telemetry.event }
    | CycleDetected of {
        session_id : Session_id.t;
        detected_at : Std.Datetime.t;
        cycle_nodes : string list;
      }
    | BuildComplete of {
        session_id : Session_id.t;
        completed_at : Std.Datetime.t;
        stats : build_stats;
        results : build_result list;
      }
    | BuildFailed of {
        session_id : Session_id.t;
        failed_at : Std.Datetime.t;
        stats : build_stats;
        built : build_result list;
        errors : build_result list;
      }
    | PlanningFailed of {
        session_id : Session_id.t;
        failed_at : Std.Datetime.t;
        reason : string;
      }
    | ExecutableFound of { package : string; binary : string }
    | ExecutableNotFound
    | ArtifactFound of { path : string }
    | ArtifactNotFound of { error : string }
    | SymbolFound of { 
        symbol_kind : string;
        symbol_name : string;
        source_path : string;
        source_sha256 : string;
        package_name : string;
        package_path : string;
      }
    | SymbolNotFound
    | ShutdownAck
    | RestartAck
    | FormatResult of { formatted_code : string; changed : bool }
    | FormatError of { error : string }
    | FormatAllResult of {
        files_formatted : int;
        files_failed : int;
        errors : (string * string) list;
      }
    | PackageCreated of { path : string; name : string }
    | PackageCreationError of { error : string }
    | PackageNotFound of {
        session_id : Session_id.t;
        package_name : string;
        available_packages : string list;
      }
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
                                        let span =
                                          match List.assoc_opt "span" e with
                                          | Some
                                              (Json.Array
                                                 [
                                                   Json.Int start; Json.Int end_;
                                                 ]) ->
                                              (start, end_)
                                          | _ ->
                                              let col =
                                                match column with
                                                | Some c -> c
                                                | None -> 0
                                              in
                                              (col, col)
                                        in
                                        let raw =
                                          match List.assoc_opt "raw" e with
                                          | Some (Json.String r) -> r
                                          | _ -> message
                                        in
                                        let hint_str =
                                          match hint with
                                          | Some h -> h
                                          | None -> ""
                                        in
                                        Some
                                          {
                                            Event.file;
                                            line;
                                            span;
                                            hint = hint_str;
                                            kind = Event.OtherError { message };
                                            raw;
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
                          let span =
                            match List.assoc_opt "span" e with
                            | Some
                                (Json.Array [ Json.Int start; Json.Int end_ ])
                              ->
                                (start, end_)
                            | _ ->
                                let col =
                                  match column with Some c -> c | None -> 0
                                in
                                (col, col)
                          in
                          let raw =
                            match List.assoc_opt "raw" e with
                            | Some (Json.String r) -> r
                            | _ -> message
                          in
                          let hint_str =
                            match hint with Some h -> h | None -> ""
                          in
                          Some
                            {
                              Event.file;
                              line;
                              span;
                              hint = hint_str;
                              kind = Event.OtherError { message };
                              raw;
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
            (* column field is optional and not currently used *)
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
            let span =
              match List.assoc_opt "span" fields with
              | Some (Json.Array [ Json.Int start; Json.Int end_ ]) ->
                  (start, end_)
              | _ -> (0, 0)
              (* default span *)
            in
            let raw =
              match List.assoc_opt "raw" fields with
              | Some (Json.String r) -> r
              | _ -> message
            in
            let hint_str = match hint with Some h -> h | None -> "" in
            (* Try to parse error kind from message *)
            let error_kind =
              if message = "Syntax error" then Event.SyntaxError
              else if String.starts_with ~prefix:"Unbound value " message then
                Event.UnboundValue
                  { name = String.sub message 14 (String.length message - 14) }
              else if String.starts_with ~prefix:"Unbound module " message then
                Event.UnboundModule
                  { name = String.sub message 15 (String.length message - 15) }
              else if String.starts_with ~prefix:"Cannot find file " message
              then
                Event.FileNotFound
                  {
                    filename = String.sub message 17 (String.length message - 17);
                  }
              else Event.OtherError { message }
            in
            Ok
              (Event.CompileError
                 {
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
                 })
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
            (* session_id field not currently used *)
            let request_type =
              match List.assoc_opt "request_type" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (Event.RpcRequestReceived { request_type; args = Json.Null })
        | Some (Json.String "RpcResponseSent") ->
            (* session_id field not currently used *)
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
                                  let col_start, col_end = e.Event.span in
                                  let error_message =
                                    match e.Event.kind with
                                    | Event.SyntaxError -> "Syntax error"
                                    | Event.TypeError { description } ->
                                        description
                                     | Event.UnboundValue { name } ->
                                         "Unbound value " ^ name
                                     | Event.UnboundModule { name } ->
                                         "Unbound module " ^ name
                                     | Event.FileNotFound { filename } ->
                                         "Cannot find file " ^ filename
                                    | Event.OtherError { message } -> message
                                  in
                                  Json.Object
                                    [
                                      ("file", Json.String e.Event.file);
                                      ("line", Json.Int e.Event.line);
                                      ( "span",
                                        Json.Array
                                          [
                                            Json.Int col_start; Json.Int col_end;
                                          ] );
                                      ("message", Json.String error_message);
                                      ( "hint",
                                        Json.String
                                          (Event.strip_ansi_codes e.Event.hint)
                                      );
                                      ( "raw",
                                        Json.String
                                          (Event.strip_ansi_codes e.Event.raw)
                                      );
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
                     let col_start, col_end = e.Event.span in
                     let error_message =
                       match e.Event.kind with
                       | Event.SyntaxError -> "Syntax error"
                       | Event.TypeError { description } -> description
                        | Event.UnboundValue { name } ->
                            "Unbound value " ^ name
                        | Event.UnboundModule { name } ->
                            "Unbound module " ^ name
                        | Event.FileNotFound { filename } ->
                            "Cannot find file " ^ filename
                       | Event.OtherError { message } -> message
                     in
                     Json.Object
                       [
                         ("file", Json.String e.Event.file);
                         ("line", Json.Int e.Event.line);
                         ( "span",
                           Json.Array [ Json.Int col_start; Json.Int col_end ]
                         );
                         ("message", Json.String error_message);
                         ( "hint",
                           Json.String (Event.strip_ansi_codes e.Event.hint) );
                         ( "raw",
                           Json.String (Event.strip_ansi_codes e.Event.raw) );
                       ])
                   result.Event.errors) );
          ]
    | Event.CycleDetected { packages } ->
        Json.Object
          [
            ("type", Json.String "CycleDetected");
            ("packages", Json.Array (List.map (fun s -> Json.String s) packages));
          ]
    | Event.CompileError { package; error } ->
        let col_start, col_end = error.span in
        let error_message =
          match error.kind with
          | Event.SyntaxError -> "Syntax error"
          | Event.TypeError { description } -> description
           | Event.UnboundValue { name } -> "Unbound value " ^ name
           | Event.UnboundModule { name } -> "Unbound module " ^ name
           | Event.FileNotFound { filename } ->
               "Cannot find file " ^ filename
          | Event.OtherError { message } -> message
        in
        Json.Object
          [
            ("type", Json.String "CompileError");
            ("package", Json.String package);
            ("file", Json.String error.file);
            ("line", Json.Int error.line);
            ("span", Json.Array [ Json.Int col_start; Json.Int col_end ]);
            ("message", Json.String error_message);
            ("hint", Json.String (Event.strip_ansi_codes error.hint));
            ("raw", Json.String (Event.strip_ansi_codes error.raw));
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
    | Event.StoreCreating ->
        Json.Object [ ("type", Json.String "StoreCreating") ]
    | Event.StoreCreated { duration_ms } ->
        Json.Object
          [
            ("type", Json.String "StoreCreated");
            ("duration_ms", Json.Int duration_ms);
          ]
    | Event.PackageSkipped { package } ->
        Json.Object
          [
            ("type", Json.String "PackageSkipped");
            ("package", Json.String package);
          ]
    | Event.WorkerPoolCreating _ ->
        Json.Object [ ("type", Json.String "WorkerPoolCreating") ]
    | Event.WorkerPoolCreated _ ->
        Json.Object [ ("type", Json.String "WorkerPoolCreated") ]

  let request_to_params = function
    | Ping -> { Jsonrpc.method_ = method_ping; params = NoParams }
    | GetWorkspaceConfig ->
        { method_ = method_get_workspace_config; params = NoParams }
    | GetPackageInfo pkg ->
        {
          method_ = method_get_package_info;
          params = Jsonrpc.Named [ ("package", Json.String pkg) ];
        }
    | GetPackageGraph -> { method_ = method_get_package_graph; params = NoParams }
    | BuildPackage pkg ->
        { method_ = method_build_package; params = build_package_params pkg }
    | BuildAll -> { method_ = method_build_all; params = NoParams }
    | Shutdown -> { method_ = method_shutdown; params = NoParams }
    | Restart -> { method_ = method_restart; params = NoParams }
    | FindExecutable name ->
        {
          method_ = method_find_executable;
          params = Jsonrpc.Named [ ("name", Json.String name) ];
        }
    | FindArtifact { package; kind; name } ->
        {
          method_ = method_find_artifact;
          params =
            Jsonrpc.Named
              [
                ("package", Json.String package);
                ("kind", Json.String kind);
                ("name", Json.String name);
              ];
        }
    | GetSymbol { kind; name } ->
        {
          method_ = method_get_symbol;
          params =
            Jsonrpc.Named
              [
                ("kind", match kind with Some k -> Json.String k | None -> Json.Null);
                ("name", Json.String name);
              ];
        }
    | FormatFile { file_path; check_only } ->
        {
          method_ = method_format_file;
          params =
            Named
              [
                ("file_path", Json.String file_path);
                ("check_only", Json.Bool check_only);
              ];
        }
    | FormatCode { code; file_path } ->
        {
          method_ = method_format_code;
          params =
            Named
              [
                ("code", Json.String code);
                ( "file_path",
                  match file_path with
                  | Some fp -> Json.String fp
                  | None -> Json.Null );
              ];
        }
    | FormatAll { mode } ->
        {
          method_ = method_format_all;
          params =
            Named
              [
                ( "mode",
                  Json.String
                    (match mode with `check -> "check" | `write -> "write") );
              ];
        }
    | NewPackage { path; name; is_library } ->
        {
          method_ = method_new_package;
          params =
            Named
              [
                ("path", Json.String path);
                ("name", Json.String name);
                ("is_library", Json.Bool is_library);
              ];
        }

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
    | "tusk.getPackageGraph" -> Ok GetPackageGraph
    | "tusk.restart" -> Ok Restart
    | "tusk.shutdown" -> Ok Shutdown
    | "tusk.findExecutable" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match List.assoc_opt "name" fields with
            | Some (Json.String name) -> Ok (FindExecutable name)
            | _ -> Error (Json.String "Missing or invalid 'name' parameter"))
        | _ -> Error (Json.String "findExecutable requires named parameters"))
    | "tusk.findArtifact" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match
              ( List.assoc_opt "package" fields,
                List.assoc_opt "kind" fields,
                List.assoc_opt "name" fields )
            with
            | ( Some (Json.String package),
                Some (Json.String kind),
                Some (Json.String name) ) ->
                Ok (FindArtifact { package; kind; name })
            | _ ->
                Error
                  (Json.String "Missing or invalid parameters for findArtifact")
            )
        | _ -> Error (Json.String "findArtifact requires named parameters"))
    | "tusk.formatFile" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match
              ( List.assoc_opt "file_path" fields,
                List.assoc_opt "check_only" fields )
            with
            | Some (Json.String file_path), Some (Json.Bool check_only) ->
                Ok (FormatFile { file_path; check_only })
            | Some (Json.String file_path), None ->
                Ok (FormatFile { file_path; check_only = false })
            | _ ->
                Error (Json.String "Missing or invalid 'file_path' parameter"))
        | _ -> Error (Json.String "Invalid parameters for formatFile"))
    | "tusk.formatCode" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match List.assoc_opt "code" fields with
            | Some (Json.String code) ->
                let file_path =
                  match List.assoc_opt "file_path" fields with
                  | Some (Json.String fp) -> Some fp
                  | _ -> None
                in
                Ok (FormatCode { code; file_path })
            | _ -> Error (Json.String "Missing or invalid 'code' parameter"))
        | _ -> Error (Json.String "Invalid parameters for formatCode"))
    | "tusk.formatAll" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match List.assoc_opt "mode" fields with
            | Some (Json.String "check") -> Ok (FormatAll { mode = `check })
            | Some (Json.String "write") -> Ok (FormatAll { mode = `write })
            | Some (Json.String _) ->
                Error
                  (Json.String
                     "Invalid mode for formatAll (must be 'check' or 'write')")
            | None -> Ok (FormatAll { mode = `write }) (* default to write *)
            | _ -> Error (Json.String "Invalid mode parameter for formatAll"))
        | _ -> Error (Json.String "Invalid parameters for formatAll"))
    | "tusk.newPackage" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match
              ( List.assoc_opt "path" fields,
                List.assoc_opt "name" fields,
                List.assoc_opt "is_library" fields )
            with
            | ( Some (Json.String path),
                Some (Json.String name),
                Some (Json.Bool is_library) ) ->
                Ok (NewPackage { path; name; is_library })
            | Some (Json.String path), Some (Json.String name), None ->
                Ok (NewPackage { path; name; is_library = true })
            | _ ->
                Error
                  (Json.String "Missing or invalid parameters for newPackage"))
        | _ -> Error (Json.String "Invalid parameters for newPackage"))
    | "tusk.getSymbol" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match
              ( List.assoc_opt "kind" fields,
                List.assoc_opt "name" fields )
            with
            | ( Some (Json.String kind),
                Some (Json.String name) ) ->
                Ok (GetSymbol { kind = Some kind; name })
            | Some Json.Null, Some (Json.String name) ->
                Ok (GetSymbol { kind = None; name })
            | None, Some (Json.String name) ->
                Ok (GetSymbol { kind = None; name })
            | _ ->
                Error
                  (Json.String "Missing or invalid parameters for getSymbol")
            )
        | _ -> Error (Json.String "Invalid parameters for getSymbol"))
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
    | PackageGraph graph ->
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
        let timestamp = Std.Datetime.to_iso8601 started_at in
        Json.Object
          [
            ("type", Json.String "build_started");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("started_at", Json.String timestamp);
          ]
    | ExecutableFound { package; binary } ->
        Json.Object
          [
            ("type", Json.String "found_executable");
            ("package", Json.String package);
            ("binary", Json.String binary);
          ]
    | ExecutableNotFound ->
        Json.Object [ ("type", Json.String "executable_not_found") ]
    | ArtifactFound { path } ->
        Json.Object
          [ ("type", Json.String "artifact_found"); ("path", Json.String path) ]
    | ArtifactNotFound { error } ->
        Json.Object
          [
            ("type", Json.String "artifact_not_found");
            ("error", Json.String error);
          ]
    | SymbolFound { symbol_kind; symbol_name; source_path; source_sha256; package_name; package_path } ->
        Json.Object
          [
            ("type", Json.String "symbol_found");
            ("symbol", Json.Object [
              ("kind", Json.String symbol_kind);
              ("name", Json.String symbol_name);
            ]);
            ("source", Json.Object [
              ("path", Json.String source_path);
              ("sha256", Json.String source_sha256);
            ]);
            ("package", Json.Object [
              ("name", Json.String package_name);
              ("path", Json.String package_path);
            ]);
          ]
    | SymbolNotFound ->
        Json.Object [ ("type", Json.String "symbol_not_found") ]
    | CycleDetected { session_id; detected_at; cycle_nodes } ->
        let timestamp = Std.Datetime.to_iso8601 detected_at in
        Json.Object
          [
            ("type", Json.String "cycle_detected");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("detected_at", Json.String timestamp);
            ( "cycle_nodes",
              Json.Array (List.map (fun s -> Json.String s) cycle_nodes) );
          ]
    | BuildEvent { session_id; event } -> (
        match Tusk_executor.Telemetry_events.to_json event with
        | Some event_json ->
            Json.Object
              [
                ("type", Json.String "build_event");
                ("session_id", Json.String (Session_id.to_string session_id));
                ("event_data", event_json);
              ]
        | None ->
            Json.Object
              [
                ("type", Json.String "build_event_skipped");
                ("session_id", Json.String (Session_id.to_string session_id));
              ])
    | BuildComplete { session_id; completed_at; stats; results } ->
        let timestamp = Std.Datetime.to_iso8601 completed_at in
        Json.Object
          [
            ("type", Json.String "build_complete");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("completed_at", Json.String timestamp);
            ("results", Json.Array (List.map build_result_to_json results));
            ("duration_ms", Json.Int stats.duration_ms);
            ("packages_built", Json.Int stats.packages_built);
            ("packages_failed", Json.Int stats.packages_failed);
            ("total_modules", Json.Int stats.total_modules);
            ("cache_hits", Json.Int stats.cache_hits);
            ("cache_misses", Json.Int stats.cache_misses);
          ]
    | BuildFailed { session_id; failed_at; stats; built; errors } ->
        let timestamp = Std.Datetime.to_iso8601 failed_at in
        Json.Object
          [
            ("type", Json.String "build_failed");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("failed_at", Json.String timestamp);
            ("built", Json.Array (List.map build_result_to_json built));
            ("errors", Json.Array (List.map build_result_to_json errors));
            ("duration_ms", Json.Int stats.duration_ms);
            ("packages_built", Json.Int stats.packages_built);
            ("packages_failed", Json.Int stats.packages_failed);
            ("total_modules", Json.Int stats.total_modules);
            ("cache_hits", Json.Int stats.cache_hits);
            ("cache_misses", Json.Int stats.cache_misses);
          ]
    | PlanningFailed { session_id; failed_at; reason } ->
        let timestamp = Std.Datetime.to_iso8601 failed_at in
        Json.Object
          [
            ("type", Json.String "planning_failed");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("failed_at", Json.String timestamp);
            ("reason", Json.String reason);
          ]
    | ShutdownAck -> Json.Object [ ("type", Json.String "shutdown_ack") ]
    | RestartAck -> Json.Object [ ("type", Json.String "restart_ack") ]
    | FormatResult { formatted_code; changed } ->
        Json.Object
          [
            ("type", Json.String "format_result");
            ("formatted_code", Json.String formatted_code);
            ("changed", Json.Bool changed);
          ]
    | FormatError { error } ->
        Json.Object
          [ ("type", Json.String "format_error"); ("error", Json.String error) ]
    | PackageCreated { path; name } ->
        Json.Object
          [
            ("type", Json.String "package_created");
            ("path", Json.String path);
            ("name", Json.String name);
          ]
    | FormatAllResult { files_formatted; files_failed; errors } ->
        Json.Object
          [
            ("type", Json.String "format_all_result");
            ("files_formatted", Json.Int files_formatted);
            ("files_failed", Json.Int files_failed);
            ( "errors",
              Json.Array
                (List.map
                   (fun (file, error) ->
                     Json.Object
                       [
                         ("file", Json.String file); ("error", Json.String error);
                       ])
                   errors) );
          ]
    | PackageCreationError { error } ->
        Json.Object
          [
            ("type", Json.String "package_creation_error");
            ("error", Json.String error);
          ]
    | PackageNotFound { session_id; package_name; available_packages } ->
        Json.Object
          [
            ("type", Json.String "package_not_found");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("package_name", Json.String package_name);
            ( "available_packages",
              Json.Array (List.map (fun p -> Json.String p) available_packages)
            );
          ]
    | Error msg -> Json.Object [ ("error", Json.String msg) ]

  let response_of_json json =
    (* This would parse JSON back to response, needed for client *)
    (* Debug: log what we're trying to parse *)
    (* Printf.eprintf "[TUSK PROTOCOL] Parsing response JSON: %s\n" (Json.to_string json); *)
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
            let started_at = Std.Datetime.now () in
            (* Will be overridden by server timestamp *)
            Ok (BuildStarted { session_id; started_at })
        | Some (Json.String "found_executable") ->
            let package =
              match List.assoc_opt "package" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let binary =
              match List.assoc_opt "binary" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            Ok (ExecutableFound { package; binary })
        | Some (Json.String "executable_not_found") -> Ok ExecutableNotFound
        | Some (Json.String "artifact_found") -> (
            match List.assoc_opt "path" fields with
            | Some (Json.String p) -> Ok (ArtifactFound { path = p })
            | _ -> Error (Json.String "Invalid artifact_found response"))
        | Some (Json.String "artifact_not_found") -> (
            match List.assoc_opt "error" fields with
            | Some (Json.String e) -> Ok (ArtifactNotFound { error = e })
            | _ -> Error (Json.String "Invalid artifact_not_found response"))
        | Some (Json.String "symbol_found") -> (
            match
              ( List.assoc_opt "symbol" fields,
                List.assoc_opt "source" fields,
                List.assoc_opt "package" fields )
            with
            | ( Some (Json.Object symbol_fields),
                Some (Json.Object source_fields),
                Some (Json.Object package_fields) ) -> (
                match
                  ( List.assoc_opt "kind" symbol_fields,
                    List.assoc_opt "name" symbol_fields,
                    List.assoc_opt "path" source_fields,
                    List.assoc_opt "sha256" source_fields,
                    List.assoc_opt "name" package_fields,
                    List.assoc_opt "path" package_fields )
                with
                | ( Some (Json.String symbol_kind),
                    Some (Json.String symbol_name),
                    Some (Json.String source_path),
                    Some (Json.String source_sha256),
                    Some (Json.String package_name),
                    Some (Json.String package_path) ) ->
                    Ok (SymbolFound { 
                      symbol_kind; symbol_name; source_path; source_sha256; package_name; package_path;
                    })
                | _ -> Error (Json.String "Invalid symbol_found nested fields"))
            | _ -> Error (Json.String "Invalid symbol_found response structure"))
        | Some (Json.String "symbol_not_found") -> Ok SymbolNotFound
        | Some (Json.String "build_event") -> (
            (* Deserialize the event using Telemetry_events.from_json *)
            match List.assoc_opt "event_data" fields with
            | Some event_json -> (
                match Tusk_executor.Telemetry_events.from_json event_json with
                | Ok event ->
                    let session_id =
                      match List.assoc_opt "session_id" fields with
                      | Some (Json.String s) -> Session_id.of_string s
                      | _ -> Session_id.make ()
                    in
                    Ok (BuildEvent { session_id; event })
                | Error err ->
                    (* Skip events that can't be deserialized (like Action events) *)
                    Log.debug
                      ("[PROTOCOL] Skipping BuildEvent: "
                      ^ (match err with
                        | Json.String s -> s
                        | _ -> "unknown error"));
                    Error
                      (Json.String "BuildEvent skipped (deserialization failed)")
                )
            | None -> Error (Json.String "BuildEvent missing event_data field"))
        | Some (Json.String "build_event_skipped") ->
            (* Server explicitly skipped this event *)
            Error (Json.String "BuildEvent skipped by server")
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
            let detected_at = Std.Datetime.now () in
            (* Will be overridden by server timestamp *)
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
            let completed_at = Std.Datetime.now () in
            (* Will be overridden by server timestamp *)
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
                   results = [];
                 })
        | Some (Json.String "build_failed") ->
            let built_packages =
              match List.assoc_opt "built_packages" fields with
              | Some (Json.Array arr) ->
                  List.filter_map
                    (function Json.String s -> Some s | _ -> None)
                    arr
              | _ -> []
            in
            let failed_packages =
              match List.assoc_opt "failed_packages" fields with
              | Some (Json.Array arr) ->
                  List.filter_map
                    (function Json.String s -> Some s | _ -> None)
                    arr
              | _ -> []
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
            let failed_at = Std.Datetime.now () in
            (* Will be overridden by server timestamp *)
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
                   built = [];
                   errors = [];
                 })
        | Some (Json.String "planning_failed") ->
            let session_id =
              match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            let reason =
              match List.assoc_opt "reason" fields with
              | Some (Json.String s) -> s
              | _ -> "Unknown planning error"
            in
            let failed_at = Std.Datetime.now () in
            Ok (PlanningFailed { session_id; failed_at; reason })
        | Some (Json.String "shutdown_ack") -> Ok ShutdownAck
        | Some (Json.String "restart_ack") -> Ok RestartAck
        | Some (Json.String "format_result") -> (
            match
              ( List.assoc_opt "formatted_code" fields,
                List.assoc_opt "changed" fields )
            with
            | Some (Json.String formatted_code), Some (Json.Bool changed) ->
                Ok (FormatResult { formatted_code; changed })
            | _ -> Error (Json.String "Invalid format_result response"))
        | Some (Json.String "format_error") -> (
            match List.assoc_opt "error" fields with
            | Some (Json.String error) -> Ok (FormatError { error })
            | _ -> Error (Json.String "Invalid format_error response"))
        | Some (Json.String "package_created") -> (
            match
              (List.assoc_opt "path" fields, List.assoc_opt "name" fields)
            with
            | Some (Json.String path), Some (Json.String name) ->
                Ok (PackageCreated { path; name })
            | _ -> Error (Json.String "Invalid package_created response"))
        | Some (Json.String "package_creation_error") -> (
            match List.assoc_opt "error" fields with
            | Some (Json.String error) -> Ok (PackageCreationError { error })
            | _ -> Error (Json.String "Invalid package_creation_error response")
            )
        | Some (Json.String "package_not_found") -> (
            match
              ( List.assoc_opt "session_id" fields,
                List.assoc_opt "package_name" fields,
                List.assoc_opt "available_packages" fields )
            with
            | ( Some (Json.String sid),
                Some (Json.String package_name),
                Some (Json.Array pkgs) ) ->
                let available_packages =
                  List.filter_map
                    (function Json.String s -> Some s | _ -> None)
                    pkgs
                in
                Ok
                  (PackageNotFound
                     {
                       session_id = Session_id.of_string sid;
                       package_name;
                       available_packages;
                     })
            | _ -> Error (Json.String "Invalid package_not_found response"))
        | Some (Json.String "format_all_result") -> (
            match
              ( List.assoc_opt "files_formatted" fields,
                List.assoc_opt "files_failed" fields,
                List.assoc_opt "errors" fields )
            with
            | ( Some (Json.Int files_formatted),
                Some (Json.Int files_failed),
                Some (Json.Array error_arr) ) ->
                let errors =
                  List.map
                    (function
                      | Json.Object error_fields -> (
                          match
                            ( List.assoc_opt "file" error_fields,
                              List.assoc_opt "error" error_fields )
                          with
                          | Some (Json.String file), Some (Json.String error) ->
                              (file, error)
                          | _ -> ("", "Invalid error format"))
                      | _ -> ("", "Invalid error format"))
                    error_arr
                in
                Ok (FormatAllResult { files_formatted; files_failed; errors })
            | _ -> Error (Json.String "Invalid format_all_result response"))
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
                        Ok (PackageGraph { nodes })
                    | _ -> (
                        match List.assoc_opt "error" fields with
                        | Some (Json.String msg) -> Ok (Error msg)
                        | _ -> Error json)))))
    | _ -> Error json
end
