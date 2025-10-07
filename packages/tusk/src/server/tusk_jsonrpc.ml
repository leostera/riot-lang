(** Tusk JSON-RPC Protocol - JSON-RPC 2.0 compliant *)

open Std
open Std.Data
open Core
open Model

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
let method_format_file = "tusk.formatFile"
let method_format_code = "tusk.formatCode"
let method_format_all = "tusk.formatAll"
let method_new_package = "tusk.newPackage"
let method_find_executable = "tusk.findExecutable"
let method_find_artifact = "tusk.findArtifact"

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
    | FindExecutable of string
    | FindArtifact of { package : string; kind : string; name : string }
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

  type response =
    | Pong
    | BuildGraph of build_graph_response
    | WorkspaceConfig of workspace_config
    | PackageInfo of package_detail
    | BuildStarted of { session_id : Session_id.t; started_at : Std.Datetime.t }
    | BuildEvent of { session_id : Session_id.t; event : Event.t }
    | CycleDetected of {
        session_id : Session_id.t;
        detected_at : Std.Datetime.t;
        cycle_nodes : string list;
      }
    | BuildComplete of {
        session_id : Session_id.t;
        completed_at : Std.Datetime.t;
        stats : build_stats;
      }
    | BuildFailed of {
        session_id : Session_id.t;
        failed_at : Std.Datetime.t;
        stats : build_stats;
        error : string;
      }
    | ExecutableFound of { package : string; binary : string }
    | ExecutableNotFound
    | ArtifactFound of { path : string }
    | ArtifactNotFound of { error : string }
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
                                        format "Unbound value %s" name
                                    | Event.UnboundModule { name } ->
                                        format "Unbound module %s" name
                                    | Event.FileNotFound { filename } ->
                                        format "Cannot find file %s" filename
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
                           format "Unbound value %s" name
                       | Event.UnboundModule { name } ->
                           format "Unbound module %s" name
                       | Event.FileNotFound { filename } ->
                           format "Cannot find file %s" filename
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
          | Event.UnboundValue { name } -> format "Unbound value %s" name
          | Event.UnboundModule { name } -> format "Unbound module %s" name
          | Event.FileNotFound { filename } ->
              format "Cannot find file %s" filename
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
    | GetBuildGraph -> { method_ = method_get_build_graph; params = NoParams }
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
    | "tusk.getBuildGraph" -> Ok GetBuildGraph
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
    | BuildEvent { session_id; event } ->
        Json.Object
          [
            ("type", Json.String "build_event");
            ("session_id", Json.String (Session_id.to_string session_id));
            ("event_data", Event.to_json event);
          ]
    | BuildComplete { session_id; completed_at; stats } ->
        let timestamp = Std.Datetime.to_iso8601 completed_at in
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
        let timestamp = Std.Datetime.to_iso8601 failed_at in
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
        | Some (Json.String "build_event") ->
            let session_id =
              match List.assoc_opt "session_id" fields with
              | Some (Json.String s) -> Session_id.of_string s
              | _ -> Session_id.make ()
            in
            (* Parse the event from event_data field *)
            let event =
              match List.assoc_opt "event_data" fields with
              | Some event_json -> (
                  match Event.from_json event_json with
                  | Ok evt -> evt
                  | Error err ->
                      failwith (format "Failed to parse event_data: %s" err))
              | None -> failwith "Missing event_data field in BuildEvent"
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
                   error;
                 })
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
          | `System_error msg -> format "System error: %s" msg
        in
        Error (format "Failed to connect to server: %s" error_msg)

  (** Format all OCaml files in the workspace *)
  let format_all t ~mode =
    match
      Jsonrpc.Client.call t.client ~method_:method_format_all
        ~params:
          (Jsonrpc.Named
             [
               ( "mode",
                 Json.String
                   (match mode with `check -> "check" | `write -> "write") );
             ])
        ()
    with
    | Ok
        (TuskProtocol.FormatAllResult { files_formatted; files_failed; errors })
      ->
        Ok (files_formatted, files_failed, errors)
    | Ok (TuskProtocol.FormatError { error }) -> Error error
    | Ok _ -> Error "Invalid format all response"
    | Error e ->
        Error
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

  (** Create a new package *)
  let new_package t ~path ~name ~is_library =
    match
      Jsonrpc.Client.call t.client ~method_:method_new_package
        ~params:
          (Jsonrpc.Named
             [
               ("path", Json.String path);
               ("name", Json.String name);
               ("is_library", Json.Bool is_library);
             ])
        ()
    with
    | Ok (TuskProtocol.PackageCreated { path; name }) -> Ok (path, name)
    | Ok (TuskProtocol.PackageCreationError { error }) -> Error error
    | Ok _ -> Error "Invalid package creation response"
    | Error e ->
        Error
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

  (** Create a new package in ./packages/ with dependencies *)
  let create_package t ~name ~deps ~is_library =
    (* Create package in ./packages/<name> *)
    let path = format "packages/%s" name in
    match new_package t ~path ~name ~is_library with
    | Ok (created_path, created_name) ->
        (* TODO: Add dependencies to tusk.toml *)
        let files =
          [ format "%s/tusk.toml" created_path; format "%s/src" created_path ]
        in
        Ok (created_path, files)
    | Error e -> Error e

  (** Create a new module file in a package *)
  let create_module t ~package ~module_name ~contents =
    (* For now, return an error since we need filesystem access from the server *)
    Error
      (format
         "Module creation not yet implemented. Please create %s.ml in package \
          '%s' manually"
         module_name package)

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
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

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
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

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
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

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
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

  (** Build with streaming support *)
  let build_streaming t request callback =
    let typed_request =
      match request with
      | BuildPackage pkg -> TuskProtocol.BuildPackage pkg
      | BuildAll -> TuskProtocol.BuildAll
    in

    (* Send the typed build request - this starts a streaming response *)
    match Jsonrpc.Client.send_request t.client typed_request with
    | Error e -> Error (format "Failed to send request: %s" e)
    | Ok () -> (
        (* Receive the first response *)
        match Jsonrpc.Client.receive_response t.client with
        | Error e -> Error (format "Failed to receive response: %s" e)
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
                          Ok (TuskProtocol.BuildEvent { session_id = _; event });
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
                  | Ok
                      {
                        result =
                          Ok
                            (TuskProtocol.PackageNotFound
                               { session_id; package_name; available_packages });
                        _;
                      } ->
                      (* Report package not found as an error and finish build *)
                      let error_msg =
                        format "Package '%s' not found. Available: %s"
                          package_name
                          (String.concat ", " available_packages)
                      in
                      callback (BuildFinished (Error error_msg));
                      Ok (BuildFinished (Error error_msg))
                  | Ok
                      {
                        result = Ok (TuskProtocol.BuildComplete { stats; _ });
                        _;
                      } ->
                      if stats.packages_failed > 0 then
                        Ok
                          (BuildFinished
                             (Error
                                (format "%d packages failed to build"
                                   stats.packages_failed)))
                      else Ok (BuildFinished (Ok ()))
                  | Ok
                      {
                        result =
                          Ok (TuskProtocol.BuildFailed { session_id; error; _ });
                        _;
                      } ->
                      Ok (BuildFinished (Error error))
                  | Ok { result = Ok (TuskProtocol.Error msg); _ } ->
                      (* Got a general error response *)
                      Error (format "Server error: %s" msg)
                  | Ok { result = Error err; _ } ->
                      Ok (BuildFinished (Error err.message))
                  | Error e -> Error (format "Failed to receive event: %s" e)
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
                        | Ok (TuskProtocol.FormatResult _) -> "FormatResult"
                        | Ok (TuskProtocol.FormatError _) -> "FormatError"
                        | Ok (TuskProtocol.FormatAllResult _) ->
                            "FormatAllResult"
                        | Ok (TuskProtocol.PackageCreated _) -> "PackageCreated"
                        | Ok (TuskProtocol.PackageCreationError _) ->
                            "PackageCreationError"
                        | Ok (TuskProtocol.PackageNotFound _) ->
                            "PackageNotFound"
                        | Ok TuskProtocol.ExecutableNotFound ->
                            "ExecutableNotFound"
                        | Ok (TuskProtocol.ExecutableFound _) ->
                            "ExecutableFound"
                        | Ok (TuskProtocol.ArtifactFound _) -> "ArtifactFound"
                        | Ok (TuskProtocol.ArtifactNotFound _) ->
                            "ArtifactNotFound"
                        | Ok (TuskProtocol.Error _) -> "Error"
                        | Error e -> format "JsonRpcError(%s)" e.message
                      in
                      Log.debug
                        "[CLIENT] Unexpected response in receive_events: %s"
                        resp_type;
                      Error "Unexpected response type"
                in
                receive_events ()
            | Ok
                (TuskProtocol.BuildComplete
                   { session_id; completed_at = _; stats }) ->
                (* Check if build actually succeeded *)
                if stats.packages_failed > 0 then
                  Ok
                    (BuildFinished
                       (Error
                          (format "%d packages failed to build"
                             stats.packages_failed)))
                else Ok (BuildFinished (Ok ()))
            | Ok (TuskProtocol.BuildFailed { session_id; error; _ }) ->
                (* Direct error *)
                Ok (BuildFinished (Error error))
            | Ok (TuskProtocol.Error msg) ->
                (* Other error *)
                Ok (BuildFinished (Error msg))
            | Error err ->
                Error (format "Build request failed: %s" err.Jsonrpc.message)
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
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

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
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

  (** Build all packages *)
  let build_all t =
    match
      Jsonrpc.Client.call t.client ~method_:method_build_all
        ~params:Jsonrpc.NoParams ()
    with
    | Ok response -> Ok response
    | Error e ->
        Error
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

  (** Find an executable by binary name *)
  let find_executable t name =
    match
      Jsonrpc.Client.call t.client ~method_:method_find_executable
        ~params:(Jsonrpc.Named [ ("name", Json.String name) ])
        ()
    with
    | Ok (TuskProtocol.ExecutableFound { package; binary }) ->
        Ok (Some (package, binary))
    | Ok TuskProtocol.ExecutableNotFound -> Ok None
    | Ok _ -> Error "Invalid findExecutable response"
    | Error e ->
        Error
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

  (** Find an artifact path *)
  let find_artifact t ~package ~kind ~name =
    match
      Jsonrpc.Client.call t.client ~method_:method_find_artifact
        ~params:
          (Jsonrpc.Named
             [
               ("package", Json.String package);
               ("kind", Json.String kind);
               ("name", Json.String name);
             ])
        ()
    with
    | Ok (TuskProtocol.ArtifactFound { path }) -> Ok path
    | Ok (TuskProtocol.ArtifactNotFound { error }) -> Error error
    | Ok _ -> Error "Invalid findArtifact response"
    | Error e ->
        Error
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

  (** Restart the server *)
  let restart t =
    match
      Jsonrpc.Client.call t.client ~method_:method_restart
        ~params:Jsonrpc.NoParams ()
    with
    | Ok _ -> Ok ()
    | Error e ->
        Error
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

  (** Format a file with ocamlformat *)
  let format_file t ~file_path ~check_only =
    match
      Jsonrpc.Client.call t.client ~method_:method_format_file
        ~params:
          (Jsonrpc.Named
             [
               ("file_path", Json.String file_path);
               ("check_only", Json.Bool check_only);
             ])
        ()
    with
    | Ok (TuskProtocol.FormatResult { formatted_code; changed }) ->
        Ok (formatted_code, changed)
    | Ok (TuskProtocol.FormatError { error }) -> Error error
    | Ok _ -> Error "Invalid format response"
    | Error e ->
        Error
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

  (** Format code string with ocamlformat *)
  let format_code t ~code ~file_path =
    let params =
      match file_path with
      | Some path ->
          Jsonrpc.Named
            [ ("code", Json.String code); ("file_path", Json.String path) ]
      | None -> Jsonrpc.Named [ ("code", Json.String code) ]
    in
    match
      Jsonrpc.Client.call t.client ~method_:method_format_code ~params ()
    with
    | Ok (TuskProtocol.FormatResult { formatted_code; changed }) ->
        Ok (formatted_code, changed)
    | Ok (TuskProtocol.FormatError { error }) -> Error error
    | Ok _ -> Error "Invalid format response"
    | Error e ->
        Error
          (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)
end

(** Server module for Tusk RPC *)
module Server = struct
  open Miniriot

  type ctx = { server_pid : Pid.t }

  let handle_build ctx reply request =
    (* Generate session ID client-side so we can register immediately *)
    let session_id = Session_id.make () in

    (* Register ourselves with the logger to receive events BEFORE sending build request *)
    Tusk_log.add_rpc_handler ~session_id ~client:(self ());

    (* Convert to tusk_protocol message type *)
    let server_request =
      match request with
      | TuskProtocol.BuildPackage pkg ->
          Tusk_protocol.Build
            {
              client_pid = self ();
              target = Tusk_protocol.Package pkg;
              session_id;
            }
      | TuskProtocol.BuildAll ->
          Tusk_protocol.Build
            { client_pid = self (); target = Tusk_protocol.All; session_id }
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
        (* Already registered with the logger before sending request *)

        (* Send BuildStarted to client *)
        reply
          (TuskProtocol.BuildStarted
             { session_id; started_at = Std.Datetime.now () });

        (* Now handle log events, PackageNotFound, CycleDetected and BuildCompleted *)
        let rec event_loop () =
          let selector = function
            | Tusk_log.Event event when event.Event.session_id = session_id ->
                `select (`log_event event)
            | Tusk_protocol.ServerResponse
                (Tusk_protocol.PackageNotFound
                   { session_id = sid; package_name; available_packages })
              when sid = session_id ->
                `select (`package_not_found (package_name, available_packages))
            | Tusk_protocol.ServerResponse
                (Tusk_protocol.CycleDetected
                   { session_id = sid; cycle_nodes; detected_at })
              when sid = session_id ->
                `select (`cycle_detected (cycle_nodes, detected_at))
            | Tusk_protocol.ServerResponse
                (Tusk_protocol.BuildCompleted
                   { session_id = sid; completed_At; stats })
              when sid = session_id ->
                `select (`build_complete (completed_At, stats))
            | _ -> `skip
          in
          match receive ~selector () with
          | `log_event event ->
              (* Strip ANSI codes from event before forwarding to client *)
              let clean_event =
                (* Create a new event with ANSI codes stripped from compile errors *)
                match event.Event.kind with
                | Event.CompileError { package; error } ->
                    let clean_error =
                      {
                        error with
                        Event.raw = Event.strip_ansi_codes error.Event.raw;
                        Event.hint = Event.strip_ansi_codes error.Event.hint;
                      }
                    in
                    {
                      event with
                      kind = Event.CompileError { package; error = clean_error };
                    }
                | _ -> event
              in
              (* Forward cleaned event to client *)
              reply
                (TuskProtocol.BuildEvent { session_id; event = clean_event });
              event_loop ()
          | `package_not_found (package_name, available_packages) ->
              (* Forward package not found to client *)
              reply
                (TuskProtocol.PackageNotFound
                   { session_id; package_name; available_packages });
              event_loop ()
          | `cycle_detected (cycle_nodes, detected_at) ->
              (* Forward cycle detected event to client *)
              reply
                (TuskProtocol.CycleDetected
                   { session_id; detected_at; cycle_nodes });
              event_loop ()
          | `build_complete (completed_at, stats) ->
              (* Build is done, send final response with actual stats *)
              reply
                (TuskProtocol.BuildComplete
                   {
                     session_id;
                     completed_at;
                     stats =
                       {
                         duration_ms =
                           int_of_float
                             (Tusk_protocol.BuildStats.get_build_duration stats
                             *. 1000.0);
                         packages_built =
                           Tusk_protocol.BuildStats.get_packages_built stats;
                         packages_failed =
                           Tusk_protocol.BuildStats.get_packages_failed stats;
                         total_modules =
                           Tusk_protocol.BuildStats.get_total_modules stats;
                         cache_hits =
                           Tusk_protocol.BuildStats.get_cache_hits stats;
                         cache_misses =
                           Tusk_protocol.BuildStats.get_cache_misses stats;
                       };
                   })
        in
        event_loop ()
    | _ -> reply (TuskProtocol.Error "Unexpected response")

  let handle_ping ctx reply request =
    Std.Log.debug "[JSONRPC] handle_ping called";
    (* Convert to tusk_protocol message type and send *)
    Std.Log.debug "[JSONRPC] Sending Ping to server %s from %s"
      (Pid.to_string ctx.server_pid)
      (Pid.to_string (self ()));
    send ctx.server_pid
      (Tusk_protocol.ServerRequest (Tusk_protocol.Ping { client_pid = self () }));
    Std.Log.debug "[JSONRPC] Waiting for Pong response...";
    (* Wait for response *)
    let selector = function
      | Tusk_protocol.ServerResponse response -> `select response
      | _ -> `skip
    in
    match receive ~selector () with
    | Tusk_protocol.Pong ->
        Std.Log.debug "[JSONRPC] Got Pong response, sending reply";
        reply TuskProtocol.Pong;
        Std.Log.debug "[JSONRPC] Reply sent"
    | _ ->
        Std.Log.error "[JSONRPC] Got unexpected response";
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
               toolchain_path =
                 Std.Path.to_string (Toolchains.get_toolchain_path toolchain);
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

  let handle_find_executable ctx reply name =
    send ctx.server_pid
      (Tusk_protocol.ServerRequest
         (Tusk_protocol.FindExecutable { client_pid = self (); name }));
    let selector = function
      | Tusk_protocol.ServerResponse
          (Tusk_protocol.ExecutableFound { package; binary }) ->
          `select (`found (package, binary))
      | Tusk_protocol.ServerResponse Tusk_protocol.ExecutableNotFound ->
          `select `not_found
      | _ -> `skip
    in
    match receive ~selector () with
    | `found (package, binary) ->
        reply (TuskProtocol.ExecutableFound { package; binary })
    | `not_found -> reply TuskProtocol.ExecutableNotFound

  let handle_find_artifact ctx reply package kind name =
    send ctx.server_pid
      (Tusk_protocol.ServerRequest
         (Tusk_protocol.FindArtifact
            { client_pid = self (); package; kind; name }));
    let selector = function
      | Tusk_protocol.ServerResponse (Tusk_protocol.ArtifactFound { path }) ->
          `select (`found path)
      | Tusk_protocol.ServerResponse (Tusk_protocol.ArtifactNotFound { error })
        ->
          `select (`err error)
      | _ -> `skip
    in
    match receive ~selector () with
    | `found path ->
        reply (TuskProtocol.ArtifactFound { path = Std.Path.to_string path })
    | `err error -> reply (TuskProtocol.ArtifactNotFound { error })

  let handle_format_file ctx reply file_path check_only =
    (* Send format request to the actual tusk server *)
    match Std.Path.of_string file_path with
    | Ok path -> (
        send ctx.server_pid
          (Tusk_protocol.ServerRequest
             (Tusk_protocol.FormatFile
                { client_pid = self (); file_path = path; check_only }));

        (* Wait for response *)
        let selector = function
          | Tusk_protocol.ServerResponse
              (Tusk_protocol.FormatResult { formatted_code; changed }) ->
              `select (`format_result (formatted_code, changed))
          | Tusk_protocol.ServerResponse (Tusk_protocol.FormatError { error })
            ->
              `select (`format_error error)
          | _ -> `skip
        in
        match receive ~selector () with
        | `format_result (formatted_code, changed) ->
            reply (TuskProtocol.FormatResult { formatted_code; changed })
        | `format_error error -> reply (TuskProtocol.FormatError { error })
        | exception _ ->
            reply
              (TuskProtocol.FormatError { error = "Format request timed out" }))
    | Error _ ->
        reply (TuskProtocol.FormatError { error = "Invalid file path" })

  let handle_format_code ctx reply code file_path =
    (* Send format request to the actual tusk server *)
    let file_path_opt =
      match file_path with
      | Some fp -> (
          match Std.Path.of_string fp with Ok p -> Some p | Error _ -> None)
      | None -> None
    in
    send ctx.server_pid
      (Tusk_protocol.ServerRequest
         (Tusk_protocol.FormatCode
            { client_pid = self (); code; file_path = file_path_opt }));

    (* Wait for response *)
    let selector = function
      | Tusk_protocol.ServerResponse
          (Tusk_protocol.FormatResult { formatted_code; changed }) ->
          `select (`format_result (formatted_code, changed))
      | Tusk_protocol.ServerResponse (Tusk_protocol.FormatError { error }) ->
          `select (`format_error error)
      | _ -> `skip
    in
    match receive ~selector () with
    | `format_result (formatted_code, changed) ->
        reply (TuskProtocol.FormatResult { formatted_code; changed })
    | `format_error error -> reply (TuskProtocol.FormatError { error })
    | exception _ ->
        reply (TuskProtocol.FormatError { error = "Format request timed out" })

  let handle_format_all ctx reply mode =
    (* Send format all request to the actual tusk server *)
    send ctx.server_pid
      (Tusk_protocol.ServerRequest
         (Tusk_protocol.FormatAll { client_pid = self (); mode }));

    (* Wait for response *)
    let selector = function
      | Tusk_protocol.ServerResponse
          (Tusk_protocol.FormatAllResult
             { files_formatted; files_failed; errors }) ->
          `select (`format_all_result (files_formatted, files_failed, errors))
      | Tusk_protocol.ServerResponse (Tusk_protocol.FormatError { error }) ->
          `select (`format_error error)
      | _ -> `skip
    in
    match receive ~selector () with
    | `format_all_result (files_formatted, files_failed, errors) ->
        reply
          (TuskProtocol.FormatAllResult
             { files_formatted; files_failed; errors })
    | `format_error error -> reply (TuskProtocol.FormatError { error })
    | exception _ ->
        reply
          (TuskProtocol.FormatError { error = "Format all request timed out" })

  let handle_new_package ctx reply path name is_library =
    (* Convert string path to Path.t *)
    match Std.Path.of_string path with
    | Ok path_obj -> (
        send ctx.server_pid
          (Tusk_protocol.ServerRequest
             (Tusk_protocol.NewPackage
                { client_pid = self (); path = path_obj; name; is_library }));

        (* Wait for response *)
        let selector = function
          | Tusk_protocol.ServerResponse
              (Tusk_protocol.PackageCreated { path; name }) ->
              `select (`package_created (path, name))
          | Tusk_protocol.ServerResponse
              (Tusk_protocol.PackageCreationError { error }) ->
              `select (`package_creation_error error)
          | _ -> `skip
        in
        match receive ~selector () with
        | `package_created (path, name) ->
            reply (TuskProtocol.PackageCreated { path; name })
        | `package_creation_error error ->
            reply (TuskProtocol.PackageCreationError { error })
        | exception _ ->
            reply
              (TuskProtocol.PackageCreationError
                 { error = "Package creation request timed out" }))
    | Error _ ->
        reply (TuskProtocol.PackageCreationError { error = "Invalid path" })

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
                Std.Log.debug "[JSONRPC] method_ping handler called";
                match request with
                | TuskProtocol.Ping ->
                    Std.Log.debug
                      "[JSONRPC] Request is Ping, calling handle_ping";
                    handle_ping ctx reply request
                | _ ->
                    Std.Log.error "[JSONRPC] Request is not Ping!";
                    ());
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
            method_ = method_find_executable;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.FindExecutable name ->
                    handle_find_executable ctx reply name
                | _ -> ());
          };
          {
            method_ = method_find_artifact;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.FindArtifact { package; kind; name } ->
                    handle_find_artifact ctx reply package kind name
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
          {
            method_ = method_format_file;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.FormatFile { file_path; check_only } ->
                    handle_format_file ctx reply file_path check_only
                | _ -> ());
          };
          {
            method_ = method_format_code;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.FormatCode { code; file_path } ->
                    handle_format_code ctx reply code file_path
                | _ -> ());
          };
          {
            method_ = method_format_all;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.FormatAll { mode } ->
                    handle_format_all ctx reply mode
                | _ -> ());
          };
          {
            method_ = method_new_package;
            fn =
              (fun reply request ->
                match request with
                | TuskProtocol.NewPackage { path; name; is_library } ->
                    handle_new_package ctx reply path name is_library
                | _ -> ());
          };
        ]
    in
    Log.debug "[RPC SERVER] Registering methods:";
    List.iter (fun h -> Log.debug "  - %s" h.Jsonrpc.Server.method_) methods;
    Jsonrpc.Server.create ~protocol:(module TuskProtocol) ~methods
end
