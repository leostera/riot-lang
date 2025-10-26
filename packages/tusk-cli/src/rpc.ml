open Std
open Std.Data
open Tusk_model
open Tusk_protocol
open Tusk_server

let command =
  let open ArgParser in
  let open Arg in
  command "rpc"
  |> about "Send RPC command to server"
  |> subcommands
       [
         command "ping" |> about "Test server connectivity";
         command "workspace" |> about "Get workspace information";
         command "graph" |> about "Get build graph";
         command "build"
         |> about "Build all or specific package"
         |> args
              [
                option "package" |> short 'p' |> long "package"
                |> help "Package to build";
              ];
         command "package"
         |> about "Get package details"
         |> args [ positional "name" |> help "Package name" ];
         command "find-executable"
         |> about "Find binary by name"
         |> args [ positional "name" |> help "Binary name" ];
         command "find-artifact" |> about "Find artifact path"
         |> args
              [
                positional "package" |> help "Owning package";
                positional "name" |> help "Binary name";
              ];
         command "format" |> about "Format a file"
         |> args [ positional "file" |> help "File to format" ];
         command "format-check"
         |> about "Check if file needs formatting"
         |> args [ positional "file" |> help "File to check" ];
         command "format-code" |> about "Format code string"
         |> args
              [
                positional "code" |> help "Code to format";
                positional "hint" |> help "Hint for parsing (optional)";
              ];
         command "json"
         |> about "Send raw JSON RPC request"
         |> args [ positional "json" |> help "JSON string" ];
         command "restart" |> about "Restart the server";
         command "shutdown" |> about "Shutdown the server";
       ]

let create_client () =
  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace_root =
    Workspace_manager.find_workspace_root cwd
    |> Option.expect ~msg:"Not in a tusk workspace"
  in
  let workspace = Workspace.make ~root:workspace_root ~packages:[] in
  Tusk_server.Server_manager.ensure_running ~workspace
  |> Result.expect ~msg:"Failed to connect to server"

let handle_ping client _sub_matches =
  let result = Tusk_client.ping client in
  match result with
  | Ok () ->
      let json = Json.Object [ ("type", Json.String "pong") ] in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_workspace client _sub_matches =
  let result = Tusk_client.get_workspace_config client in
  match result with
  | Ok config ->
      let json =
        Json.Object
          [
            ("type", Json.String "workspace_config");
            ( "workspace_root",
              Json.String config.Tusk_protocol.WireProtocol.workspace_root );
            ( "target_dir",
              Json.String config.Tusk_protocol.WireProtocol.target_dir );
            ( "toolchain",
              Json.String config.Tusk_protocol.WireProtocol.toolchain );
            ( "toolchain_path",
              Json.String config.Tusk_protocol.WireProtocol.toolchain_path );
            ( "packages",
              Json.Array
                (List.map
                   (fun (pkg : Tusk_protocol.WireProtocol.package_info) ->
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
                   config.Tusk_protocol.WireProtocol.packages) );
            ( "total_packages",
              Json.Int config.Tusk_protocol.WireProtocol.total_packages );
          ]
      in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_find_executable client sub_matches =
  let open ArgParser in
  let name = get_one sub_matches "name" |> Option.expect ~msg:"name required" in
  let result = Tusk_client.find_executable client name in
  match result with
  | Ok (Some (package, binary)) ->
      let json =
        Json.Object
          [
            ("type", Json.String "found_executable");
            ("package", Json.String package);
            ("binary", Json.String binary);
          ]
      in
      println "%s" (Json.to_string json);
      Ok ()
  | Ok None ->
      let json = Json.Object [ ("type", Json.String "executable_not_found") ] in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_find_artifact client sub_matches =
  let open ArgParser in
  let pkg =
    get_one sub_matches "package" |> Option.expect ~msg:"package required"
  in
  let name = get_one sub_matches "name" |> Option.expect ~msg:"name required" in
  let result =
    Tusk_client.find_artifact client ~package:pkg ~kind:"binary" ~name
  in
  match result with
  | Ok path ->
      let json =
        Json.Object
          [ ("type", Json.String "artifact_found"); ("path", Json.String path) ]
      in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      let json =
        Json.Object
          [
            ("type", Json.String "artifact_not_found"); ("error", Json.String e);
          ]
      in
      println "%s" (Json.to_string json);
      Error (Failure e)

let handle_package client sub_matches =
  let open ArgParser in
  let package_name =
    get_one sub_matches "name" |> Option.expect ~msg:"name required"
  in
  let result = Tusk_client.get_package_info client package_name in
  match result with
  | Ok detail ->
      let json =
        Json.Object
          [
            ("type", Json.String "package_info");
            ( "package",
              Json.Object
                [
                  ( "name",
                    Json.String detail.Tusk_protocol.WireProtocol.package.name
                  );
                  ( "path",
                    Json.String detail.Tusk_protocol.WireProtocol.package.path
                  );
                  ( "dependencies",
                    Json.Array
                      (List.map
                         (fun d -> Json.String d)
                         detail.Tusk_protocol.WireProtocol.package.dependencies)
                  );
                ] );
            ( "sources",
              Json.Array
                (List.map
                   (fun s -> Json.String s)
                   detail.Tusk_protocol.WireProtocol.sources) );
            ( "dependency_names",
              Json.Array
                (List.map
                   (fun d -> Json.String d)
                   detail.Tusk_protocol.WireProtocol.dependency_names) );
          ]
      in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_graph client _sub_matches =
  let result = Tusk_client.get_build_graph client in
  match result with
  | Ok response ->
      let nodes_json =
        List.map
          (fun node ->
            Json.Object
              [
                ( "name",
                  Json.String node.Tusk_protocol.WireProtocol.package_name );
                ("status", Json.String node.Tusk_protocol.WireProtocol.status);
                ( "dependencies",
                  Json.Array
                    (List.map
                       (fun d -> Json.String d)
                       node.Tusk_protocol.WireProtocol.deps) );
              ])
          response.Tusk_protocol.WireProtocol.nodes
      in
      let json =
        Json.Object
          [
            ("type", Json.String "build_graph"); ("nodes", Json.Array nodes_json);
          ]
      in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_build client sub_matches =
  let open ArgParser in
  let package = get_one sub_matches "package" in
  let request =
    match package with
    | Some pkg -> Tusk_client.BuildPackage pkg
    | None -> Tusk_client.BuildAll
  in
  let session_id = ref None in
  let callback = function
    | Tusk_client.BuildStarted sid ->
        session_id := Some sid;
        let dt = Datetime.now () in
        let timestamp = Datetime.to_iso8601 dt in
        let json =
          Json.Object
            [
              ("type", Json.String "build_started");
              ("timestamp", Json.String timestamp);
              ("session_id", Json.String (Session_id.to_string sid));
            ]
        in
        println "%s" (Json.to_string json)
    | Tusk_client.BuildEvent event -> (
        match Tusk_executor.Telemetry_events.to_json event with
        | Some json -> println "%s" (Json.to_string json)
        | None -> ())
    | Tusk_client.BuildCompleted { stats; _ } ->
        let json =
          Json.Object
            [
              ("type", Json.String "success");
              ("packages_built", Json.Int stats.packages_built);
              ("cache_hits", Json.Int stats.cache_hits);
            ]
        in
        println "%s" (Json.to_string json)
    | Tusk_client.BuildFailed { errors; stats; _ } ->
        let error_details =
          List.filter_map
            (fun (r : Tusk_protocol.WireProtocol.build_result) ->
              match r.status with
              | Tusk_protocol.WireProtocol.Failed err ->
                  let error_msg =
                    match err with
                    | Tusk_protocol.WireProtocol.PlanningFailed planning_err ->
                        Tusk_planner.Planning_error.to_string planning_err
                    | Tusk_protocol.WireProtocol.ExecutionFailed { message } ->
                        message
                    | Tusk_protocol.WireProtocol.ActionFailed action_err -> (
                        match action_err with
                        | Tusk_executor.Action_executor.ExecutionFailed
                            { message } ->
                            message
                        | Tusk_executor.Action_executor.OutputsNotCreated
                            { missing } ->
                            format "Outputs not created: %s"
                              (String.concat ", "
                                 (List.map Path.to_string missing))
                        | Tusk_executor.Action_executor.DependenciesFailed
                            { failed } ->
                            format "Dependencies failed: %d actions"
                              (List.length failed))
                  in
                  Some
                    (Json.Object
                       [
                         ("package", Json.String r.package.name);
                         ("error", Json.String error_msg);
                       ])
              | _ -> None)
            errors
        in
        let json =
          Json.Object
            [
              ("type", Json.String "error");
              ("message", Json.String "Build failed");
              ("packages_built", Json.Int stats.packages_built);
              ("packages_failed", Json.Int stats.packages_failed);
              ("errors", Json.Array error_details);
            ]
        in
        println "%s" (Json.to_string json)
  in
  let result = Tusk_client.build_streaming client request callback in
  match result with
  | Ok _ -> Ok ()
  | Error e ->
      let error_msg =
        match e with
        | Tusk_client.JsonrpcError je -> Tusk_client.jsonrpc_error_to_string je
        | Tusk_client.PackageNotFound { package_name; available_packages } ->
            format "Package not found: %s (available: %s)" package_name
              (String.concat ", " available_packages)
        | Tusk_client.UnexpectedEvent { reason; _ } -> reason
      in
      let response =
        Json.Object
          [ ("type", Json.String "Error"); ("message", Json.String error_msg) ]
      in
      println "%s" (Json.to_string response);
      Error (Failure error_msg)

let handle_restart client _sub_matches =
  let result = Tusk_client.restart client in
  match result with
  | Ok () ->
      let json = Json.Object [ ("type", Json.String "restarted") ] in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_shutdown client _sub_matches =
  let result = Tusk_client.shutdown client in
  match result with
  | Ok () ->
      let json = Json.Object [ ("type", Json.String "shutdown") ] in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_format client sub_matches =
  let open ArgParser in
  let file_path =
    get_one sub_matches "file" |> Option.expect ~msg:"file required"
  in
  let result = Tusk_client.format_file client ~file_path ~check_only:false in
  match result with
  | Ok (formatted_code, changed) ->
      let json =
        Json.Object
          [
            ("type", Json.String "format_result");
            ("formatted_code", Json.String formatted_code);
            ("changed", Json.Bool changed);
          ]
      in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_format_check client sub_matches =
  let open ArgParser in
  let file_path =
    get_one sub_matches "file" |> Option.expect ~msg:"file required"
  in
  let result = Tusk_client.format_file client ~file_path ~check_only:true in
  match result with
  | Ok (_formatted_code, changed) ->
      let json =
        Json.Object
          [
            ("type", Json.String "format_check");
            ("needs_formatting", Json.Bool changed);
          ]
      in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_format_code client sub_matches =
  let open ArgParser in
  let code = get_one sub_matches "code" |> Option.expect ~msg:"code required" in
  let file_path = get_one sub_matches "hint" in
  let result = Tusk_client.format_code client ~code ~file_path in
  match result with
  | Ok (formatted_code, changed) ->
      let json =
        Json.Object
          [
            ("type", Json.String "format_result");
            ("formatted_code", Json.String formatted_code);
            ("changed", Json.Bool changed);
          ]
      in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_json _client sub_matches =
  let open ArgParser in
  let json_str =
    get_one sub_matches "json" |> Option.expect ~msg:"json required"
  in
  println "Error: json command not yet implemented";
  println "Would send: %s" json_str;
  Error (Failure "Not implemented")

let run matches =
  let open ArgParser in
  let client = create_client () in
  let result =
    match get_subcommand matches with
    | Some ("ping", sub_matches) -> handle_ping client sub_matches
    | Some ("workspace", sub_matches) -> handle_workspace client sub_matches
    | Some ("find-executable", sub_matches) ->
        handle_find_executable client sub_matches
    | Some ("find-artifact", sub_matches) ->
        handle_find_artifact client sub_matches
    | Some ("package", sub_matches) -> handle_package client sub_matches
    | Some ("graph", sub_matches) -> handle_graph client sub_matches
    | Some ("build", sub_matches) -> handle_build client sub_matches
    | Some ("restart", sub_matches) -> handle_restart client sub_matches
    | Some ("shutdown", sub_matches) -> handle_shutdown client sub_matches
    | Some ("format", sub_matches) -> handle_format client sub_matches
    | Some ("format-check", sub_matches) ->
        handle_format_check client sub_matches
    | Some ("format-code", sub_matches) -> handle_format_code client sub_matches
    | Some ("json", sub_matches) -> handle_json client sub_matches
    | Some (cmd, _) ->
        println "Unknown rpc command: %s" cmd;
        Error (Failure (format "Unknown rpc command: %s" cmd))
    | None ->
        println "No rpc subcommand provided. Use 'tusk rpc --help' for usage.";
        Error (Failure "No rpc subcommand")
  in
  Tusk_client.close client;
  result
