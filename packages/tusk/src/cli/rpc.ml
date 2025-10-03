(** RPC command implementation *)

open Std
open Std.Data
open Core
open Model
open Server

let create_local_client () =
  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  Server.Server_manager.ensure_running ~workspace
  |> Result.expect ~msg:"Failed to connect to server"

(** Execute the rpc command *)
let run args =
  let cmd = if List.length args > 0 then List.nth args 0 else "" in
  let rest =
    if List.length args > 1 then
      let rec drop n lst =
        if n <= 0 then lst
        else match lst with [] -> [] | _ :: t -> drop (n - 1) t
      in
      drop 1 args
    else []
  in

  (* Show help if no subcommand provided *)
  if cmd = "" then (
    println "Available RPC commands:";
    println "  tusk rpc ping              - Test server connectivity";
    println "  tusk rpc workspace         - Get workspace information";
    println
      "  tusk rpc package <name>    - Get package details including sources";
    println "  tusk rpc graph             - Get build graph";
    println "  tusk rpc build [package]   - Build all or specific package";
    println "  tusk rpc restart           - Restart the server";
    println "  tusk rpc shutdown          - Shutdown the server";
    Ok ())
  else if cmd = "ping" then (
    let client = create_local_client () in
    let result = Tusk_jsonrpc.Client.ping client in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok () ->
        let json = Json.Object [ ("type", Json.String "pong") ] in
        println "%s" (Json.to_string json);
        Ok ()
    | Error e ->
        println "Error: %s" e;
        Error (Failure e))
  else if cmd = "workspace" then (
    let client = create_local_client () in
    let result = Tusk_jsonrpc.Client.get_workspace_config client in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok config ->
        let json =
          Json.Object
            [
              ("type", Json.String "workspace_config");
              ( "workspace_root",
                Json.String config.Tusk_jsonrpc.TuskProtocol.workspace_root );
              ( "target_dir",
                Json.String config.Tusk_jsonrpc.TuskProtocol.target_dir );
              ( "toolchain",
                Json.String config.Tusk_jsonrpc.TuskProtocol.toolchain );
              ( "toolchain_path",
                Json.String config.Tusk_jsonrpc.TuskProtocol.toolchain_path );
              ( "packages",
                Json.Array
                  (List.map
                     (fun (pkg : Tusk_jsonrpc.TuskProtocol.package_info) ->
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
                     config.Tusk_jsonrpc.TuskProtocol.packages) );
              ( "total_packages",
                Json.Int config.Tusk_jsonrpc.TuskProtocol.total_packages );
            ]
        in
        println "%s" (Json.to_string json);
        Ok ()
    | Error e ->
        println "Error: %s" e;
        Error (Failure e))
  else if cmd = "package" then
    match rest with
    | [] ->
        println "Error: package name required";
        println "Usage: tusk rpc package <package-name>";
        Error (Failure "Missing package name")
    | package_name :: _ -> (
        let client = create_local_client () in
        let result = Tusk_jsonrpc.Client.get_package_info client package_name in
        Tusk_jsonrpc.Client.close client;
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
                          Json.String
                            detail.Tusk_jsonrpc.TuskProtocol.package.name );
                        ( "path",
                          Json.String
                            detail.Tusk_jsonrpc.TuskProtocol.package.path );
                        ( "dependencies",
                          Json.Array
                            (List.map
                               (fun d -> Json.String d)
                               detail.Tusk_jsonrpc.TuskProtocol.package
                                 .dependencies) );
                      ] );
                  ( "sources",
                    Json.Array
                      (List.map
                         (fun s -> Json.String s)
                         detail.Tusk_jsonrpc.TuskProtocol.sources) );
                  ( "dependency_names",
                    Json.Array
                      (List.map
                         (fun d -> Json.String d)
                         detail.Tusk_jsonrpc.TuskProtocol.dependency_names) );
                ]
            in
            println "%s" (Json.to_string json);
            Ok ()
        | Error e ->
            println "Error: %s" e;
            Error (Failure e))
  else if cmd = "graph" then (
    let client = create_local_client () in
    let result = Tusk_jsonrpc.Client.get_build_graph client in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok response ->
        let nodes_json =
          List.map
            (fun node ->
              Json.Object
                [
                  ( "name",
                    Json.String node.Tusk_jsonrpc.TuskProtocol.package_name );
                  ("status", Json.String node.Tusk_jsonrpc.TuskProtocol.status);
                  ( "dependencies",
                    Json.Array
                      (List.map
                         (fun d -> Json.String d)
                         node.Tusk_jsonrpc.TuskProtocol.deps) );
                ])
            response.Tusk_jsonrpc.TuskProtocol.nodes
        in
        let json =
          Json.Object
            [
              ("type", Json.String "build_graph");
              ("nodes", Json.Array nodes_json);
            ]
        in
        println "%s" (Json.to_string json);
        Ok ()
    | Error e ->
        println "Error: %s" e;
        Error (Failure e))
  else if cmd = "build" then (
    (* Parse optional package name *)
    let package =
      if List.length args > 1 then Some (List.nth args 1) else None
    in
    let request =
      match package with
      | Some pkg -> Tusk_jsonrpc.Client.BuildPackage pkg
      | None -> Tusk_jsonrpc.Client.BuildAll
    in
    let client = create_local_client () in
    let session_id = ref None in
    let callback = function
      | Tusk_jsonrpc.Client.BuildStarted sid ->
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
      | Tusk_jsonrpc.Client.BuildEvent event ->
          (* Use Event.to_json for all events *)
          let json = Event.to_json event in
          println "%s" (Json.to_string json)
      | Tusk_jsonrpc.Client.BuildFinished result ->
          let json =
            match result with
            | Ok () -> Json.Object [ ("type", Json.String "success") ]
            | Error msg ->
                Json.Object
                  [
                    ("type", Json.String "error"); ("message", Json.String msg);
                  ]
          in
          println "%s" (Json.to_string json)
    in
    let result = Tusk_jsonrpc.Client.build_streaming client request callback in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok _ -> Ok ()
    | Error e ->
        let response =
          Json.Object
            [ ("type", Json.String "Error"); ("message", Json.String e) ]
        in
        println "%s" (Json.to_string response);
        Error (Failure e))
  else if cmd = "restart" then (
    let client = create_local_client () in
    let result = Tusk_jsonrpc.Client.restart client in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok () ->
        let json = Json.Object [ ("type", Json.String "restarted") ] in
        println "%s" (Json.to_string json);
        Ok ()
    | Error e ->
        println "Error: %s" e;
        Error (Failure e))
  else if cmd = "shutdown" then (
    let client = create_local_client () in
    let result = Tusk_jsonrpc.Client.shutdown client in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok () ->
        let json = Json.Object [ ("type", Json.String "shutdown") ] in
        println "%s" (Json.to_string json);
        Ok ()
    | Error e ->
        println "Error: %s" e;
        Error (Failure e))
  else if cmd = "format" then
    (* Format a file *)
    match rest with
    | [] ->
        println "Error: file path required";
        println "Usage: tusk rpc format <file-path>";
        Error (Failure "Missing file path")
    | file_path :: _ -> (
        let client = create_local_client () in
        let result =
          Tusk_jsonrpc.Client.format_file client ~file_path ~check_only:false
        in
        Tusk_jsonrpc.Client.close client;
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
            Error (Failure e))
  else if cmd = "format-check" then
    (* Check if a file needs formatting *)
    match rest with
    | [] ->
        println "Error: file path required";
        println "Usage: tusk rpc format-check <file-path>";
        Error (Failure "Missing file path")
    | file_path :: _ -> (
        let client = create_local_client () in
        let result =
          Tusk_jsonrpc.Client.format_file client ~file_path ~check_only:true
        in
        Tusk_jsonrpc.Client.close client;
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
            Error (Failure e))
  else if cmd = "format-code" then
    (* Format code string *)
    match rest with
    | [] ->
        println "Error: code string required";
        println "Usage: tusk rpc format-code <code-string> [file-hint]";
        Error (Failure "Missing code string")
    | code :: file_hint -> (
        let file_path = match file_hint with [] -> None | h :: _ -> Some h in
        let client = create_local_client () in
        let result = Tusk_jsonrpc.Client.format_code client ~code ~file_path in
        Tusk_jsonrpc.Client.close client;
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
            Error (Failure e))
  else (
    println "Error: Unknown RPC command '%s'" cmd;
    println
      "Available commands: ping, workspace, graph, build [package], format \
       <file>, format-check <file>, format-code <code>, restart, shutdown";
    Error (Failure (format "Unknown RPC command: %s" cmd)))
