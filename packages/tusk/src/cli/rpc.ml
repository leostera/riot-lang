(** RPC command implementation *)

open Std
open Std.Data
open Core
open Model
open Server

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
  let workspace =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  Server.Server_manager.ensure_running ~workspace
  |> Result.expect ~msg:"Failed to connect to server"

let handle_ping client _sub_matches =
  let result = Tusk_jsonrpc.Client.ping client in
  match result with
  | Ok () ->
      let json = Json.Object [ ("type", Json.String "pong") ] in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_workspace client _sub_matches =
  let result = Tusk_jsonrpc.Client.get_workspace_config client in
  match result with
  | Ok config ->
      let json =
        Json.Object
          [
            ("type", Json.String "workspace_config");
            ( "workspace_root",
              Json.String config.Tusk_jsonrpc.WireProtocol.workspace_root );
            ( "target_dir",
              Json.String config.Tusk_jsonrpc.WireProtocol.target_dir );
            ("toolchain", Json.String config.Tusk_jsonrpc.WireProtocol.toolchain);
            ( "toolchain_path",
              Json.String config.Tusk_jsonrpc.WireProtocol.toolchain_path );
            ( "packages",
              Json.Array
                (List.map
                   (fun (pkg : Tusk_jsonrpc.WireProtocol.package_info) ->
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
                   config.Tusk_jsonrpc.WireProtocol.packages) );
            ( "total_packages",
              Json.Int config.Tusk_jsonrpc.WireProtocol.total_packages );
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
  let result = Tusk_jsonrpc.Client.find_executable client name in
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
    Tusk_jsonrpc.Client.find_artifact client ~package:pkg ~kind:"binary" ~name
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
  let result = Tusk_jsonrpc.Client.get_package_info client package_name in
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
                    Json.String detail.Tusk_jsonrpc.WireProtocol.package.name );
                  ( "path",
                    Json.String detail.Tusk_jsonrpc.WireProtocol.package.path );
                  ( "dependencies",
                    Json.Array
                      (List.map
                         (fun d -> Json.String d)
                         detail.Tusk_jsonrpc.WireProtocol.package.dependencies)
                  );
                ] );
            ( "sources",
              Json.Array
                (List.map
                   (fun s -> Json.String s)
                   detail.Tusk_jsonrpc.WireProtocol.sources) );
            ( "dependency_names",
              Json.Array
                (List.map
                   (fun d -> Json.String d)
                   detail.Tusk_jsonrpc.WireProtocol.dependency_names) );
          ]
      in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_graph client _sub_matches =
  let result = Tusk_jsonrpc.Client.get_build_graph client in
  match result with
  | Ok response ->
      let nodes_json =
        List.map
          (fun node ->
            Json.Object
              [
                ("name", Json.String node.Tusk_jsonrpc.WireProtocol.package_name);
                ("status", Json.String node.Tusk_jsonrpc.WireProtocol.status);
                ( "dependencies",
                  Json.Array
                    (List.map
                       (fun d -> Json.String d)
                       node.Tusk_jsonrpc.WireProtocol.deps) );
              ])
          response.Tusk_jsonrpc.WireProtocol.nodes
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
    | Some pkg -> Tusk_jsonrpc.Client.BuildPackage pkg
    | None -> Tusk_jsonrpc.Client.BuildAll
  in
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
        let json = Event.to_json event in
        println "%s" (Json.to_string json)
    | Tusk_jsonrpc.Client.BuildFinished result ->
        let json =
          match result with
          | Ok () -> Json.Object [ ("type", Json.String "success") ]
          | Error msg ->
              Json.Object
                [ ("type", Json.String "error"); ("message", Json.String msg) ]
        in
        println "%s" (Json.to_string json)
  in
  let result = Tusk_jsonrpc.Client.build_streaming client request callback in
  match result with
  | Ok _ -> Ok ()
  | Error e ->
      let response =
        Json.Object
          [ ("type", Json.String "Error"); ("message", Json.String e) ]
      in
      println "%s" (Json.to_string response);
      Error (Failure e)

let handle_restart client _sub_matches =
  let result = Tusk_jsonrpc.Client.restart client in
  match result with
  | Ok () ->
      let json = Json.Object [ ("type", Json.String "restarted") ] in
      println "%s" (Json.to_string json);
      Ok ()
  | Error e ->
      println "Error: %s" e;
      Error (Failure e)

let handle_shutdown client _sub_matches =
  let result = Tusk_jsonrpc.Client.shutdown client in
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
  let result =
    Tusk_jsonrpc.Client.format_file client ~file_path ~check_only:false
  in
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
  let result =
    Tusk_jsonrpc.Client.format_file client ~file_path ~check_only:true
  in
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
  let result = Tusk_jsonrpc.Client.format_code client ~code ~file_path in
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
  (* Tusk_jsonrpc.Client.close client; *)
  result
