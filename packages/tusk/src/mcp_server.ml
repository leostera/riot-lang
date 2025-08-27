(** MCP (Model Context Protocol) server for tusk build system *)

open Miniriot
open Std.Data

(** Helper to create a tusk client connected to the local server *)
let create_local_client () =
  (* Get current workspace *)
  let cwd = Env.current_dir () |> Result.unwrap in
  match Workspace_manager.scan cwd with
  | Error _ -> Error "Failed to find workspace"
  | Ok workspace -> (
      (* Use Server_manager to ensure server is running and get connected client *)
      match Server_manager.ensure_running ~workspace with
      | Ok client -> Ok client
      | Error _ -> Error "Failed to connect to server")

(** Logging *)
let log_file = ref None

let ensure_log_dir () =
  let home =
    match Env.home_dir () with
    | Some h -> Path.to_string h
    | None -> failwith "Failed to get home dir"
  in
  let log_dir = Filename.concat home ".tusk/logs" in
  (* Create .tusk directory if it doesn't exist *)
  let tusk_dir = Filename.concat home ".tusk" in
  let () =
    if not (File_utils.exists ~path:tusk_dir) then
      let _ =
        Fs.mkdir
          (Path.of_string tusk_dir |> Result.expect ~msg:"Invalid tusk_dir")
          0o755
      in
      ()
  in
  (* Create logs directory if it doesn't exist *)
  let () =
    if not (File_utils.exists ~path:log_dir) then
      let _ =
        Fs.mkdir
          (Path.of_string log_dir |> Result.expect ~msg:"Invalid log_dir")
          0o755
      in
      ()
  in
  log_dir

let init_logging () =
  let log_dir = ensure_log_dir () in
  let log_path = Filename.concat log_dir "mcp.log" in
  let oc = open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 log_path in
  log_file := Some oc;
  (* Write startup message *)
  let dt = Std.Datetime.now () in
  let iso = Std.Datetime.to_iso8601 dt in
  Printf.fprintf oc
    "\n===== MCP Server Started: %s =====\n"
    iso;
  flush oc

let log msg =
  match !log_file with
  | Some oc ->
      let dt = Std.Datetime.now () in
      let timestamp = Std.Datetime.to_iso8601 dt in
      Printf.fprintf oc "[%s] %s\n" timestamp
        msg;
      flush oc
  | None -> ()

let log_json label json =
  log (Printf.sprintf "%s: %s" label (Json.to_string json))

let close_logging () =
  match !log_file with
  | Some oc ->
      log "MCP Server shutting down";
      close_out oc;
      log_file := None
  | None -> ()

(** Available tools *)
let tools =
  [
    {
      Mcp.name = "build";
      description = Some "Build packages in the workspace";
      input_schema =
        Json.Object
          [
            ("type", Json.String "object");
            ( "properties",
              Json.Object
                [
                  ( "package",
                    Json.Object
                      [
                        ("type", Json.String "string");
                        ( "description",
                          Json.String
                            "Package name to build (optional, builds all if \
                             not specified)" );
                      ] );
                ] );
          ];
    };
    {
      Mcp.name = "clean";
      description = Some "Clean build artifacts";
      input_schema =
        Json.Object
          [ ("type", Json.String "object"); ("properties", Json.Object []) ];
    };
    {
      Mcp.name = "run";
      description = Some "Run a binary";
      input_schema =
        Json.Object
          [
            ("type", Json.String "object");
            ( "properties",
              Json.Object
                [
                  ( "binary",
                    Json.Object
                      [
                        ("type", Json.String "string");
                        ( "description",
                          Json.String "Binary name to run (optional)" );
                      ] );
                ] );
          ];
    };
    {
      Mcp.name = "workspace_info";
      description = Some "Get workspace information";
      input_schema =
        Json.Object
          [ ("type", Json.String "object"); ("properties", Json.Object []) ];
    };
    {
      Mcp.name = "build_graph";
      description = Some "Get the build dependency graph";
      input_schema =
        Json.Object
          [ ("type", Json.String "object"); ("properties", Json.Object []) ];
    };
  ]

(** Available resources *)
let resources =
  [
    {
      Mcp.uri = "workspace://info";
      name = Some "Workspace Information";
      description = Some "Current workspace configuration and packages";
      mime_type = Some "application/json";
    };
    {
      Mcp.uri = "build://graph";
      name = Some "Build Graph";
      description = Some "Dependency graph for all packages";
      mime_type = Some "application/json";
    };
    {
      Mcp.uri = "build://status";
      name = Some "Build Status";
      description = Some "Current build status and results";
      mime_type = Some "application/json";
    };
  ]

(** Execute a tool *)
let execute_tool name arguments =
  match name with
  | "build" ->
      (* Temporarily disabled - Client module not available *)
      [ Mcp.Text "Build command is temporarily disabled" ]
  (* Original build handler commented out
  | "build" -> (
      let package =
        match arguments with
        | Some (Json.Object fields) -> (
            match List.assoc_opt "package" fields with
            | Some (Json.String pkg) -> Some pkg
            | _ -> None)
        | _ -> None
      in
      (* Call tusk build via RPC and collect logs *)
      let request =
        match package with
        | Some pkg -> Tusk_jsonrpc.TuskProtocol.BuildPackage pkg
        | None -> Tusk_jsonrpc.TuskProtocol.BuildAll
      in
      log
        (Printf.sprintf "Calling call_build with request: %s"
           (match request with
           | Tusk_jsonrpc.TuskProtocol.BuildAll -> "BuildAll"
           | Tusk_jsonrpc.TuskProtocol.BuildPackage pkg ->
               Printf.sprintf "BuildPackage(%s)" pkg
           | _ -> "other"));
      let session_id = ref None in
      let logs = ref [] in
      let callback = function
        | Tusk_jsonrpc.Client.BuildStarted sid -> session_id := Some sid
        | Tusk_jsonrpc.Client.BuildEvent log_event ->
            (* Now we get typed log events, format them *)
            let formatted = Log.event_to_string log_event in
            logs := formatted :: !logs
        | Tusk_jsonrpc.Client.BuildFinished _ -> ()
      in
      let request_converted =
        match request with
        | Tusk_jsonrpc.TuskProtocol.BuildPackage pkg ->
            Tusk_jsonrpc.Client.BuildPackage pkg
        | Tusk_jsonrpc.TuskProtocol.BuildAll -> Tusk_jsonrpc.Client.BuildAll
        | _ -> Tusk_jsonrpc.Client.BuildAll
      in
      match create_local_client () with
      | Error e ->
          [ Mcp.Text (Printf.sprintf "Failed to connect to server: %s" e) ]
      | Ok client -> (
          match
            Tusk_jsonrpc.Client.build_streaming client request_converted
              callback
          with
          | Ok (Tusk_jsonrpc.Client.BuildFinished (Ok ())) ->
              (* Build succeeded - return logs *)
              let logs = List.rev !logs in
              let sid_str =
                match !session_id with
                | None -> "unknown"
                | Some sid -> Session_id.to_string sid
              in
              log
                (Printf.sprintf "Build succeeded - session: %s, log count: %d"
                   sid_str (List.length logs));
              let log_text = String.concat "\n" logs in
              Tusk_jsonrpc.Client.close client;
              if log_text = "" then
                [
                  Mcp.Text
                    (Printf.sprintf "Build completed successfully (session: %s)"
                       sid_str);
                ]
              else
                [
                  Mcp.Text
                    (Printf.sprintf "Build completed (session: %s):\n%s" sid_str
                       log_text);
                ]
          | Ok (Tusk_jsonrpc.Client.BuildFinished (Error msg)) ->
              (* Build failed - return logs and error *)
              let logs = List.rev !logs in
              let sid_str =
                match !session_id with
                | None -> "unknown"
                | Some sid -> Session_id.to_string sid
              in
              log
                (Printf.sprintf
                   "Build failed - session: %s, error: %s, log count: %d"
                   sid_str msg (List.length logs));
              let log_text = String.concat "\n" logs in
              Tusk_jsonrpc.Client.close client;
              [
                Mcp.Text
                  (Printf.sprintf "Build failed (session: %s): %s\n%s" sid_str
                     msg log_text);
              ]
          | Ok (Tusk_jsonrpc.Client.BuildStarted _) ->
              (* Unexpected - this should be handled by callback *)
              Tusk_jsonrpc.Client.close client;
              [ Mcp.Text "Build started unexpectedly" ]
          | Ok (Tusk_jsonrpc.Client.BuildEvent _) ->
              (* Unexpected - this should be handled by callback *)
              Tusk_jsonrpc.Client.close client;
              [ Mcp.Text "Build event received unexpectedly" ]
          | Error e ->
              log (Printf.sprintf "Build error: %s" e);
              Tusk_jsonrpc.Client.close client;
              [ Mcp.Text (Printf.sprintf "Build failed: %s" e) ]))
  *)
  | "clean" -> (
      (* Run clean command *)
      let result = Command.system "rm -rf ./target" in
      match Std.Command.of_unix_status result with
      | Std.Command.Exited 0 ->
          [ Mcp.Text "Build artifacts cleaned successfully" ]
      | _ -> [ Mcp.Text "Failed to clean build artifacts" ])
  | "workspace_info" -> (
      match create_local_client () with
      | Error e ->
          [ Mcp.Text (Printf.sprintf "Failed to connect to server: %s" e) ]
      | Ok client -> (
          match Tusk_jsonrpc.Client.get_workspace_config client with
          | Ok config ->
              let json =
                Json.Object
                  [
                    ("workspace_root", Json.String config.workspace_root);
                    ("target_dir", Json.String config.target_dir);
                    ("toolchain", Json.String config.toolchain);
                    ("toolchain_path", Json.String config.toolchain_path);
                    ( "packages",
                      Json.Array
                        (List.map
                           (fun (pkg : Tusk_jsonrpc.TuskProtocol.package_info)
                              ->
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
              in
              Tusk_jsonrpc.Client.close client;
              [ Mcp.Text (Json.to_string json) ]
          | Error e ->
              Tusk_jsonrpc.Client.close client;
              [ Mcp.Text (Printf.sprintf "Failed to get workspace info: %s" e) ]
          ))
  | "build_graph" -> (
      match create_local_client () with
      | Error e ->
          [ Mcp.Text (Printf.sprintf "Failed to connect to server: %s" e) ]
      | Ok client -> (
          match Tusk_jsonrpc.Client.get_build_graph client with
          | Ok graph ->
              let nodes_json =
                List.map
                  (fun node ->
                    Json.Object
                      [
                        ( "package",
                          Json.String
                            node.Tusk_jsonrpc.TuskProtocol.package_name );
                        ( "src_dir",
                          Json.String node.Tusk_jsonrpc.TuskProtocol.src_dir );
                        ( "out_dir",
                          Json.String node.Tusk_jsonrpc.TuskProtocol.out_dir );
                        ( "status",
                          Json.String node.Tusk_jsonrpc.TuskProtocol.status );
                        ( "deps",
                          Json.Array
                            (List.map
                               (fun d -> Json.String d)
                               node.Tusk_jsonrpc.TuskProtocol.deps) );
                      ])
                  graph.nodes
              in
              let json = Json.Object [ ("nodes", Json.Array nodes_json) ] in
              Tusk_jsonrpc.Client.close client;
              [ Mcp.Text (Json.to_string json) ]
          | Error e ->
              Tusk_jsonrpc.Client.close client;
              [ Mcp.Text (Printf.sprintf "Failed to get build graph: %s" e) ]))
  | "run" ->
      let binary =
        match arguments with
        | Some (Json.Object fields) -> (
            match List.assoc_opt "binary" fields with
            | Some (Json.String bin) -> Some bin
            | _ -> None)
        | _ -> None
      in
      (* For now, just return info about running *)
      let msg =
        match binary with
        | Some bin -> Printf.sprintf "Would run binary: %s" bin
        | None -> "Would run default binary"
      in
      [ Mcp.Text msg ]
  | _ -> [ Mcp.Text (Printf.sprintf "Unknown tool: %s" name) ]

(** Read a resource *)
let read_resource uri =
  match uri with
  | "workspace://info" -> (
      match create_local_client () with
      | Error e ->
          [
            Mcp.TextContent
              {
                text = Printf.sprintf "Failed to connect to server: %s" e;
                mime_type = None;
              };
          ]
      | Ok client -> (
          match Tusk_jsonrpc.Client.get_workspace_config client with
          | Ok config ->
              let json =
                Json.Object
                  [
                    ("workspace_root", Json.String config.workspace_root);
                    ("target_dir", Json.String config.target_dir);
                    ("toolchain", Json.String config.toolchain);
                    ("toolchain_path", Json.String config.toolchain_path);
                    ( "packages",
                      Json.Array
                        (List.map
                           (fun (pkg : Tusk_jsonrpc.TuskProtocol.package_info)
                              ->
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
              in
              Tusk_jsonrpc.Client.close client;
              [
                Mcp.TextContent
                  {
                    text = Json.to_string json;
                    mime_type = Some "application/json";
                  };
              ]
          | _ ->
              Tusk_jsonrpc.Client.close client;
              [
                Mcp.TextContent
                  { text = "Failed to get workspace info"; mime_type = None };
              ]))
  | "build://graph" -> (
      match create_local_client () with
      | Error e ->
          [
            Mcp.TextContent
              {
                text = Printf.sprintf "Failed to connect to server: %s" e;
                mime_type = None;
              };
          ]
      | Ok client -> (
          match Tusk_jsonrpc.Client.get_build_graph client with
          | Ok graph ->
              let nodes_json =
                List.map
                  (fun node ->
                    Json.Object
                      [
                        ( "package",
                          Json.String
                            node.Tusk_jsonrpc.TuskProtocol.package_name );
                        ( "deps",
                          Json.Array
                            (List.map
                               (fun d -> Json.String d)
                               node.Tusk_jsonrpc.TuskProtocol.deps) );
                      ])
                  graph.nodes
              in
              let json = Json.Object [ ("nodes", Json.Array nodes_json) ] in
              Tusk_jsonrpc.Client.close client;
              [
                Mcp.TextContent
                  {
                    text = Json.to_string json;
                    mime_type = Some "application/json";
                  };
              ]
          | _ ->
              Tusk_jsonrpc.Client.close client;
              [
                Mcp.TextContent
                  { text = "Failed to get build graph"; mime_type = None };
              ]))
  | "build://status" ->
      [
        Mcp.TextContent
          {
            text = "{\"status\": \"ready\"}";
            mime_type = Some "application/json";
          };
      ]
  | _ ->
      [
        Mcp.TextContent
          { text = Printf.sprintf "Unknown resource: %s" uri; mime_type = None };
      ]

(** Handle a request *)
let handle_request (req : Mcp.request) =
  log
    (Printf.sprintf "Handling request: method=%s, id=%s" req.method_name
       (match req.id with Mcp.String s -> s | Mcp.Number n -> string_of_int n));
  match req.params with
  | Some (Mcp.InitializeParams params) ->
      log
        (Printf.sprintf "Initialize request from: %s v%s (protocol: %s)"
           params.client_info.name params.client_info.version
           params.protocol_version);
      Printf.printf "[MCP] Client: %s v%s\n" params.client_info.name
        params.client_info.version;
      Printf.printf "[MCP] Protocol version: %s\n" params.protocol_version;

      Mcp.make_success req.id
        (Mcp.InitializeResult
           {
             protocol_version = "2024-11-05";
             capabilities =
               {
                 tools = Some ();
                 resources = Some { subscribe = None; list_changed = None };
                 prompts = None;
               };
             server_info = { name = "tusk-mcp"; version = "0.1.0" };
             instructions =
               Some
                 "Tusk MCP server provides tools and resources for building \
                  OCaml projects";
           })
  | Some Mcp.InitializedParams ->
      log "Client sent initialized notification";
      Printf.printf "[MCP] Client initialized\n";
      Mcp.make_success req.id Mcp.InitializedResult
  | Some Mcp.ListToolsParams ->
      log "Listing tools";
      Mcp.make_success req.id
        (Mcp.ListToolsResult { tools; next_cursor = None })
  | Some (Mcp.CallToolParams { name; arguments }) ->
      log (Printf.sprintf "Calling tool: %s" name);
      (match arguments with
      | Some args -> log_json "  Arguments" args
      | None -> log "  No arguments");
      let content = execute_tool name arguments in
      Mcp.make_success req.id (Mcp.CallToolResult { content; is_error = None })
  | Some Mcp.ListResourcesParams ->
      log "Listing resources";
      Mcp.make_success req.id
        (Mcp.ListResourcesResult { resources; next_cursor = None })
  | Some (Mcp.ReadResourceParams { uri }) ->
      log (Printf.sprintf "Reading resource: %s" uri);
      let contents = read_resource uri in
      Mcp.make_success req.id (Mcp.ReadResourceResult { contents })
  | Some Mcp.PingParams ->
      log "Ping request";
      Mcp.make_success req.id Mcp.PingResult
  | Some Mcp.ShutdownParams ->
      log "Shutdown request received";
      Printf.printf "[MCP] Shutting down\n";
      Mcp.make_success req.id Mcp.ShutdownResult
  | _ ->
      log (Printf.sprintf "Unknown method: %s" req.method_name);
      Mcp.make_error req.id Mcp.method_not_found "Method not found"

(** MCP server main loop *)
let rec server_loop () =
  (* Read from stdin *)
  try
    let line = input_line stdin in
    log (Printf.sprintf "Received: %s" line);
    (* Parse JSON-RPC request *)
    match Json.of_string line with
    | Ok json -> (
        (* For now, we'll handle requests manually since deserialization isn't implemented *)
        match json with
        | Json.Object fields ->
            let jsonrpc =
              match List.assoc_opt "jsonrpc" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let id =
              match List.assoc_opt "id" fields with
              | Some (Json.String s) -> Mcp.String s
              | Some (Json.Int n) -> Mcp.Number n
              | Some (Json.Float f) -> Mcp.Number (int_of_float f)
              | _ -> Mcp.String "0"
            in
            let method_str =
              match List.assoc_opt "method" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in

            (* Create a simplified request *)
            let params =
              match method_str with
              | "initialize" -> (
                  match List.assoc_opt "params" fields with
                  | Some (Json.Object p) ->
                      let protocol_version =
                        match List.assoc_opt "protocolVersion" p with
                        | Some (Json.String s) -> s
                        | _ -> "2024-11-05"
                      in
                      let client_info =
                        match List.assoc_opt "clientInfo" p with
                        | Some (Json.Object ci) ->
                            let name =
                              match List.assoc_opt "name" ci with
                              | Some (Json.String s) -> s
                              | _ -> "unknown"
                            in
                            let version =
                              match List.assoc_opt "version" ci with
                              | Some (Json.String s) -> s
                              | _ -> "0.0.0"
                            in
                            ({ Mcp.name; version } : Mcp.client_info)
                        | _ -> { Mcp.name = "unknown"; version = "0.0.0" }
                      in
                      Some
                        (Mcp.InitializeParams
                           {
                             protocol_version;
                             capabilities =
                               {
                                 tools = None;
                                 resources = None;
                                 prompts = None;
                                 sampling = None;
                               };
                             client_info;
                           })
                  | _ -> None)
              | "initialized" -> Some Mcp.InitializedParams
              | "tools/list" -> Some Mcp.ListToolsParams
              | "tools/call" -> (
                  match List.assoc_opt "params" fields with
                  | Some (Json.Object p) ->
                      let name =
                        match List.assoc_opt "name" p with
                        | Some (Json.String s) -> s
                        | _ -> ""
                      in
                      let arguments = List.assoc_opt "arguments" p in
                      Some (Mcp.CallToolParams { name; arguments })
                  | _ -> None)
              | "resources/list" -> Some Mcp.ListResourcesParams
              | "resources/read" -> (
                  match List.assoc_opt "params" fields with
                  | Some (Json.Object p) ->
                      let uri =
                        match List.assoc_opt "uri" p with
                        | Some (Json.String s) -> s
                        | _ -> ""
                      in
                      Some (Mcp.ReadResourceParams { uri })
                  | _ -> None)
              | "ping" -> Some Mcp.PingParams
              | "shutdown" -> Some Mcp.ShutdownParams
              | _ -> None
            in

            let request =
              { Mcp.jsonrpc; id; method_name = method_str; params }
            in

            let response = handle_request request in
            let response_json = Mcp.response_to_json response in
            let response_str = Json.to_string response_json in
            log (Printf.sprintf "Sending response: %s" response_str);
            Printf.printf "%s\n%!" response_str;

            (* Continue unless shutdown *)
            if method_str <> "shutdown" then server_loop () else ()
        | _ ->
            log "Invalid JSON-RPC request (not an object)";
            Printf.eprintf "[MCP] Invalid JSON-RPC request\n";
            server_loop ())
    | Error e ->
        log (Printf.sprintf "JSON parse error: %s" e);
        Printf.eprintf "[MCP] JSON parse error: %s\n" e;
        server_loop ()
  with
  | End_of_file ->
      log "Connection closed (EOF)";
      close_logging ();
      Printf.eprintf "[MCP] Connection closed\n"
  | exn ->
      let error_msg = Printexc.to_string exn in
      log (Printf.sprintf "Error in server loop: %s" error_msg);
      Printf.eprintf "[MCP] Error: %s\n" error_msg;
      server_loop ()

(** Start the MCP server *)
let start () =
  (* Initialize logging first *)
  init_logging ();
  log "Starting MCP server";

  Printf.eprintf "[MCP] Tusk MCP Server starting...\n";
  Printf.eprintf "[MCP] Listening on stdin/stdout for JSON-RPC messages\n";
  Printf.eprintf "[MCP] Logging to ~/.tusk/logs/mcp.log\n";

  (* Ensure tusk server is running *)
  let _ =
    try
      log "Checking if tusk server is running...";
      let cwd_path = Std.Env.current_dir () |> Result.unwrap in
      match Workspace_manager.scan cwd_path with
      | Error _ ->
          log "Could not find workspace";
          Printf.eprintf "[MCP] Warning: Could not find workspace\n"
      | Ok workspace -> (
          match Server_manager.ensure_running ~workspace with
          | Ok _ -> log "Tusk server is running"
          | Error _ ->
              log "Could not ensure tusk server is running";
              Printf.eprintf
                "[MCP] Warning: Could not ensure tusk server is running\n")
    with exn ->
      let msg =
        Printf.sprintf "Could not ensure tusk server is running: %s"
          (Printexc.to_string exn)
      in
      log msg;
      Printf.eprintf "[MCP] Warning: %s\n" msg
  in

  (* Start the MCP server loop *)
  server_loop ()
