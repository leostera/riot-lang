(** MCP (Model Context Protocol) server for tusk build system *)

open Miniriot

(** Available tools *)
let tools = [
  {
    Mcp.name = "build";
    description = Some "Build packages in the workspace";
    input_schema = Json.Object [
      ("type", Json.String "object");
      ("properties", Json.Object [
        ("package", Json.Object [
          ("type", Json.String "string");
          ("description", Json.String "Package name to build (optional, builds all if not specified)");
        ]);
      ]);
    ];
  };
  {
    Mcp.name = "clean";
    description = Some "Clean build artifacts";
    input_schema = Json.Object [
      ("type", Json.String "object");
      ("properties", Json.Object []);
    ];
  };
  {
    Mcp.name = "run";
    description = Some "Run a binary";
    input_schema = Json.Object [
      ("type", Json.String "object");
      ("properties", Json.Object [
        ("binary", Json.Object [
          ("type", Json.String "string");
          ("description", Json.String "Binary name to run (optional)");
        ]);
      ]);
    ];
  };
  {
    Mcp.name = "workspace_info";
    description = Some "Get workspace information";
    input_schema = Json.Object [
      ("type", Json.String "object");
      ("properties", Json.Object []);
    ];
  };
  {
    Mcp.name = "build_graph";
    description = Some "Get the build dependency graph";
    input_schema = Json.Object [
      ("type", Json.String "object");
      ("properties", Json.Object []);
    ];
  };
]

(** Available resources *)
let resources = [
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
  | "build" -> (
      let package = match arguments with
        | Some (Json.Object fields) -> (
            match List.assoc_opt "package" fields with
            | Some (Json.String pkg) -> Some pkg
            | _ -> None
          )
        | _ -> None
      in
      (* Call tusk build via RPC *)
      let request = match package with
        | Some pkg -> Rpc_json.BuildPackage pkg
        | None -> Rpc_json.BuildAll
      in
      match Rpc_json_client.call request with
      | Ok (Rpc_json.Success) ->
          [Mcp.Text "Build started successfully"]
      | Ok response ->
          let json = Rpc_json.response_to_json response in
          [Mcp.Text (Json.to_string json)]
      | Error e ->
          [Mcp.Text (Printf.sprintf "Build failed: %s" e)]
    )
  | "clean" -> (
      (* Run clean command *)
      let result = System.system "rm -rf ./target" in
      match result with
      | Unix.WEXITED 0 ->
          [Mcp.Text "Build artifacts cleaned successfully"]
      | _ ->
          [Mcp.Text "Failed to clean build artifacts"]
    )
  | "workspace_info" -> (
      match Rpc_json_client.call Rpc_json.GetWorkspaceConfig with
      | Ok (Rpc_json.WorkspaceConfig config) ->
          let json = Json.Object [
            ("workspace_root", Json.String config.workspace_root);
            ("toolchain", Json.String config.toolchain);
            ("packages", Json.Array (List.map (fun p -> Json.String p) config.packages));
          ] in
          [Mcp.Text (Json.to_string json)]
      | Ok response ->
          let json = Rpc_json.response_to_json response in
          [Mcp.Text (Json.to_string json)]
      | Error e ->
          [Mcp.Text (Printf.sprintf "Failed to get workspace info: %s" e)]
    )
  | "build_graph" -> (
      match Rpc_json_client.call Rpc_json.GetBuildGraph with
      | Ok (Rpc_json.BuildGraph graph) ->
          let nodes_json = List.map (fun node ->
            Json.Object [
              ("package", Json.String node.Rpc_json.package_name);
              ("src_dir", Json.String node.src_dir);
              ("out_dir", Json.String node.out_dir);
              ("status", Json.String node.status);
              ("deps", Json.Array (List.map (fun d -> Json.String d) node.deps));
            ]
          ) graph.nodes in
          let json = Json.Object [
            ("nodes", Json.Array nodes_json);
          ] in
          [Mcp.Text (Json.to_string json)]
      | Ok response ->
          let json = Rpc_json.response_to_json response in
          [Mcp.Text (Json.to_string json)]
      | Error e ->
          [Mcp.Text (Printf.sprintf "Failed to get build graph: %s" e)]
    )
  | "run" -> (
      let binary = match arguments with
        | Some (Json.Object fields) -> (
            match List.assoc_opt "binary" fields with
            | Some (Json.String bin) -> Some bin
            | _ -> None
          )
        | _ -> None
      in
      (* For now, just return info about running *)
      let msg = match binary with
        | Some bin -> Printf.sprintf "Would run binary: %s" bin
        | None -> "Would run default binary"
      in
      [Mcp.Text msg]
    )
  | _ ->
      [Mcp.Text (Printf.sprintf "Unknown tool: %s" name)]

(** Read a resource *)
let read_resource uri =
  match uri with
  | "workspace://info" -> (
      match Rpc_json_client.call Rpc_json.GetWorkspaceConfig with
      | Ok (Rpc_json.WorkspaceConfig config) ->
          let json = Json.Object [
            ("workspace_root", Json.String config.workspace_root);
            ("toolchain", Json.String config.toolchain);
            ("packages", Json.Array (List.map (fun p -> Json.String p) config.packages));
          ] in
          [Mcp.TextContent { text = Json.to_string json; mime_type = Some "application/json" }]
      | _ ->
          [Mcp.TextContent { text = "Failed to get workspace info"; mime_type = None }]
    )
  | "build://graph" -> (
      match Rpc_json_client.call Rpc_json.GetBuildGraph with
      | Ok (Rpc_json.BuildGraph graph) ->
          let nodes_json = List.map (fun node ->
            Json.Object [
              ("package", Json.String node.Rpc_json.package_name);
              ("deps", Json.Array (List.map (fun d -> Json.String d) node.deps));
            ]
          ) graph.nodes in
          let json = Json.Object [("nodes", Json.Array nodes_json)] in
          [Mcp.TextContent { text = Json.to_string json; mime_type = Some "application/json" }]
      | _ ->
          [Mcp.TextContent { text = "Failed to get build graph"; mime_type = None }]
    )
  | "build://status" ->
      [Mcp.TextContent { text = "{\"status\": \"ready\"}"; mime_type = Some "application/json" }]
  | _ ->
      [Mcp.TextContent { text = Printf.sprintf "Unknown resource: %s" uri; mime_type = None }]

(** Handle a request *)
let handle_request (req : Mcp.request) =
  match req.params with
  | Some (Mcp.InitializeParams params) ->
      Printf.printf "[MCP] Client: %s v%s\n" 
        params.client_info.name 
        params.client_info.version;
      Printf.printf "[MCP] Protocol version: %s\n" params.protocol_version;
      
      Mcp.make_success req.id (Mcp.InitializeResult {
        protocol_version = "2024-11-05";
        capabilities = {
          tools = Some ();
          resources = Some { subscribe = None; list_changed = None };
          prompts = None;
        };
        server_info = {
          name = "tusk-mcp";
          version = "0.1.0";
        };
        instructions = Some "Tusk MCP server provides tools and resources for building OCaml projects";
      })
  
  | Some Mcp.InitializedParams ->
      Printf.printf "[MCP] Client initialized\n";
      Mcp.make_success req.id Mcp.InitializedResult
  
  | Some Mcp.ListToolsParams ->
      Mcp.make_success req.id (Mcp.ListToolsResult {
        tools = tools;
        next_cursor = None;
      })
  
  | Some (Mcp.CallToolParams { name; arguments }) ->
      let content = execute_tool name arguments in
      Mcp.make_success req.id (Mcp.CallToolResult {
        content = content;
        is_error = None;
      })
  
  | Some Mcp.ListResourcesParams ->
      Mcp.make_success req.id (Mcp.ListResourcesResult {
        resources = resources;
        next_cursor = None;
      })
  
  | Some (Mcp.ReadResourceParams { uri }) ->
      let contents = read_resource uri in
      Mcp.make_success req.id (Mcp.ReadResourceResult {
        contents = contents;
      })
  
  | Some Mcp.PingParams ->
      Mcp.make_success req.id Mcp.PingResult
  
  | Some Mcp.ShutdownParams ->
      Printf.printf "[MCP] Shutting down\n";
      Mcp.make_success req.id Mcp.ShutdownResult
  
  | _ ->
      Mcp.make_error req.id Mcp.method_not_found "Method not found"

(** MCP server main loop *)
let rec server_loop () =
  (* Read from stdin *)
  try
    let line = input_line stdin in
    (* Parse JSON-RPC request *)
    match Json.of_string line with
    | Ok json -> (
        (* For now, we'll handle requests manually since deserialization isn't implemented *)
        match json with
        | Json.Object fields -> (
            let jsonrpc = match List.assoc_opt "jsonrpc" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let id = match List.assoc_opt "id" fields with
              | Some (Json.String s) -> Mcp.String s
              | Some (Json.Int n) -> Mcp.Number n
              | Some (Json.Float f) -> Mcp.Number (int_of_float f)
              | _ -> Mcp.String "0"
            in
            let method_str = match List.assoc_opt "method" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            
            (* Create a simplified request *)
            let params = match method_str with
              | "initialize" -> (
                  match List.assoc_opt "params" fields with
                  | Some (Json.Object p) ->
                      let protocol_version = match List.assoc_opt "protocolVersion" p with
                        | Some (Json.String s) -> s
                        | _ -> "2024-11-05"
                      in
                      let client_info = match List.assoc_opt "clientInfo" p with
                        | Some (Json.Object ci) ->
                            let name = match List.assoc_opt "name" ci with
                              | Some (Json.String s) -> s
                              | _ -> "unknown"
                            in
                            let version = match List.assoc_opt "version" ci with
                              | Some (Json.String s) -> s
                              | _ -> "0.0.0"
                            in
                            { Mcp.name; version }
                        | _ -> { Mcp.name = "unknown"; version = "0.0.0" }
                      in
                      Some (Mcp.InitializeParams {
                        protocol_version;
                        capabilities = {
                          tools = None;
                          resources = None;
                          prompts = None;
                          sampling = None;
                        };
                        client_info = {
                          name = "unknown";
                          version = "0.0.0";
                        };
                      })
                  | _ -> None
                )
              | "initialized" -> Some Mcp.InitializedParams
              | "tools/list" -> Some Mcp.ListToolsParams
              | "tools/call" -> (
                  match List.assoc_opt "params" fields with
                  | Some (Json.Object p) ->
                      let name = match List.assoc_opt "name" p with
                        | Some (Json.String s) -> s
                        | _ -> ""
                      in
                      let arguments = List.assoc_opt "arguments" p in
                      Some (Mcp.CallToolParams { name; arguments })
                  | _ -> None
                )
              | "resources/list" -> Some Mcp.ListResourcesParams
              | "resources/read" -> (
                  match List.assoc_opt "params" fields with
                  | Some (Json.Object p) ->
                      let uri = match List.assoc_opt "uri" p with
                        | Some (Json.String s) -> s
                        | _ -> ""
                      in
                      Some (Mcp.ReadResourceParams { uri })
                  | _ -> None
                )
              | "ping" -> Some Mcp.PingParams
              | "shutdown" -> Some Mcp.ShutdownParams
              | _ -> None
            in
            
            let request = {
              Mcp.jsonrpc = jsonrpc;
              id = id;
              method_name = method_str;
              params = params;
            } in
            
            let response = handle_request request in
            let response_json = Mcp.response_to_json response in
            Printf.printf "%s\n%!" (Json.to_string response_json);
            
            (* Continue unless shutdown *)
            if method_str <> "shutdown" then
              server_loop ()
            else
              ()
          )
        | _ ->
            Printf.eprintf "[MCP] Invalid JSON-RPC request\n";
            server_loop ()
      )
    | Error e ->
        Printf.eprintf "[MCP] JSON parse error: %s\n" e;
        server_loop ()
  with
  | End_of_file ->
      Printf.eprintf "[MCP] Connection closed\n"
  | exn ->
      Printf.eprintf "[MCP] Error: %s\n" (Printexc.to_string exn);
      server_loop ()

(** Start the MCP server *)
let start () =
  Printf.eprintf "[MCP] Tusk MCP Server starting...\n";
  Printf.eprintf "[MCP] Listening on stdin/stdout for JSON-RPC messages\n";
  
  (* Ensure tusk server is running *)
  let _ = 
    try
      ignore (Server_manager.ensure_running ());
      ()
    with _ ->
      Printf.eprintf "[MCP] Warning: Could not ensure tusk server is running\n"
  in
  
  (* Start the MCP server loop *)
  server_loop ()