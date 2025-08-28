(** MCP (Model Context Protocol) server for tusk build system *)

open Miniriot
open Std.Data

(** Helper to create a tusk client connected to the local server *)
let create_local_client () =
  let cwd =
    Std.Env.current_dir ()
    |> Std.Result.expect ~msg:"Failed to get current directory"
  in
  match Workspace_manager.scan cwd with
  | Error _ -> Error "Failed to find workspace"
  | Ok workspace -> (
      match Server_manager.ensure_running ~workspace with
      | Ok client -> Ok client
      | Error _ -> Error "Failed to connect to server")

(** Available tools for MCP *)
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
      input_schema = Json.Object [ ("type", Json.String "object") ];
    };
    {
      Mcp.name = "workspace_info";
      description = Some "Get workspace information";
      input_schema = Json.Object [ ("type", Json.String "object") ];
    };
    {
      Mcp.name = "build_graph";
      description = Some "Get the build dependency graph";
      input_schema = Json.Object [ ("type", Json.String "object") ];
    };
    {
      Mcp.name = "format_file";
      description = Some "Format an OCaml source file with ocamlformat";
      input_schema =
        Json.Object
          [
            ("type", Json.String "object");
            ( "properties",
              Json.Object
                [
                  ( "file_path",
                    Json.Object
                      [
                        ("type", Json.String "string");
                        ( "description",
                          Json.String "Path to the OCaml file to format" );
                      ] );
                  ( "check_only",
                    Json.Object
                      [
                        ("type", Json.String "boolean");
                        ( "description",
                          Json.String
                            "If true, only check if formatting is needed \
                             without modifying the file" );
                      ] );
                ] );
            ("required", Json.Array [ Json.String "file_path" ]);
          ];
    };
    {
      Mcp.name = "format_code";
      description = Some "Format OCaml code string with ocamlformat";
      input_schema =
        Json.Object
          [
            ("type", Json.String "object");
            ( "properties",
              Json.Object
                [
                  ( "code",
                    Json.Object
                      [
                        ("type", Json.String "string");
                        ("description", Json.String "OCaml code to format");
                      ] );
                  ( "file_hint",
                    Json.Object
                      [
                        ("type", Json.String "string");
                        ( "description",
                          Json.String
                            "Optional file path hint for determining if code \
                             is .ml or .mli" );
                      ] );
                ] );
            ("required", Json.Array [ Json.String "code" ]);
          ];
    };
  ]

(** Execute a tool and return MCP content items *)
let execute_tool name arguments =
  match name with
  | "build" -> (
      (* Extract package argument if provided *)
      let package =
        match arguments with
        | Some (Json.Object fields) -> (
            match List.assoc_opt "package" fields with
            | Some (Json.String pkg) -> Some pkg
            | _ -> None)
        | _ -> None
      in

      (* Connect to server and build *)
      match create_local_client () with
      | Error e ->
          [
            Mcp.Text (Json.to_string (Json.Object [ ("error", Json.String e) ]));
          ]
      | Ok client -> (
          (* Collect all events as JSON *)
          let events = ref [] in
          let callback = function
            | Tusk_jsonrpc.Client.BuildStarted sid ->
                events :=
                  Json.Object
                    [
                      ("type", Json.String "build_started");
                      ( "timestamp",
                        Json.String (Datetime.to_iso8601 (Datetime.now ())) );
                      ("session_id", Json.String (Session_id.to_string sid));
                    ]
                  :: !events
            | Tusk_jsonrpc.Client.BuildEvent event ->
                (* Convert event to JSON - this includes all build logs *)
                events := Event.to_json event :: !events
            | Tusk_jsonrpc.Client.BuildFinished result ->
                let status_json =
                  match result with
                  | Ok () ->
                      Json.Object
                        [
                          ("type", Json.String "build_completed");
                          ( "timestamp",
                            Json.String (Datetime.to_iso8601 (Datetime.now ()))
                          );
                          ("success", Json.Bool true);
                        ]
                  | Error e ->
                      Json.Object
                        [
                          ("type", Json.String "build_failed");
                          ( "timestamp",
                            Json.String (Datetime.to_iso8601 (Datetime.now ()))
                          );
                          ("success", Json.Bool false);
                          ("error", Json.String e);
                        ]
                in
                events := status_json :: !events
          in

          (* Build the requested package or all *)
          let request =
            match package with
            | Some pkg -> Tusk_jsonrpc.Client.BuildPackage pkg
            | None -> Tusk_jsonrpc.Client.BuildAll
          in

          (* Execute build with streaming *)
          match Tusk_jsonrpc.Client.build_streaming client request callback with
          | Ok _ ->
              (* Return all events as JSON array - same format as rpc build *)
              let all_events = Json.Array (List.rev !events) in
              [ Mcp.Text (Json.to_string all_events) ]
          | Error e ->
              [
                Mcp.Text
                  (Json.to_string (Json.Object [ ("error", Json.String e) ]));
              ]))
  | "clean" -> (
      (* Clean build artifacts *)
      let cwd =
        Std.Env.current_dir ()
        |> Std.Result.expect ~msg:"Failed to get current directory"
      in
      let target_dir = Filename.concat (Std.Path.to_string cwd) "target" in
      let cmd = Printf.sprintf "rm -rf %s" target_dir in
      let result = Std.Command.system cmd in
      match Std.Command.of_unix_status result with
      | Std.Command.Exited 0 ->
          [
            Mcp.Text
              (Json.to_string
                 (Json.Object
                    [
                      ("success", Json.Bool true);
                      ( "message",
                        Json.String "Build artifacts cleaned successfully" );
                    ]));
          ]
      | _ ->
          [
            Mcp.Text
              (Json.to_string
                 (Json.Object
                    [
                      ("success", Json.Bool false);
                      ("message", Json.String "Failed to clean build artifacts");
                    ]));
          ])
  | "workspace_info" -> (
      match create_local_client () with
      | Error e ->
          [
            Mcp.Text (Json.to_string (Json.Object [ ("error", Json.String e) ]));
          ]
      | Ok client -> (
          match Tusk_jsonrpc.Client.get_workspace_config client with
          | Ok config ->
              (* Convert workspace config to JSON *)
              let packages_json =
                List.map
                  (fun (pkg : Tusk_jsonrpc.TuskProtocol.package_info) ->
                    Json.Object
                      [
                        ("name", Json.String pkg.name);
                        ("path", Json.String pkg.path);
                        ( "dependencies",
                          Json.Array
                            (List.map (fun d -> Json.String d) pkg.dependencies)
                        );
                      ])
                  config.packages
              in
              let workspace_json =
                Json.Object
                  [
                    ("workspace_root", Json.String config.workspace_root);
                    ("target_dir", Json.String config.target_dir);
                    ("toolchain", Json.String config.toolchain);
                    ("toolchain_path", Json.String config.toolchain_path);
                    ("packages", Json.Array packages_json);
                    ("total_packages", Json.Int config.total_packages);
                  ]
              in
              [ Mcp.Text (Json.to_string workspace_json) ]
          | Error e ->
              [
                Mcp.Text
                  (Json.to_string (Json.Object [ ("error", Json.String e) ]));
              ]))
  | "build_graph" -> (
      match create_local_client () with
      | Error e ->
          [
            Mcp.Text (Json.to_string (Json.Object [ ("error", Json.String e) ]));
          ]
      | Ok client -> (
          match Tusk_jsonrpc.Client.get_build_graph client with
          | Ok graph ->
              (* Convert build graph to JSON *)
              let nodes_json =
                List.map
                  (fun (node : Tusk_jsonrpc.TuskProtocol.build_node) ->
                    Json.Object
                      [
                        ("package", Json.String node.package_name);
                        ( "dependencies",
                          Json.Array
                            (List.map (fun d -> Json.String d) node.deps) );
                      ])
                  graph.nodes
              in
              let graph_json =
                Json.Object [ ("nodes", Json.Array nodes_json) ]
              in
              [ Mcp.Text (Json.to_string graph_json) ]
          | Error e ->
              [
                Mcp.Text
                  (Json.to_string (Json.Object [ ("error", Json.String e) ]));
              ]))
  | "format_file" -> (
      (* Extract arguments *)
      let file_path, check_only =
        match arguments with
        | Some (Json.Object fields) -> (
            match
              ( List.assoc_opt "file_path" fields,
                List.assoc_opt "check_only" fields )
            with
            | Some (Json.String path), Some (Json.Bool check) -> (path, check)
            | Some (Json.String path), None -> (path, false)
            | _ -> ("", false))
        | _ -> ("", false)
      in

      if file_path = "" then
        [
          Mcp.Text
            (Json.to_string
               (Json.Object [ ("error", Json.String "file_path is required") ]));
        ]
      else
        (* Format the file through the server *)
        match Std.Path.of_string file_path with
        | Error _ ->
            [
              Mcp.Text
                (Json.to_string
                   (Json.Object
                      [
                        ("success", Json.Bool false);
                        ("error", Json.String "Invalid file path");
                      ]));
            ]
        | Ok file_path -> (
            match create_local_client () with
            | Error e ->
                [
                  Mcp.Text
                    (Json.to_string
                       (Json.Object
                          [
                            ("success", Json.Bool false);
                            ("error", Json.String e);
                          ]));
                ]
            | Ok client -> (
                match Tusk_jsonrpc.Client.format_file client ~file_path:(Std.Path.to_string file_path) ~check_only with
                | Ok (formatted_code, changed) ->
                    [
                      Mcp.Text
                        (Json.to_string
                           (Json.Object
                              [
                                ("success", Json.Bool true);
                                ("formatted_code", Json.String formatted_code);
                                ("changed", Json.Bool changed);
                              ]));
                    ]
                | Error error ->
                    [
                      Mcp.Text
                        (Json.to_string
                           (Json.Object
                              [
                                ("success", Json.Bool false);
                                ("error", Json.String error);
                              ]));
                    ])))
  | "format_code" -> (
      (* Extract arguments *)
      let code, file_hint =
        match arguments with
        | Some (Json.Object fields) -> (
            match
              ( List.assoc_opt "code" fields,
                List.assoc_opt "file_hint" fields )
            with
            | Some (Json.String c), Some (Json.String h) ->
                (c, match Std.Path.of_string h with Ok p -> Some p | Error _ -> None)
            | Some (Json.String c), None -> (c, None)
            | _ -> ("", None))
        | _ -> ("", None)
      in

      if code = "" then
        [
          Mcp.Text
            (Json.to_string
               (Json.Object [ ("error", Json.String "code is required") ]));
        ]
      else
        (* Format the code through the server *)
        match create_local_client () with
        | Error e ->
            [
              Mcp.Text
                (Json.to_string
                   (Json.Object
                      [
                        ("success", Json.Bool false);
                        ("error", Json.String e);
                      ]));
            ]
        | Ok client -> (
            let file_hint_str = Option.map Std.Path.to_string file_hint in
            match Tusk_jsonrpc.Client.format_code client ~code ~file_path:file_hint_str with
            | Ok (formatted_code, changed) ->
                [
                  Mcp.Text
                    (Json.to_string
                       (Json.Object
                          [
                            ("success", Json.Bool true);
                            ("formatted_code", Json.String formatted_code);
                            ("changed", Json.Bool changed);
                          ]));
                ]
            | Error error ->
                [
                  Mcp.Text
                    (Json.to_string
                       (Json.Object
                          [
                            ("success", Json.Bool false);
                            ("error", Json.String error);
                          ]));
                ]))
  | _ ->
      [
        Mcp.Text
          (Json.to_string
             (Json.Object
                [
                  ("error", Json.String (Printf.sprintf "Unknown tool: %s" name));
                ]));
      ]

(** MCP Protocol module implementing Jsonrpc.ApplicationProtocol *)
module McpProtocol = struct
  type request =
    | Initialize of {
        protocol_version : string;
        capabilities : Json.t;
        client_info : Json.t;
      }
    | ToolsList
    | ToolsCall of { name : string; arguments : Json.t option }

  type response =
    | InitializeResult of {
        protocol_version : string;
        capabilities : Json.t;
        server_info : Json.t;
      }
    | ToolsListResult of { tools : Json.t }
    | ToolsCallResult of { content : Json.t }
    | Error of string

  let request_of_json json =
    match json with
    | Json.Object fields -> (
        match List.assoc_opt "method" fields with
        | Some (Json.String "initialize") ->
            let params =
              List.assoc_opt "params" fields
              |> Option.value ~default:(Json.Object [])
            in
            let protocol_version =
              match params with
              | Json.Object p -> (
                  match List.assoc_opt "protocolVersion" p with
                  | Some (Json.String v) -> v
                  | _ -> "2024-11-05")
              | _ -> "2024-11-05"
            in
            let capabilities =
              match params with
              | Json.Object p ->
                  List.assoc_opt "capabilities" p
                  |> Option.value ~default:(Json.Object [])
              | _ -> Json.Object []
            in
            let client_info =
              match params with
              | Json.Object p ->
                  List.assoc_opt "clientInfo" p
                  |> Option.value ~default:(Json.Object [])
              | _ -> Json.Object []
            in
            Std.Result.Ok
              (Initialize { protocol_version; capabilities; client_info })
        | Some (Json.String "tools/list") -> Std.Result.Ok ToolsList
        | Some (Json.String "tools/call") -> (
            match List.assoc_opt "params" fields with
            | Some (Json.Object params) -> (
                match List.assoc_opt "name" params with
                | Some (Json.String name) ->
                    let arguments = List.assoc_opt "arguments" params in
                    Std.Result.Ok (ToolsCall { name; arguments })
                | _ -> Std.Result.Error (Json.String "Missing tool name"))
            | _ -> Std.Result.Error (Json.String "Missing tool call parameters")
            )
        | Some (Json.String method_name) ->
            Std.Result.Error
              (Json.String (Printf.sprintf "Unknown method: %s" method_name))
        | _ -> Std.Result.Error (Json.String "Invalid method"))
    | _ -> Std.Result.Error (Json.String "Invalid request format")

  let response_to_json = function
    | InitializeResult { protocol_version; capabilities; server_info } ->
        Json.Object
          [
            ("protocolVersion", Json.String protocol_version);
            ("capabilities", capabilities);
            ("serverInfo", server_info);
          ]
    | ToolsListResult { tools } -> Json.Object [ ("tools", tools) ]
    | ToolsCallResult { content } -> Json.Object [ ("content", content) ]
    | Error msg -> Json.Object [ ("error", Json.String msg) ]

  let response_of_json json =
    (* For MCP we don't parse responses on the server side *)
    Std.Result.Error (Json.String "Response parsing not implemented")

  let request_to_params = function
    | Initialize _ -> { Jsonrpc.method_ = "initialize"; params = NoParams }
    | ToolsList -> { Jsonrpc.method_ = "tools/list"; params = NoParams }
    | ToolsCall { name; arguments } ->
        {
          Jsonrpc.method_ = "tools/call";
          params =
            Named
              [
                ("name", Json.String name);
                ("arguments", Option.value ~default:Json.Null arguments);
              ];
        }

  let request_of_params method_ params =
    match method_ with
    | "initialize" ->
        Std.Result.Ok
          (Initialize
             {
               protocol_version = "2024-11-05";
               capabilities = Json.Object [];
               client_info = Json.Object [];
             })
    | "tools/list" -> Std.Result.Ok ToolsList
    | "tools/call" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match List.assoc_opt "name" fields with
            | Some (Json.String name) ->
                let arguments = List.assoc_opt "arguments" fields in
                Std.Result.Ok (ToolsCall { name; arguments })
            | _ -> Std.Result.Error (Json.String "Missing tool name"))
        | _ -> Std.Result.Error (Json.String "Invalid tools/call parameters"))
    | _ ->
        Std.Result.Error
          (Json.String (Printf.sprintf "Unknown method: %s" method_))
end

(** Create MCP server using Jsonrpc.Server *)
let create_server () =
  let ctx = () in
  let methods =
    [
      {
        Jsonrpc.Server.method_ = "initialize";
        fn =
          (fun reply request ->
            match request with
            | McpProtocol.Initialize
                { protocol_version = _; capabilities = _; client_info = _ } ->
                let response =
                  McpProtocol.InitializeResult
                    {
                      protocol_version = "2024-11-05";
                      capabilities = Json.Object [ ("tools", Json.Object []) ];
                      server_info =
                        Json.Object
                          [
                            ("name", Json.String "tusk-mcp");
                            ("version", Json.String "0.1.0");
                          ];
                    }
                in
                reply response
            | _ -> reply (McpProtocol.Error "Invalid request for initialize"));
      };
      {
        Jsonrpc.Server.method_ = "tools/list";
        fn =
          (fun reply request ->
            match request with
            | McpProtocol.ToolsList ->
                let tools_json =
                  List.map
                    (fun (tool : Mcp.tool) ->
                      Json.Object
                        [
                          ("name", Json.String tool.name);
                          ( "description",
                            match tool.description with
                            | Some d -> Json.String d
                            | None -> Json.Null );
                          ("inputSchema", tool.input_schema);
                        ])
                    tools
                in
                let response =
                  McpProtocol.ToolsListResult { tools = Json.Array tools_json }
                in
                reply response
            | _ -> reply (McpProtocol.Error "Invalid request for tools/list"));
      };
      {
        Jsonrpc.Server.method_ = "tools/call";
        fn =
          (fun reply request ->
            match request with
            | McpProtocol.ToolsCall { name; arguments } ->
                let content = execute_tool name arguments in
                let content_json =
                  List.map
                    (function
                      | Mcp.Text text ->
                          Json.Object
                            [
                              ("type", Json.String "text");
                              ("text", Json.String text);
                            ]
                      | Mcp.Resource _ ->
                          Json.Object [ ("type", Json.String "resource") ])
                    content
                in
                let response =
                  McpProtocol.ToolsCallResult
                    { content = Json.Array content_json }
                in
                reply response
            | _ -> reply (McpProtocol.Error "Invalid request for tools/call"));
      };
    ]
  in
  Jsonrpc.Server.create ~protocol:(module McpProtocol) ~methods

(** Start the MCP server *)
let start () =
  Printf.eprintf "[MCP] Tusk MCP Server starting...\n";
  Printf.eprintf "[MCP] Listening on stdin/stdout for JSON-RPC messages\n";

  (* Ensure tusk server is running *)
  let _ =
    try
      let cwd =
        Std.Env.current_dir () |> Std.Result.expect ~msg:"Failed to get cwd"
      in
      match Workspace_manager.scan cwd with
      | Error _ -> Printf.eprintf "[MCP] Warning: Could not find workspace\n"
      | Ok workspace -> (
          match Server_manager.ensure_running ~workspace with
          | Ok _ -> ()
          | Error _ ->
              Printf.eprintf
                "[MCP] Warning: Could not ensure tusk server is running\n")
    with exn -> Printf.eprintf "[MCP] Warning: %s\n" (Printexc.to_string exn)
  in

  (* Create the JSON-RPC server *)
  let server = create_server () in

  (* Main loop - read from stdin and write to stdout *)
  let rec loop () =
    try
      let line = input_line stdin in
      (* Reply function that sends response to stdout *)
      let reply response_str = Printf.printf "%s\n%!" response_str in
      Jsonrpc.Server.handle_message server reply line;
      loop ()
    with
    | End_of_file -> Printf.eprintf "[MCP] Connection closed\n"
    | exn ->
        Printf.eprintf "[MCP] Error: %s\n" (Printexc.to_string exn);
        loop ()
  in
  loop ()
