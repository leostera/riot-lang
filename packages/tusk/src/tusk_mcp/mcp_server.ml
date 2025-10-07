(** MCP (Model Context Protocol) server for tusk build system *)

open Std
open Std.Data
open Miniriot
open Core
open Model
open Server

type ctx = { client : Tusk_jsonrpc.Client.t }

module TuskMcp = struct
  type tool_request =
    | Build of { package : string option }
    | GetWorkspace
    | GetGraph
    | GetPackage of { name : string }
    | FindExecutable of { name : string }
    | FindArtifact of { package : string; name : string }

  type tool_response =
    | BuildResult of { messages : string list }
    | WorkspaceInfo of { json : string }
    | GraphInfo of { json : string }
    | PackageInfo of { json : string }
    | ExecutableInfo of { json : string }
    | ArtifactInfo of { json : string }
    | Error of { message : string }

  let tools =
    let open Mcp in
    [
      {
        name = "tusk.build";
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
        name = "tusk.workspace";
        description = Some "Get workspace configuration and package information";
        input_schema =
          Json.Object
            [ ("type", Json.String "object"); ("properties", Json.Object []) ];
      };
      {
        name = "tusk.graph";
        description = Some "Get the build dependency graph";
        input_schema =
          Json.Object
            [ ("type", Json.String "object"); ("properties", Json.Object []) ];
      };
      {
        name = "tusk.package";
        description =
          Some
            "Get detailed information about a specific package including \
             sources";
        input_schema =
          Json.Object
            [
              ("type", Json.String "object");
              ( "properties",
                Json.Object
                  [
                    ( "name",
                      Json.Object
                        [
                          ("type", Json.String "string");
                          ("description", Json.String "Package name to query");
                        ] );
                  ] );
              ("required", Json.Array [ Json.String "name" ]);
            ];
      };
      {
        name = "tusk.findExecutable";
        description = Some "Find a binary by name (owner package and binary)";
        input_schema =
          Json.Object
            [
              ("type", Json.String "object");
              ( "properties",
                Json.Object
                  [
                    ( "name",
                      Json.Object
                        [
                          ("type", Json.String "string");
                          ("description", Json.String "Binary name to lookup");
                        ] );
                  ] );
              ("required", Json.Array [ Json.String "name" ]);
            ];
      };
      {
        name = "tusk.findArtifact";
        description = Some "Find the artifact path for a built binary";
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
                          ("description", Json.String "Owning package name");
                        ] );
                    ( "name",
                      Json.Object
                        [
                          ("type", Json.String "string");
                          ("description", Json.String "Binary name");
                        ] );
                  ] );
              ("required", Json.Array [ Json.String "package"; Json.String "name" ]);
            ];
      };
    ]

  let resources =
    let open Mcp in
    [
      {
        uri = "tusk://workspace";
        name = Some "Workspace Information";
        description = Some "Current workspace configuration and packages";
        mime_type = Some "application/json";
      };
      {
        uri = "tusk://graph";
        name = Some "Build Graph";
        description = Some "Dependency graph for all packages";
        mime_type = Some "application/json";
      };
    ]

  type request =
    | Initialize of {
        protocol_version : string;
        capabilities : Mcp.client_capabilities;
        client_info : Mcp.client_info;
      }
    | Initialized
    | ListTools
    | CallTool of tool_request
    | ListResources
    | ReadResource of { uri : string }
    | Ping
    | Shutdown

  type response =
    | InitializeResult of {
        protocol_version : string;
        capabilities : Mcp.server_capabilities;
        server_info : Mcp.server_info;
        instructions : string option;
      }
    | InitializedResult
    | ListToolsResult of { tools : Mcp.tool list }
    | CallToolResult of tool_response
    | ListResourcesResult of { resources : Mcp.resource list }
    | ReadResourceResult of { contents : Mcp.resource_contents list }
    | PingResult
    | ShutdownResult
    | Error of string

  let request_to_params = function
    | Initialize _ -> { Jsonrpc.method_ = "initialize"; params = NoParams }
    | Initialized -> { method_ = "initialized"; params = NoParams }
    | ListTools -> { method_ = "tools/list"; params = NoParams }
    | CallTool _ -> { method_ = "tools/call"; params = NoParams }
    | ListResources -> { method_ = "resources/list"; params = NoParams }
    | ReadResource { uri } ->
        {
          method_ = "resources/read";
          params = Named [ ("uri", Json.String uri) ];
        }
    | Ping -> { method_ = "ping"; params = NoParams }
    | Shutdown -> { method_ = "shutdown"; params = NoParams }

  let request_of_params method_ params =
    match method_ with
    | "initialize" -> Ok Initialized
    | "initialized" -> Ok Initialized
    | "tools/list" -> Ok ListTools
    | "tools/call" -> (
        match params with
        | Jsonrpc.Named fields -> (
            let name =
              match List.assoc_opt "name" fields with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let arguments = List.assoc_opt "arguments" fields in
            match name with
            | "tusk.build" ->
                let package =
                  match arguments with
                  | Some (Json.Object f) -> (
                      match List.assoc_opt "package" f with
                      | Some (Json.String pkg) -> Some pkg
                      | _ -> None)
                  | _ -> None
                in
                Ok (CallTool (Build { package }))
            | "tusk.workspace" -> Ok (CallTool GetWorkspace)
            | "tusk.graph" -> Ok (CallTool GetGraph)
            | "tusk.package" -> (
                match arguments with
                | Some (Json.Object f) -> (
                    match List.assoc_opt "name" f with
                    | Some (Json.String pkg_name) ->
                        Ok (CallTool (GetPackage { name = pkg_name }))
                    | _ -> Error (Json.String "Missing 'name' parameter"))
                | _ -> Error (Json.String "Missing arguments for tusk.package"))
            | "tusk.findExecutable" -> (
                match arguments with
                | Some (Json.Object f) -> (
                    match List.assoc_opt "name" f with
                    | Some (Json.String name) ->
                        Ok (CallTool (FindExecutable { name }))
                    | _ -> Error (Json.String "Missing 'name' parameter"))
                | _ -> Error (Json.String "Missing arguments for tusk.findExecutable"))
            | "tusk.findArtifact" -> (
                match arguments with
                | Some (Json.Object f) -> (
                    match (List.assoc_opt "package" f, List.assoc_opt "name" f) with
                    | Some (Json.String package), Some (Json.String name) ->
                        Ok (CallTool (FindArtifact { package; name }))
                    | _ ->
                        Error
                          (Json.String
                             "Missing 'package' or 'name' parameter"))
                | _ -> Error (Json.String "Missing arguments for tusk.findArtifact"))
            | _ -> Error (Json.String (format "Unknown tool: %s" name)))
        | _ -> Error (Json.String "tools/call requires named parameters"))
    | "resources/list" -> Ok ListResources
    | "resources/read" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match List.assoc_opt "uri" fields with
            | Some (Json.String uri) -> Ok (ReadResource { uri })
            | _ -> Error (Json.String "Missing uri parameter"))
        | _ -> Error (Json.String "resources/read requires named parameters"))
    | "ping" -> Ok Ping
    | "shutdown" -> Ok Shutdown
    | _ -> Error (Json.String (format "Unknown method: %s" method_))

  let response_to_json = function
    | InitializeResult
        { protocol_version; capabilities; server_info; instructions } ->
        Json.Object
          [
            ("protocolVersion", Json.String protocol_version);
            ( "serverInfo",
              Json.Object
                [
                  ("name", Json.String server_info.Mcp.name);
                  ("version", Json.String server_info.version);
                ] );
            ("capabilities", Json.Object []);
          ]
    | InitializedResult -> Json.Object []
    | ListToolsResult { tools } ->
        Json.Object
          [
            ( "tools",
              Json.Array
                (List.map
                   (fun (t : Mcp.tool) ->
                     Json.Object
                       [
                         ("name", Json.String t.name);
                         ( "description",
                           match t.description with
                           | Some d -> Json.String d
                           | None -> Json.Null );
                         ("inputSchema", t.input_schema);
                       ])
                   tools) );
          ]
    | CallToolResult resp -> (
        match resp with
        | BuildResult { messages } ->
            Json.Object
              [
                ( "content",
                  Json.Array (List.map (fun msg -> Json.String msg) messages) );
                ("isError", Json.Bool false);
              ]
        | WorkspaceInfo { json } ->
            Json.Object
              [
                ("content", Json.Array [ Json.String json ]);
                ("isError", Json.Bool false);
              ]
        | GraphInfo { json } ->
            Json.Object
              [
                ("content", Json.Array [ Json.String json ]);
                ("isError", Json.Bool false);
              ]
        | PackageInfo { json } ->
            Json.Object
              [
                ("content", Json.Array [ Json.String json ]);
                ("isError", Json.Bool false);
              ]
        | ExecutableInfo { json } ->
            Json.Object
              [
                ("content", Json.Array [ Json.String json ]);
                ("isError", Json.Bool false);
              ]
        | ArtifactInfo { json } ->
            Json.Object
              [
                ("content", Json.Array [ Json.String json ]);
                ("isError", Json.Bool false);
              ]
        | Error { message } ->
            Json.Object
              [
                ("content", Json.Array [ Json.String message ]);
                ("isError", Json.Bool true);
              ])
    | ListResourcesResult { resources } ->
        Json.Object
          [
            ( "resources",
              Json.Array
                (List.map
                   (fun (r : Mcp.resource) ->
                     Json.Object
                       [
                         ("uri", Json.String r.uri);
                         ( "name",
                           match r.name with
                           | Some n -> Json.String n
                           | None -> Json.Null );
                       ])
                   resources) );
          ]
    | ReadResourceResult { contents } ->
        Json.Object [ ("contents", Json.Array []) ]
    | PingResult -> Json.Object []
    | ShutdownResult -> Json.Object []
    | Error msg -> Json.Object [ ("error", Json.String msg) ]

  let response_of_json _json : (response, Json.t) result =
    Error (Json.String "Not implemented")
end

let execute_tool (ctx : ctx) (req : TuskMcp.tool_request) :
    TuskMcp.tool_response =
  match req with
  | TuskMcp.Build { package } ->
      TuskMcp.Error { message = "Build not implemented yet" }
  | TuskMcp.GetWorkspace ->
      TuskMcp.Error { message = "GetWorkspace not implemented yet" }
  | TuskMcp.GetGraph ->
      TuskMcp.Error { message = "GetGraph not implemented yet" }
  | TuskMcp.GetPackage { name } ->
      TuskMcp.Error { message = "GetPackage not implemented yet" }
  | TuskMcp.FindExecutable { name } -> (
      match Server.Tusk_jsonrpc.Client.find_executable ctx.client name with
      | Ok (Some (package, binary)) ->
          let json =
            Json.Object
              [
                ("type", Json.String "found_executable");
                ("package", Json.String package);
                ("binary", Json.String binary);
              ]
            |> Json.to_string
          in
          TuskMcp.ExecutableInfo { json }
      | Ok None ->
          let json = Json.Object [ ("type", Json.String "executable_not_found") ] |> Json.to_string in
          TuskMcp.ExecutableInfo { json }
      | Error msg -> TuskMcp.Error { message = msg })
  | TuskMcp.FindArtifact { package; name } -> (
      match Server.Tusk_jsonrpc.Client.find_artifact ctx.client ~package ~kind:"binary" ~name with
      | Ok path ->
          let json =
            Json.Object [ ("type", Json.String "artifact_found"); ("path", Json.String path) ]
            |> Json.to_string
          in
          TuskMcp.ArtifactInfo { json }
      | Error msg ->
          let json =
            Json.Object [ ("type", Json.String "artifact_not_found"); ("error", Json.String msg) ]
            |> Json.to_string
          in
          TuskMcp.ArtifactInfo { json })

let create_server (ctx : ctx) =
  let methods =
    Jsonrpc.Server.
      [
        {
          method_ = "initialize";
          fn =
            (fun reply _req ->
              reply
                (TuskMcp.InitializeResult
                   {
                     protocol_version = "2024-11-05";
                     capabilities =
                       {
                         Mcp.tools = Some ();
                         resources =
                           Some { subscribe = None; list_changed = None };
                         prompts = None;
                       };
                     server_info = { name = "tusk-mcp"; version = "0.1.0" };
                     instructions =
                       Some
                         "Tusk MCP server provides tools and resources for \
                          building OCaml projects";
                   }));
        };
        {
          method_ = "initialized";
          fn = (fun reply _req -> reply TuskMcp.InitializedResult);
        };
        {
          method_ = "tools/list";
          fn =
            (fun reply _req ->
              reply (TuskMcp.ListToolsResult { tools = TuskMcp.tools }));
        };
        {
          method_ = "tools/call";
          fn =
            (fun reply req ->
              match req with
              | TuskMcp.CallTool tool_req ->
                  let tool_resp = execute_tool ctx tool_req in
                  reply (TuskMcp.CallToolResult tool_resp)
              | _ -> ());
        };
        {
          method_ = "resources/list";
          fn =
            (fun reply _req ->
              reply
                (TuskMcp.ListResourcesResult { resources = TuskMcp.resources }));
        };
        {
          method_ = "resources/read";
          fn =
            (fun reply req ->
              match req with
              | TuskMcp.ReadResource { uri } ->
                  reply
                    (TuskMcp.ReadResourceResult
                       {
                         contents =
                           [ Mcp.TextContent { text = uri; mime_type = None } ];
                       })
              | _ -> ());
        };
        { method_ = "ping"; fn = (fun reply _req -> reply TuskMcp.PingResult) };
        {
          method_ = "shutdown";
          fn =
            (fun reply _req ->
              Log.info "[MCP] Shutdown requested";
              reply TuskMcp.ShutdownResult);
        };
      ]
  in
  Jsonrpc.Server.create ~protocol:(module TuskMcp) ~methods

let start_stdio_server ~client =
  spawn @@ fun () ->
  Log.debug "[MCP] stdio server starting";
  let ctx = { client } in
  let mcp_server = create_server ctx in

  let stdin_file = Fs.File.from_fd IO.stdin in

  let rec server_loop () =
    match Fs.File.read_line stdin_file with
    | Ok line ->
        let reply msg = println "%s" msg in
        Jsonrpc.Server.handle_message mcp_server reply line;
        server_loop ()
    | Error _ ->
        Log.info "[MCP] Connection closed";
        ()
  in

  Ok (server_loop ())

let start () =
  Log.set_level Log.Info;
  Log.info "[MCP] Tusk MCP Server starting...";
  Log.info "[MCP] Listening on stdin/stdout for JSON-RPC messages";

  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  let client =
    Server_manager.ensure_running ~workspace
    |> Result.expect ~msg:"Failed to connect to server"
  in

  start_stdio_server ~client
