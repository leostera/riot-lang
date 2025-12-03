(** MCP (Model Context Protocol) server for tusk build system *)

open Std
open Std.Data

open Tusk_model

type ctx = { client : Tusk_client.t }

module TuskMcp = struct
  type tool_request =
    | Build of Tools.Build.request
    | GetWorkspace of Tools.Describe_workspace.request
    | GetGraph of Tools.Get_package_graph.request
    | GetPackage of Tools.Describe_package.request
    | FindExecutable of Tools.Find_executable.request
    | FindArtifact of Tools.Find_artifact.request
    | CreatePackage of Tools.Create_package.request
    | CreateModule of Tools.Create_module.request
    | FormatFile of Tools.Format_file.request
    | FormatCode of Tools.Format_code.request

  type tool_response =
    | BuildResult of Tools.Build.response
    | WorkspaceResult of Tools.Describe_workspace.response
    | GraphResult of Tools.Get_package_graph.response
    | PackageResult of Tools.Describe_package.response
    | FindExecutableResult of Tools.Find_executable.response
    | FindArtifactResult of Tools.Find_artifact.response
    | CreatePackageResult of Tools.Create_package.response
    | CreateModuleResult of Tools.Create_module.response
    | FormatFileResult of Tools.Format_file.response
    | FormatCodeResult of Tools.Format_code.response
    | Error of { message : string }

  let tools = Tools.all_tools ()

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
            | "build" ->
                let package =
                  match arguments with
                  | Some (Json.Object f) -> (
                      match List.assoc_opt "package" f with
                      | Some (Json.String pkg) -> Some pkg
                      | _ -> None)
                  | _ -> None
                in
                Ok (CallTool (Build { package }))
            | "describeWorkspace" -> Ok (CallTool (GetWorkspace ()))
            | "getPackageGraph" -> Ok (CallTool (GetGraph ()))
            | "describePackage" -> (
                match arguments with
                | Some (Json.Object f) -> (
                    match List.assoc_opt "name" f with
                    | Some (Json.String pkg_name) ->
                        Ok (CallTool (GetPackage { name = pkg_name }))
                    | _ -> Error (Json.String "Missing 'name' parameter"))
                | _ ->
                    Error (Json.String "Missing arguments for describePackage"))
            | "findExecutable" -> (
                match arguments with
                | Some (Json.Object f) -> (
                    match List.assoc_opt "name" f with
                    | Some (Json.String name) ->
                        Ok (CallTool (FindExecutable { name }))
                    | _ -> Error (Json.String "Missing 'name' parameter"))
                | _ ->
                    Error (Json.String "Missing arguments for findExecutable"))
            | "findArtifact" -> (
                match arguments with
                | Some (Json.Object f) -> (
                    match
                      (List.assoc_opt "package" f, List.assoc_opt "name" f)
                    with
                    | Some (Json.String package), Some (Json.String name) ->
                        Ok (CallTool (FindArtifact { package; name }))
                    | _ ->
                        Error
                          (Json.String "Missing 'package' or 'name' parameter"))
                | _ -> Error (Json.String "Missing arguments for findArtifact"))
            | "createPackage" -> (
                match arguments with
                | Some (Json.Object f) ->
                    let name =
                      match List.assoc_opt "name" f with
                      | Some (Json.String s) -> s
                      | _ -> ""
                    in
                    let deps =
                      match List.assoc_opt "deps" f with
                      | Some (Json.Array arr) ->
                          List.filter_map
                            (function Json.String s -> Some s | _ -> None)
                            arr
                      | _ -> []
                    in
                    let is_library =
                      match List.assoc_opt "is_library" f with
                      | Some (Json.Bool b) -> b
                      | _ -> true
                    in
                    Ok (CallTool (CreatePackage { name; deps; is_library }))
                | _ -> Error (Json.String "Missing arguments for createPackage")
                )
            | "createModule" -> (
                match arguments with
                | Some (Json.Object f) ->
                    let package =
                      match List.assoc_opt "package" f with
                      | Some (Json.String s) -> s
                      | _ -> ""
                    in
                    let module_name =
                      match List.assoc_opt "module_name" f with
                      | Some (Json.String s) -> s
                      | _ -> ""
                    in
                    let contents =
                      match List.assoc_opt "contents" f with
                      | Some (Json.String s) -> s
                      | _ -> "open Std\n"
                    in
                    Ok
                      (CallTool
                         (CreateModule { package; module_name; contents }))
                | _ -> Error (Json.String "Missing arguments for createModule"))
            | "formatFile" -> (
                match arguments with
                | Some (Json.Object f) ->
                    let file_path =
                      match List.assoc_opt "file_path" f with
                      | Some (Json.String s) -> s
                      | _ -> ""
                    in
                    let check_only =
                      match List.assoc_opt "check_only" f with
                      | Some (Json.Bool b) -> b
                      | _ -> false
                    in
                    Ok (CallTool (FormatFile { file_path; check_only }))
                | _ -> Error (Json.String "Missing arguments for formatFile"))
            | "formatCode" -> (
                match arguments with
                | Some (Json.Object f) ->
                    let code =
                      match List.assoc_opt "code" f with
                      | Some (Json.String s) -> s
                      | _ -> ""
                    in
                    let file_path =
                      match List.assoc_opt "file_path" f with
                      | Some (Json.String s) -> Some s
                      | _ -> None
                    in
                    Ok (CallTool (FormatCode { code; file_path }))
                | _ -> Error (Json.String "Missing arguments for formatCode"))
            | _ -> Error (Json.String ("Unknown tool: " ^ name)))
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
    | _ -> Error (Json.String ("Unknown method: " ^ method_))

  let response_to_json = function
    | InitializeResult
        { protocol_version; capabilities; server_info; instructions } ->
        let fields =
          [
            ("protocolVersion", Json.String protocol_version);
            ( "serverInfo",
              Json.Object
                [
                  ("name", Json.String server_info.Mcp.name);
                  ("version", Json.String server_info.version);
                ] );
            ( "capabilities",
              let caps_fields = [] in
              let caps_fields =
                match capabilities.Mcp.tools with
                | None -> caps_fields
                | Some _ -> caps_fields @ [ ("tools", Json.Object []) ]
              in
              let caps_fields =
                match capabilities.resources with
                | None -> caps_fields
                | Some rc ->
                    caps_fields
                    @ [
                        ( "resources",
                          Json.Object
                            [
                              ( "subscribe",
                                match rc.subscribe with
                                | None -> Json.Bool false
                                | Some b -> Json.Bool b );
                              ( "listChanged",
                                match rc.list_changed with
                                | None -> Json.Bool false
                                | Some b -> Json.Bool b );
                            ] );
                      ]
              in
              let caps_fields =
                match capabilities.prompts with
                | None -> caps_fields
                | Some _ -> caps_fields @ [ ("prompts", Json.Object []) ]
              in
              Json.Object caps_fields );
          ]
        in
        let fields =
          match instructions with
          | Some instr -> fields @ [ ("instructions", Json.String instr) ]
          | None -> fields
        in
        Json.Object fields
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
        | BuildResult r -> Tools.Build.response_to_json r
        | WorkspaceResult r -> Tools.Describe_workspace.response_to_json r
        | GraphResult r -> Tools.Get_package_graph.response_to_json r
        | PackageResult r -> Tools.Describe_package.response_to_json r
        | FindExecutableResult r -> Tools.Find_executable.response_to_json r
        | FindArtifactResult r -> Tools.Find_artifact.response_to_json r
        | CreatePackageResult r -> Tools.Create_package.response_to_json r
        | CreateModuleResult r -> Tools.Create_module.response_to_json r
        | FormatFileResult r -> Tools.Format_file.response_to_json r
        | FormatCodeResult r -> Tools.Format_code.response_to_json r
        | Error { message } ->
            Json.Object
              [
                ( "content",
                  Json.Array
                    [
                      Json.Object
                        [
                          ("type", Json.String "text");
                          ("text", Json.String message);
                        ];
                    ] );
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
  | TuskMcp.Build build_req ->
      Log.debug "[EXECUTE_TOOL] Calling Build.execute";
      let result = Tools.Build.execute ctx.client build_req in
      Log.debug "[EXECUTE_TOOL] Build.execute returned";
      TuskMcp.BuildResult result
  | TuskMcp.GetWorkspace ws_req ->
      Log.debug "[EXECUTE_TOOL] Calling Describe_workspace.execute";
      let result = Tools.Describe_workspace.execute ctx.client ws_req in
      Log.debug "[EXECUTE_TOOL] Describe_workspace.execute returned";
      TuskMcp.WorkspaceResult result
  | TuskMcp.GetGraph graph_req ->
      Log.debug "[EXECUTE_TOOL] Calling Get_package_graph.execute";
      let result = Tools.Get_package_graph.execute ctx.client graph_req in
      Log.debug "[EXECUTE_TOOL] Get_package_graph.execute returned";
      TuskMcp.GraphResult result
  | TuskMcp.GetPackage pkg_req ->
      Log.debug "[EXECUTE_TOOL] Calling Describe_package.execute";
      let result = Tools.Describe_package.execute ctx.client pkg_req in
      Log.debug "[EXECUTE_TOOL] Describe_package.execute returned";
      TuskMcp.PackageResult result
  | TuskMcp.FindExecutable exec_req ->
      Log.debug "[EXECUTE_TOOL] Calling Find_executable.execute";
      let result = Tools.Find_executable.execute ctx.client exec_req in
      Log.debug "[EXECUTE_TOOL] Find_executable.execute returned";
      TuskMcp.FindExecutableResult result
  | TuskMcp.FindArtifact artifact_req ->
      Log.debug "[EXECUTE_TOOL] Calling Find_artifact.execute";
      let result = Tools.Find_artifact.execute ctx.client artifact_req in
      Log.debug "[EXECUTE_TOOL] Find_artifact.execute returned";
      TuskMcp.FindArtifactResult result
  | TuskMcp.CreatePackage create_pkg_req ->
      Log.debug "[EXECUTE_TOOL] Calling Create_package.execute";
      let result = Tools.Create_package.execute ctx.client create_pkg_req in
      Log.debug "[EXECUTE_TOOL] Create_package.execute returned";
      TuskMcp.CreatePackageResult result
  | TuskMcp.CreateModule create_mod_req ->
      Log.debug "[EXECUTE_TOOL] Calling Create_module.execute";
      let result = Tools.Create_module.execute ctx.client create_mod_req in
      Log.debug "[EXECUTE_TOOL] Create_module.execute returned";
      TuskMcp.CreateModuleResult result
  | TuskMcp.FormatFile format_file_req ->
      Log.debug "[EXECUTE_TOOL] Calling Format_file.execute";
      let result = Tools.Format_file.execute ctx.client format_file_req in
      Log.debug "[EXECUTE_TOOL] Format_file.execute returned";
      TuskMcp.FormatFileResult result
  | TuskMcp.FormatCode format_code_req ->
      Log.debug "[EXECUTE_TOOL] Calling Format_code.execute";
      let result = Tools.Format_code.execute ctx.client format_code_req in
      Log.debug "[EXECUTE_TOOL] Format_code.execute returned";
      TuskMcp.FormatCodeResult result

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
                         {|# Tusk MCP Server - OCaml Build System Interface

## CRITICAL RULES - READ FIRST

### ALWAYS Use Tusk Tools Instead of Shell Commands
**NEVER** use shell commands to inspect or build the workspace. This MCP server provides specialized tools that:
- Are faster and more accurate than shell commands
- Return structured data instead of text output
- Understand the build system's internal state
- Work correctly with tusk's module system and caching

### DO NOT Use These Commands:
- `find` → Use `tusk.package` to see package contents
- `ls` / `tree` → Use `tusk.workspace` to see project structure
- `grep` → Use `tusk.package` to find source files
- `dune build` → Use `tusk.build` to compile packages
- `ocamlc` / `ocamlopt` → NEVER call compilers directly
- Manual file searches → Use the MCP tools

## How Tusk's Build System Works

### Package Structure
- Each package lives in `packages/<name>/`
- Source files go in `packages/<name>/src/`
- Entry point for binaries: `src/main.ml`
- Configuration: `packages/<name>/tusk.toml`

### Module Namespacing
- File `packages/foo/src/bar.ml` creates module `Foo.Bar` (internal name)
- **NEVER reference namespaced names** like `Std__Crypto__Algo__Sha256`
- Always use the clean hierarchical name: `Std.Crypto.Algo.Sha256`

### Subdirectories as Libraries
- Subdirectories in `src/` become sub-libraries
- Example: `packages/tusk/src/cli/build.ml` → `Tusk.Cli.Build`
- The build system auto-generates parent modules for subdirectories
- Files in subdirs are accessible via the parent module (e.g., `Cli.Build`)

### Build System Features
- Uses content-based caching (builds are incremental)
- Automatically manages dependencies between packages
- Compiles only what changed since last build

## Recommended Workflow

### 1. Start Every Task by Understanding the Workspace
```
Call tusk.workspace first to get:
- All packages in the project
- Their locations and dependencies
- The complete project structure
```

### 2. Inspect Specific Packages
```
Call tusk.package with {name: "package_name"} to get:
- All source files in the package
- Package dependencies
- Package configuration
```

### 3. Build Packages
```
Call tusk.build with:
- {package: "name"} → Build specific package + dependencies
- {} → Build entire workspace
```

### 4. Find and Run Binaries
```
Workflow:
1. tusk.findExecutable {name: "binary_name"} → Find which package owns it
2. tusk.build {package: "that_package"} → Build it
3. tusk.findArtifact {package: "that_package", name: "binary_name"} → Get path
4. Use the path to run the binary
```

## Common Mistakes to Avoid

1. **Don't search for files manually** - Use `tusk.package` instead
2. **Don't call build tools directly** - Use `tusk.build` instead
3. **Don't assume package locations** - Use `tusk.workspace` to discover them
4. **Don't reference modules by namespaced names** - Use hierarchical names
5. **Don't modify package sources to fix build logic** - Build system issues require build system fixes

## Tool Selection Guide

- **tusk.workspace** - Start here! Get complete project overview
- **tusk.package** - Inspect a specific package's contents
- **tusk.graph** - Understand package dependencies and build order
- **tusk.build** - Compile code after making changes
- **tusk.findExecutable** - Locate which package owns a binary
- **tusk.findArtifact** - Get filesystem path to a built binary

USE THESE TOOLS EVERY TIME instead of shell commands. They are purpose-built for the tusk workspace and will give you accurate, structured information about the build system's state.|};
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
          fn = (fun reply _req -> reply TuskMcp.ShutdownResult);
        };
      ]
  in
  Jsonrpc.Server.create ~protocol:(module TuskMcp) ~methods

let start_stdio_server ~client =
  let ctx = { client } in
  let mcp_server = create_server ctx in

  let stdin_file = Fs.File.from_fd IO.stdin in

  let rec server_loop () =
    match Fs.File.read_line stdin_file with
    | Ok line -> (
        try
          let reply msg = println msg in
          Jsonrpc.Server.handle_message mcp_server reply line;
          server_loop ()
        with exn ->
          println ("Something went wrong: " ^ Exception.to_string exn);
          server_loop ())
    | Error _ -> ()
  in

  server_loop ()

let start () =
  Log.set_level Log.Trace;

  let cwd =
    Env.current_dir ()
    |> Result.expect ~msg:"Failed to get current directory"
  in
  let (workspace, _load_errors) =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  let client =
    Tusk_server.Server_manager.ensure_running ~workspace ~config:Tusk_server.Server_config.default
    |> Result.expect ~msg:"Failed to connect to server"
  in

  start_stdio_server ~client
