open Std
open Std.Data

let description =
  {|Get complete workspace information including all packages, their paths, dependencies, and configuration. Returns structured JSON with the complete project layout.

WHEN TO USE THIS TOOL:
- At the START of any task to understand the codebase layout
- When you need to discover what packages exist in the project
- When you want to understand the project structure
- When you need to find package locations or paths
- When mapping out dependencies between packages
- Before making changes to understand what packages are affected

INSTEAD OF:
- DO NOT use 'find' to search for packages
- DO NOT use 'ls' to list directories  
- DO NOT use 'grep' to search for package names in tusk.toml files
- DO NOT manually traverse directories to discover packages
- DO NOT use 'tree' or other filesystem tools

PARAMETERS:
No parameters required - always returns complete workspace information.

RETURNS:
Returns WorkspaceInfo with JSON containing:
- workspace_root: Absolute path to the workspace root directory
- target_dir: Path to the build output directory (usually target/debug)
- toolchain: OCaml toolchain version being used
- toolchain_path: Path to the OCaml toolchain installation
- packages: Array of all packages, each with:
  - name: Package name (e.g., "tusk", "std", "kernel")
  - path: Relative path from workspace root to package
  - dependencies: List of other package names this package depends on
- total_packages: Total number of packages in workspace

Or returns an Error if workspace scan fails.

EXAMPLES:
- Get all workspace info: {} (no parameters needed)

USE THIS TOOL FIRST when starting work on the codebase to understand its structure. This gives you a complete map of the project without needing to use filesystem commands.|}

let tool =
  let open Mcp in
  let input_schema =
    Json.Object
      [ ("type", Json.String "object"); ("properties", Json.Object []) ]
  in
  { name = "describeWorkspace"; description = Some description; input_schema }

type request = unit
type response = WorkspaceInfo of { json : string } | Error of string

let execute (client : Server.Tusk_jsonrpc.Client.t) (_ : request) : response =
  Log.debug "[WORKSPACE TOOL] execute() called";
  match Server.Tusk_jsonrpc.Client.get_workspace_config client with
  | Ok config ->
      Log.debug "[WORKSPACE TOOL] got workspace config successfully";
      let json =
        Json.Object
          [
            ( "workspace_root",
              Json.String config.Server.Tusk_jsonrpc.TuskProtocol.workspace_root
            );
            ( "target_dir",
              Json.String config.Server.Tusk_jsonrpc.TuskProtocol.target_dir );
            ( "toolchain",
              Json.String config.Server.Tusk_jsonrpc.TuskProtocol.toolchain );
            ( "toolchain_path",
              Json.String config.Server.Tusk_jsonrpc.TuskProtocol.toolchain_path
            );
            ( "packages",
              Json.Array
                (List.map
                   (fun (pkg : Server.Tusk_jsonrpc.TuskProtocol.package_info) ->
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
                   config.Server.Tusk_jsonrpc.TuskProtocol.packages) );
            ( "total_packages",
              Json.Int config.Server.Tusk_jsonrpc.TuskProtocol.total_packages );
          ]
        |> Json.to_string
      in
      Log.debug "[WORKSPACE TOOL] returning WorkspaceInfo";
      WorkspaceInfo { json }
  | Error msg ->
      Log.error "[WORKSPACE TOOL] RPC error: %s" msg;
      Error msg

let response_to_json = function
  | WorkspaceInfo { json } ->
      Json.Object
        [
          ( "content",
            Json.Array
              [
                Json.Object
                  [ ("type", Json.String "text"); ("text", Json.String json) ];
              ] );
          ("isError", Json.Bool false);
        ]
  | Error message ->
      Json.Object
        [
          ( "content",
            Json.Array
              [
                Json.Object
                  [
                    ("type", Json.String "text"); ("text", Json.String message);
                  ];
              ] );
          ("isError", Json.Bool true);
        ]
