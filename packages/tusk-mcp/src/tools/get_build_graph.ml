open Std
open Std.Data

let description =
  {|Get the build dependency graph showing relationships between all packages. Returns the complete dependency tree with build status for each package.

WHEN TO USE THIS TOOL:
- When you need to understand package dependencies
- When figuring out build order for packages
- When investigating circular dependencies
- When understanding which packages depend on a given package
- When planning changes that might affect multiple packages
- Before modifying a package to see what else depends on it

INSTEAD OF:
- DO NOT manually parse tusk.toml files to find dependencies
- DO NOT use grep to search for dependency declarations
- DO NOT try to infer dependencies from import statements
- DO NOT use filesystem tools to understand dependencies

PARAMETERS:
No parameters required - always returns complete dependency graph.

RETURNS:
Returns GraphInfo with JSON containing:
- nodes: Array of packages in the build graph, each containing:
  - package: Package name (e.g., "tusk", "std")
  - status: Current build status (pending, building, built, failed)
  - dependencies: List of package names this package depends on

This gives you the complete dependency tree, showing which packages must be built before others and the current build state of each package.

Or returns an Error if graph generation fails.

EXAMPLES:
- Get full dependency graph: {} (no parameters needed)

USE THIS to understand package relationships before making changes that might affect multiple packages. This shows you the exact dependency tree without needing to parse configuration files.|}

let tool =
  let open Mcp in
  let input_schema =
    Json.Object
      [ ("type", Json.String "object"); ("properties", Json.Object []) ]
  in
  { name = "getBuildGraph"; description = Some description; input_schema }

type request = unit
type response = GraphInfo of { json : string } | Error of string

let execute (client : Tusk_client.t) (_ : request) : response =
  match Tusk_client.get_build_graph client with
  | Ok response ->
      let json =
        Json.Object
          [
            ( "nodes",
              Json.Array
                (List.map
                   (fun node ->
                     Json.Object
                       [
                         ( "package",
                           Json.String
                             node.Tusk_protocol.WireProtocol.package_name );
                         ( "status",
                           Json.String node.Tusk_protocol.WireProtocol.status );
                         ( "dependencies",
                           Json.Array
                             (List.map
                                (fun d -> Json.String d)
                                node.Tusk_protocol.WireProtocol.deps) );
                       ])
                   response.Tusk_protocol.WireProtocol.nodes) );
          ]
        |> Json.to_string
      in
      GraphInfo { json }
  | Error msg -> Error msg

let response_to_json = function
  | GraphInfo { json } ->
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
